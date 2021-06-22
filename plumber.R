library(plumber)
library(magrittr)

options(plumber.port = 8000)

db_path <- Sys.getenv("DB_PATH")

key <- charToRaw(readr::read_rds(file.path(db_path, "key.rds")))

#' Authenticate incoming requests
#* @filter authenticate
function(req, res) {
  if (req$REQUEST_METHOD == "GET") return(plumber::forward())

  if (!hasName(req, "HTTP_AUTHORIZATION")) {
    res$status <- 401

    return(list(
      error = "auth_header_missing",
      error_msg = "Authorization header required!"
    ))
  }

  matches <- stringr::str_match(req$HTTP_AUTHORIZATION, "^Bearer\\s(.*)$")
  if (is.na(matches)) {
    res$status <- 401

    return(list(
      error = "auth_header_wrong_format",
      error_msg = "Authorization header must be of form 'Bearer <JWT>'!"
    ))
  }
  jwt <- matches[1,2]

  error <- FALSE
  tryCatch({
    req$jwt <- jose::jwt_decode_hmac(jwt, key)
  }, error = function(e) {
    error <<- TRUE
    res$status <- 401
  })
  if (error) return(list(
    error = "invalid_token",
    error_msg = "Invalid token!"
  ))

  plumber::forward()
}

#* @filter cors
cors <- function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  plumber::forward()
}

#* Add location of user at a specific timestamp. The user is inferred from the authorization token.
#* @param lat Latitude
#* @param lon Longitude
#* @param ts Timestamp
#* @post /add_location_ts
function(req, lat, lon, ts) {
  user_id <- req$jwt$user_id

  entry <- tibble::tibble(user_id = user_id, lat = lat, lon = lon, time = ts)

  db <- DBI::dbConnect(RSQLite::SQLite(), file.path(db_path, "db.sqlite"))

  on.exit(DBI::dbDisconnect(db))

  DBI::dbAppendTable(db, "location_ts", entry)
}

#* Get location_ts table
#* @get /location_ts
function() {
  db <- DBI::dbConnect(RSQLite::SQLite(), file.path(db_path, "db.sqlite"))

  on.exit(DBI::dbDisconnect(db))

  DBI::dbReadTable(db, "location_ts")
}

#* Echo the parameter that was sent in
#* @param msg The message to echo back.
#* @get /echo
function(msg=""){
  list(msg = paste0("The message is: '", msg, "'"))
}

#* Plot out data from the iris dataset
#* @param spec If provided, filter the data to only this species (e.g. 'setosa')
#* @get /plot
#* @serializer png
function(spec){
  myData <- iris
  title <- "All Species"

  # Filter if the species was specified
  if (!missing(spec)){
    title <- paste0("Only the '", spec, "' Species")
    myData <- subset(iris, Species == spec)
  }

  plot(myData$Sepal.Length, myData$Petal.Length,
       main=title, xlab="Sepal Length", ylab="Petal Length")
}
