tm1_get_dimensions <- function(tm1_connection, ShowControlObjects = FALSE) {

  tm1_adminhost <- tm1_connection$adminhost
  tm1_httpport <- tm1_connection$port
  tm1_auth_key <- tm1_connection$key
  tm1_ssl <- tm1_connection$ssl



  u1 <- ifelse(tm1_ssl==TRUE, "https://", "http://")
  #u1 <- "https://"
  u2 <- tm1_adminhost
  u3 <- ":"
  u4 <- tm1_httpport
  u5 <- "/api/v1/Dimensions"

  if (ShowControlObjects == FALSE) {
    u6 <- "?$filter=not%20startswith(Name,'}')"
  } else{
    u6 <- ""
  }

  # url development
  url <- paste0(u1, u2, u3, u4, u5, u6)
  #url = "https://localhost:8881/api/v1/Dimensions"

  # post request
  tm1_process_return <-
    httr::GET(url,
              httr::add_headers("Authorization" = tm1_auth_key))


  # check return if error
  if (is.null(jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$error$message) == FALSE) {
    message(jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$error$message)
  }
  else
  {
    tm1_return <- jsonlite::fromJSON(httr::content(tm1_process_return, "text"))
    return(tm1_return$value$Name)

  }







}
