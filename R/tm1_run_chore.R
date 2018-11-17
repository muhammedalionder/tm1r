tm1_run_chore <- function(tm1_connection, chore = "") {

  tm1_adminhost <- tm1_connection$adminhost
  tm1_httpport <- tm1_connection$port
  tm1_auth_key <- tm1_connection$key
  tm1_ssl <- tm1_connection$ssl

  # added because some http does not know space
  chore <- gsub(" ", "%20", chore, fixed=TRUE)

  u1 <- ifelse(tm1_ssl==TRUE, "https://", "http://")
  #u1 <- "https://"
  u2 <- tm1_adminhost
  u3 <- ":"
  u4 <- tm1_httpport
  u5 <- "/api/v1/Chores('"
  u6 <- chore
  u7 <- "')/tm1.Execute"

  # url development
  url <- paste0(u1, u2, u3, u4, u5, u6, u7)
  #url = "https://localhost:8881/api/v1/Chores('create_Y2Ksales_cube')/tm1.Execute"

  # post request
  tm1_chore_return <-
    httr::POST(url,
               httr::add_headers("Authorization" = tm1_auth_key),
               httr::add_headers("Content-Type" = "application/json"))



  # return manipulation
  # if content is empty; then success
  # else get the error message to differentiate abortion and minor error
  if(httr::content(tm1_chore_return, "text", encoding = "UTF-8") == "")
  {
    tm1_chore_message <- "ChoreCompletedSuccessfully"
    print(tm1_chore_message)
    }
  else
  {
    # check return if error
    if (is.null(jsonlite::fromJSON(httr::content(tm1_chore_return, "text"))$error$message) == FALSE) {
      message(jsonlite::fromJSON(httr::content(tm1_chore_return, "text"))$error$message)
      stop()
    }

  }




}
