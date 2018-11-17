tm1_delete_view <- function(tm1_connection,
                            cube, view) {

  tm1_adminhost <- tm1_connection$adminhost
  tm1_httpport <- tm1_connection$port
  tm1_auth_key <- tm1_connection$key
  tm1_ssl <- tm1_connection$ssl

  # added because some http does not know space
  cube <- gsub(" ", "%20", cube, fixed=TRUE)
  view <- gsub(" ", "%20", view, fixed=TRUE)

  u1 <- ifelse(tm1_ssl==TRUE, "https://", "http://")
  #u1 <- "https://"
  u2 <- tm1_adminhost
  u3 <- ":"
  u4 <- tm1_httpport
  u5 <- "/api/v1/Cubes('"
  u6 <- cube
  u7 <- "')/Views('"
  u8 <- view
  u9 <- "')"

  # url development
  url <- paste0(u1, u2, u3, u4, u5, u6, u7, u8, u9)
  #url = "https://localhost:8881/api/v1/Cubes('SalesCube')/Views('TempViewByMDX')"


  # delete request
  tm1_process_return <-
    httr::DELETE(url,
         httr::add_headers("Authorization" = tm1_auth_key),
         httr::add_headers("Content-Type" = "application/json"))

  # return manipulation
  # if content is empty; then success
  # else get the error message to differentiate abortion and minor error
  if(httr::content(tm1_process_return, "text", encoding = "UTF-8") == "")
  {
    tm1_process_message <- "ViewDeletedSuccessfully"
    print(tm1_process_message)

  }
  else
  {
    # check return if error
    if (is.null(jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$error$message) == FALSE) {
      message(jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$error$message)
      stop()
    }

  }




}
