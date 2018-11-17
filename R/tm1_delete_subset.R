tm1_delete_subset <- function(tm1_connection,
                            dimension,
                            subset) {

  tm1_adminhost <- tm1_connection$adminhost
  tm1_httpport <- tm1_connection$port
  tm1_auth_key <- tm1_connection$key
  tm1_ssl <- tm1_connection$ssl

  # added because some http does not know space
  dimension <- gsub(" ", "%20", dimension, fixed=TRUE)
  subset <- gsub(" ", "%20", subset, fixed=TRUE)


  u1 <- ifelse(tm1_ssl==TRUE, "https://", "http://")
  u2 <- tm1_adminhost
  u3 <- ":"
  u4 <- tm1_httpport
  u5 <- "/api/v1/Dimensions('"
  u6 <- dimension
  u7 <- "')/Hierarchies('"
  u8 <- dimension
  u9 <- "')/Subsets('"
  u10 <- subset
  u11 <- "')"


  # url development
  url <- paste0(u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11)
  #url = "https://localhost:8881/api/v1/Dimensions('month')/Hierarchies('month')/Subsets('test')"

  # delete request
  tm1_process_return <-
    httr::DELETE(url,
         httr::add_headers("Authorization" = tm1_auth_key),
         httr::add_headers("Content-Type" = "application/json"))

  # return manipulation

  if(httr::content(tm1_process_return, "text", encoding = "UTF-8") == "")
    {

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
