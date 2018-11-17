tm1_get_dimension_elements <- function(tm1_connection, dimension) {

  tm1_adminhost <- tm1_connection$adminhost
  tm1_httpport <- tm1_connection$port
  tm1_auth_key <- tm1_connection$key
  tm1_ssl <- tm1_connection$ssl

  # added because some http does not know space
  dimension <- gsub(" ", "%20", dimension, fixed=TRUE)

  u1 <- ifelse(tm1_ssl==TRUE, "https://", "http://")
  #u1 <- "https://"
  u2 <- tm1_adminhost
  u3 <- ":"
  u4 <- tm1_httpport
  u5 <- "/api/v1/Dimensions('"
  u6 <- dimension
  u7 <- "')/Hierarchies('"
  u8 <- dimension
  u9 <- "')/Members?$select=Name&$expand=Element($select=Type)&$format=application/json;odata.metadata=none"

  # url development
  url <- paste0(u1, u2, u3, u4, u5, u6, u7, u8, u9)
  #url = "https://localhost:8881/api/v1/Dimensions('Account1')/Hierarchies('Account1')/Members?
  #$select=Name&$expand=Element($select=Type)&$format=application/json;odata.metadata=none"

  # post request
  tm1_process_return <-
    httr::GET(url,
              httr::add_headers("Authorization" = tm1_auth_key))

  # check return if error
  if (is.null(jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$error$message) == FALSE) {
    message(jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$error$message)
    stop()
  }

  # make it proper
  tm1_dim_els <- jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$value

  #change to data frame
  tm1_dim_els <- as.data.frame(tm1_dim_els)
  colnames(tm1_dim_els)[2] <- "Type"
  #return
  return(tm1_dim_els)



}
