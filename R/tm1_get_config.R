tm1_get_config <- function(tm1_connection) {

  tm1_adminhost <- tm1_connection$adminhost
  tm1_httpport <- tm1_connection$port
  tm1_auth_key <- tm1_connection$key
  tm1_ssl <- tm1_connection$ssl

  u1 <- ifelse(tm1_ssl==TRUE, "https://", "http://")
  #u1 <- "https://"
  u2 <- tm1_adminhost
  u3 <- ":"
  u4 <- tm1_httpport
  u5 <- "/api/v1/Configuration"

  # url development
  url <- paste0(u1, u2, u3, u4, u5)
  #url = "https://localhost:8881/api/v1/Configuration"

  # post request
  tm1_process_return <-
    httr::GET(url,
              httr::add_headers("Authorization" = tm1_auth_key))

  # make it proper
  tm1_config <- jsonlite::fromJSON(httr::content(tm1_process_return, "text"))

  #change to data frame
  tm1_config <- as.data.frame(as.matrix(tm1_config))

  # change column name from random
  colnames(tm1_config)[1] <- "Value"

  #delete 1st row
  tm1_config <- tm1_config[-c(1), ]

  #change to proper data frame again
  tm1_config <- as.data.frame(as.matrix(tm1_config))
  colnames(tm1_config)[1] <- "Value"

  #return
  return(tm1_config)



}
