tm1_get_log <- function(tm1_connection, lognumber=5) {

  tm1_adminhost <- tm1_connection$adminhost
  tm1_httpport <- tm1_connection$port
  tm1_auth_key <- tm1_connection$key
  tm1_ssl <- tm1_connection$ssl

  u1 <- ifelse(tm1_ssl==TRUE, "https://", "http://")
  #u1 <- "https://"
  u2 <- tm1_adminhost
  u3 <- ":"
  u4 <- tm1_httpport
  u5 <- "/api/v1/MessageLogEntries?$orderby=ID%20desc&$top="
  u6 <- toString(lognumber)
  u7 <- "&$select=Level,TimeStamp,Message"

  # url development
  url <- paste0(u1, u2, u3, u4, u5, u6, u7)
  #url = "https://localhost:8881/api/v1/MessageLogEntries?$orderby=ID desc&$top=5&$select=Level,TimeStamp,Message"


    # post request
  tm1_log_return <-
    httr::GET(url,
              httr::add_headers("Authorization" = tm1_auth_key))

  # make it proper
  tm1_log <- jsonlite::fromJSON(httr::content(tm1_log_return, "text"))$value

  #change to data frame
  tm1_log <- as.data.frame(as.matrix(tm1_log))

  #delete 1st column
  tm1_log <- tm1_log[,-c(1) ]

  #return
  return(tm1_log)




}
