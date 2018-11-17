tm1_get_instances <- function(adminhost = "localhost", port = "5898", ssl = TRUE) {

  u1 <- ifelse(ssl==TRUE, "https://", "http://")
  u2 <- adminhost
  u3 <- ":"
  u4 <- port
  u5 <- "/api/v1/Servers"

  # url development
  url <- paste0(u1, u2, u3, u4, u5)
  #url = "https://localhost:5898/api/v1/Servers"

  httr::set_config(httr::config(ssl_verifypeer = FALSE))
  httr::set_config(httr::config(ssl_verifyhost = FALSE))

  # post request
  tm1_return <-
    httr::GET(url)

  # make it proper
  tm1_instances <- jsonlite::fromJSON(httr::content(tm1_return, "text"))$value

    #return
  return(tm1_instances)



}
