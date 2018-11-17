tm1_api_request <- function(tm1_connection,
                            url, body ="", type = "GET") {

  tm1_adminhost <- tm1_connection$adminhost
  tm1_httpport <- tm1_connection$port
  tm1_auth_key <- tm1_connection$key
  tm1_ssl <- tm1_connection$ssl

  #type <- paste0("httr::", type)

  # request

    tm1_process_return <-
      do.call(get(type, asNamespace("httr")),
              list(url,
                   httr::add_headers("Authorization" = tm1_auth_key),
                   httr::add_headers("Content-Type" = "application/json"),
                   body = body))


  tm1_return <- jsonlite::fromJSON(httr::content(tm1_process_return, "text"))

  return(tm1_return)





}
