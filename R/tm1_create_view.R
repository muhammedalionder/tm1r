tm1_create_view <- function(tm1_connection,
                            cube, view, mdx) {

  tm1_adminhost <- tm1_connection$adminhost
  tm1_httpport <- tm1_connection$port
  tm1_auth_key <- tm1_connection$key
  tm1_ssl <- tm1_connection$ssl

  # added because some http does not know space
  cube <- gsub(" ", "%20", cube, fixed=TRUE)

  u1 <- ifelse(tm1_ssl==TRUE, "https://", "http://")
  #u1 <- "https://"
  u2 <- tm1_adminhost
  u3 <- ":"
  u4 <- tm1_httpport
  u5 <- "/api/v1/Cubes('"
  u6 <- cube
  u7 <- "')/Views"

  # url development
  url <- paste0(u1, u2, u3, u4, u5, u6, u7)
  #url = "https://localhost:8881/api/v1/Cubes('SalesCube')/Views"

  # sample parameter syntax
  parameters <-
  "{
    \"Parameters\":
    [
     {\"Name\":\"par1name\", \"Value\":\"par1value\"}
    ,{\"Name\":\"par2name\", \"Value\":par2value}
    ,{\"Name\":\"par3name\", \"Value\":par3value}
    ]
  }"

  # parameter string development

  p1 <- "{    \"@odata.type\": \"ibm.tm1.api.v1.MDXView\", \"Name\":\""
  p2 <- view
  p3 <- "\", \"MDX\":\""
  p4 <- mdx
  p5 <- "\"}"


  parameters <- paste0(p1, p2, p3, p4, p5)

  # post request
  tm1_process_return <-
    httr::POST(url,
         httr::add_headers("Authorization" = tm1_auth_key),
         httr::add_headers("Content-Type" = "application/json"),
         body = parameters)

  # return manipulation
  # if content is empty; then success
  # else get the error message to differentiate abortion and minor error
  if(httr::content(tm1_process_return, "text", encoding = "UTF-8") == "")
  {
    tm1_process_message <- "ViewCreatedSuccessfully"
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
