tm1_get_cube_dimensions <- function(tm1_connection, cube) {

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
  u7 <- "')/Dimensions"


  # url development
  url <- paste0(u1, u2, u3, u4, u5, u6, u7)
  #url = "https://localhost:8881/api/v1/Cubes('SalesCube')/Dimensions"

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
  tm1_cube_dims <- jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$value

  #change to data frame
  tm1_cube_dims <- as.data.frame(tm1_cube_dims)

  #delete 1st column
  tm1_cube_dims <- tm1_cube_dims[,c(2) ]

  #return
  return(tm1_cube_dims)



}
