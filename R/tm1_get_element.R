tm1_get_element <- function(tm1_connection, dimension, element = "", index = 0) {

  tm1_adminhost <- tm1_connection$adminhost
  tm1_httpport <- tm1_connection$port
  tm1_auth_key <- tm1_connection$key
  tm1_ssl <- tm1_connection$ssl

  # added because some http does not know space
  dimension <- gsub(" ", "%20", dimension, fixed=TRUE)
  element <- gsub(" ", "%20", element, fixed=TRUE)

  u1 <- ifelse(tm1_ssl==TRUE, "https://", "http://")
  #u1 <- "https://"
  u2 <- tm1_adminhost
  u3 <- ":"
  u4 <- tm1_httpport
  u5 <- "/api/v1/Dimensions('"
  u6 <- dimension
  u7 <- "')/Hierarchies('"
  u8 <- dimension
  u9 <- "')/Elements"

  if (element != "") {
    u10 <- "('"
    u11 <- element
    u12 <- "')?$expand=Components"
  }
  else
  {
    if (index > 0)
  {
    u10 <- "?$filter=Index%20eq%20"
    u11 <- index
    u12 <- ""
    }
  else
  {
    #error
    message("element or index parameter should be specified AND index should be greater than 0")
    stop()

  }
  }

  # url development
  url <- paste0(u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12)
  #url = "https://localhost:8881/api/v1/Dimensions('month')/Hierarchies('month')/Elements('Year')?$expand=Components"

  # post request
  tm1_process_return <-
    httr::GET(url,
              httr::add_headers("Authorization" = tm1_auth_key))

  tm1_element_object <- list(name=c(NULL), uniquename=c(NULL), type=c(NULL),
                             level=c(NULL), index=c(0), components=c(NULL))

  # check return if error
  if (is.null(jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$error$message) == FALSE) {
    return(tm1_element_object)
  }
  else
  {
    if (element != "") {
      content <- jsonlite::fromJSON(httr::content(tm1_process_return, "text"))
    }
    else
    {
      content <- jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$value
    }

    tm1_element_object$name <- content$Name
    tm1_element_object$uniquename <- content$UniqueName
    tm1_element_object$type <- content$Type
    tm1_element_object$level <- content$Level
    tm1_element_object$index <- content$Index
    tm1_element_object$components <- content$Components$Name

    return(tm1_element_object)

  }


  #return




}
