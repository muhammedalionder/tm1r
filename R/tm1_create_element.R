tm1_create_element <- function(tm1_connection,
                            dimension,
                            element,
                            parent = "",
                            weight = 1) {

  tm1_adminhost <- tm1_connection$adminhost
  tm1_httpport <- tm1_connection$port
  tm1_auth_key <- tm1_connection$key
  tm1_ssl <- tm1_connection$ssl

  # added because some http does not know space
  dimension <- gsub(" ", "%20", dimension, fixed=TRUE)

  ##########################################3
  ### if the element does not exist, add it to the dimension

  if (tm1_get_element(tm1_connection, dimension, element)$index == 0) {

  u1 <- ifelse(tm1_ssl==TRUE, "https://", "http://")
  u2 <- tm1_adminhost
  u3 <- ":"
  u4 <- tm1_httpport
  u5 <- "/api/v1/Dimensions('"
  u6 <- dimension
  u7 <- "')/Hierarchies('"
  u8 <- dimension
  u9 <- "')/Elements"


  # url development
  url <- paste0(u1, u2, u3, u4, u5, u6, u7, u8, u9)
  #url = "https://localhost:8881/api/v1/Dimensions('month')/Hierarchies('month')/Elements"

  elementbody1 <- "{    \"Name\": \""
  elementbody2 <- element
  elementbody3 <-" \" }"

  elementbody <- paste0(elementbody1, elementbody2, elementbody3)

  # post request
  tm1_process_return <-
    httr::POST(url,
         httr::add_headers("Authorization" = tm1_auth_key),
         httr::add_headers("Content-Type" = "application/json"),
         body = elementbody)

  # return manipulation

    # check return if error
    if (is.null(jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$error$message) == FALSE) {
      message(jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$error$message)
      stop()
    }

  }

  ##########################################
  ### now the element exist. connect it to the parent if it is not a component

  elementproper <- tm1_get_element(tm1_connection, dimension, element)$name
  components <- tm1_get_element(tm1_connection, dimension, parent)$components
  componentcheckresult <- subset(components, components == elementproper)


  if (parent != "" &&  length(componentcheckresult) == 0) {

    u1 <- ifelse(tm1_ssl==TRUE, "https://", "http://")
    u2 <- tm1_adminhost
    u3 <- ":"
    u4 <- tm1_httpport
    u5 <- "/api/v1/Dimensions('"
    u6 <- dimension
    u7 <- "')/Hierarchies('"
    u8 <- dimension
    u9 <- "')/Edges"


    # url development
    url <- paste0(u1, u2, u3, u4, u5, u6, u7, u8, u9)
    #url = "https://localhost:8881/api/v1/Dimensions('month')/Hierarchies('month')/Edges"

    elementbody1 <- "[ {    \"ParentName\": \" "
    elementbody2 <- parent
    elementbody3 <-" \", \"ComponentName\": \" "
    elementbody4 <- element
    elementbody5 <-" \", \"Weight\": "
    elementbody6 <- weight
    elementbody7 <- " } ]"

    elementbody <- paste0(elementbody1, elementbody2, elementbody3, elementbody4,
                          elementbody5, elementbody6, elementbody7)

    # post request
    tm1_process_return <-
      httr::POST(url,
                 httr::add_headers("Authorization" = tm1_auth_key),
                 httr::add_headers("Content-Type" = "application/json"),
                 body = elementbody)

    # return manipulation

    # check return if error
    if (is.null(jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$error$message) == FALSE) {
      message(jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$error$message)
      stop()
    }

  }




}
