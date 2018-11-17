tm1_create_subset <- function(tm1_connection,
                            dimension,
                            subset,
                            element = "",
                            mdx = "",
                            overwrite = TRUE) {

  tm1_adminhost <- tm1_connection$adminhost
  tm1_httpport <- tm1_connection$port
  tm1_auth_key <- tm1_connection$key
  tm1_ssl <- tm1_connection$ssl

  # Check if subset exist
  subsetname <- subset
  subsetreturn <- tm1_get_dimension_subsets(tm1_connection, dimension)
  subsets <- as.vector(subsetreturn$Name)
  subsetexist <- ifelse(length(subset(subsets, subsets == subsetname)) == 0, FALSE, TRUE)

  # if subset exists and if overwrite is false, stop
  if (subsetexist == TRUE && overwrite == FALSE) {
    message("subset already exists")
    stop()
  }

  # added because some http does not know space
  dimension <- gsub(" ", "%20", dimension, fixed=TRUE)

  ##########################################3
  ### if the subset does not exist, create it with posts

  if (subsetexist == FALSE){

  u1 <- ifelse(tm1_ssl==TRUE, "https://", "http://")
  u2 <- tm1_adminhost
  u3 <- ":"
  u4 <- tm1_httpport
  u5 <- "/api/v1/Dimensions('"
  u6 <- dimension
  u7 <- "')/Hierarchies('"
  u8 <- dimension
  u9 <- "')/Subsets"


  # url development
  url <- paste0(u1, u2, u3, u4, u5, u6, u7, u8, u9)
  #url = "https://localhost:8881/api/v1/Dimensions('month')/Hierarchies('month')/Subsets"

  if (mdx != "") {
    elementbody1 <- "{    \"Name\": \""
    elementbody2 <- subset
    elementbody3 <-" \", \"Expression\": \" "
    elementbody4 <- mdx
    elementbody5 <-" \" }"

    elementbody <- paste0(elementbody1, elementbody2, elementbody3, elementbody4, elementbody5)
  } else if (element != "") {

    elementbody1 <- "{    \"Name\": \""
    elementbody2 <- subset
    elementbody3 <- " \", \"Hierarchy@odata.bind\": \" "
    elementbody4 <- paste0("Dimensions('", dimension, "')/Hierarchies('", dimension, "')")
    elementbody5 <- " \", \"Elements@odata.bind\": [ "
    elementbody6 <- paste0("",
                                paste0(
                                  paste0("\"Dimensions('", dimension, "')/Hierarchies('", dimension, "')/Elements('", strsplit(element, "|", TRUE)[[1]], "')\""),
                                  collapse = ", "),
                                "")
    elementbody7 <- " ] }"

    elementbody <- paste0(elementbody1, elementbody2, elementbody3, elementbody4,
                          elementbody5, elementbody6, elementbody7)

  } else{

    message("element or mdx should be specified")
    stop()

  }



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


  } else {

    ##########################################3
    ### if the subset exists, update it with patch

    # added because some http does not know space
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

    elementbody <- "{ \"Expression\":\" \"}"

    # first update with an empty expression
    tm1_process_return <-
      httr::PATCH(url,
                 httr::add_headers("Authorization" = tm1_auth_key),
                 httr::add_headers("Content-Type" = "application/json"),
                 body = elementbody)


    if (mdx != "") {

      elementbody1 <-"{ \"Expression\": \" "
      elementbody2 <- mdx
      elementbody3 <-" \" }"

      elementbody <- paste0(elementbody1, elementbody2, elementbody3)

    } else if (element != "") {

      elementbody1 <- "{ \"Hierarchy@odata.bind\": \" "
      elementbody2 <- paste0("Dimensions('", dimension, "')/Hierarchies('", dimension, "')")
      elementbody3 <- " \", \"Elements@odata.bind\": [ "
      elementbody4 <- paste0("",
                             paste0(
                               paste0("\"Dimensions('", dimension, "')/Hierarchies('", dimension, "')/Elements('", strsplit(element, "|", TRUE)[[1]], "')\""),
                               collapse = ", "),
                             "")
      elementbody5 <- " ] }"

      elementbody <- paste0(elementbody1, elementbody2, elementbody3, elementbody4, elementbody5)

    } else{

      message("element or mdx should be specified")
      stop()

    }

    # after updating with an empty expression, patch with elements or mdx
    tm1_process_return <-
      httr::PATCH(url,
                  httr::add_headers("Authorization" = tm1_auth_key),
                  httr::add_headers("Content-Type" = "application/json"),
                  body = elementbody)

    # check return if error
    if (is.null(jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$error$message) == FALSE) {
      message(jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$error$message)
      stop()
    }

  }




}
