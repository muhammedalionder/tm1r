tm1_send_dataset <- function(tm1_connection,
                          valueset, cube, rowdim, coldim,
                          titledim1 = "", titleel1 = "", titledim2 = "", titleel2 = "",
                          titledim3 = "", titleel3 = "", titledim4 = "", titleel4 = "",
                          titledim5 = "", titleel5 = "", titledim6 = "", titleel6 = "",
                          titledim7 = "", titleel7 = "", titledim8 = "", titleel8 = ""
                          ) {

  tm1_adminhost <- tm1_connection$adminhost
  tm1_httpport <- tm1_connection$port
  tm1_auth_key <- tm1_connection$key
  tm1_ssl <- tm1_connection$ssl

  ###########################################
  ###########mdx generate
  mdx <- tm1_create_mdx(cube,
                   rowdim1 = rowdim, rowel1 = paste(rownames(valueset), collapse = '|'),
                   coldim1 = coldim, colel1 = paste(colnames(valueset), collapse = '|'),
                   titledim1 = titledim1, titleel1 = titleel1,
                   titledim2 = titledim2, titleel2 = titleel2,
                   titledim3 = titledim3, titleel3 = titleel3,
                   titledim4 = titledim4, titleel4 = titleel4,
                   titledim5 = titledim5, titleel5 = titleel5,
                   titledim6 = titledim6, titleel6 = titleel6,
                   titledim7 = titledim7, titleel7 = titleel7,
                   titledim8 = titledim8, titleel8 = titleel8,
                   rowsuppress = FALSE, colsuppress = FALSE)

  ###########################################
  ########### construct mdx to get id

  # url development
  u1 <- ifelse(tm1_ssl==TRUE, "https://", "http://")
  u2 <- tm1_adminhost
  u3 <- ":"
  u4 <- tm1_httpport
  u5 <- "/api/v1/ExecuteMDX"
  #u6 <- "?$expand=Axes($expand=Tuples($expand=Members($select=Name,UniqueName))),Cells($select=Value)"
  u6 <- ""

  url <- paste0(u1, u2, u3, u4, u5, u6)
  #url = "https://localhost:8881/api/v1/ExecuteMDX?
  #$expand=Axes($expand=Tuples($expand=Members($select=Name))),Cells($select=Value)"

  # change mdx to body text
  bodytext <- paste0("{	\"MDX\": \" ", mdx, "\"}")

  # post request
  tm1_process_return <-
    httr::POST(url,
               httr::add_headers("Authorization" = tm1_auth_key),
               httr::add_headers("Content-Type" = "application/json"),
               body = bodytext)

  # check return if error
  if (is.null(jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$error$message) == FALSE) {
    message(jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$error$message)
    stop()
  }

  # Get the CellSetID
  tm1_return <- jsonlite::fromJSON(httr::content(tm1_process_return, "text"))
  CellSetID <- tm1_return$ID

  ###########################################
  ########### SEnd the values
  # url development
  u1 <- ifelse(tm1_ssl==TRUE, "https://", "http://")
  u2 <- tm1_adminhost
  u3 <- ":"
  u4 <- tm1_httpport
  u5 <- "/api/v1/Cellsets('"
  u6 <- CellSetID
  u7 <- "')/Cells"

  url <- paste0(u1, u2, u3, u4, u5, u6, u7)

  # change body to values
  nrown <- nrow(valueset)
  ncoln <- ncol(valueset)
  cellnumber <- nrown * ncoln
  ordinalvalues <- character(cellnumber)
  ordinalnumber <- 0

  for (i in 1:nrown) {

    for (j in 1:ncoln) {

      ordinalvalues[ordinalnumber+1] <- paste0("{ \"Ordinal\": ", ordinalnumber, ", \"Value\": ", valueset[i,j], " }")
      ordinalnumber <- ordinalnumber + 1
    }

  }

  bodytext <- ""
  bodytext <- paste0(bodytext, "[")
  ordinalvaluesstr <- paste(ordinalvalues, collapse = ',')
  bodytext <- paste0(bodytext, ordinalvaluesstr)
  bodytext <- paste0(bodytext, "]")

  # patch request
  tm1_process_return <-
    httr::PATCH(url,
               httr::add_headers("Authorization" = tm1_auth_key),
               httr::add_headers("Content-Type" = "application/json"),
               body = bodytext)

  # check return if error
  if(httr::content(tm1_process_return, "text", encoding = "UTF-8") == "")
  {
    # Do nothing.
  }
  else
  {
    if (is.null(jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$error$message) == FALSE) {
      message(jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$error$message)
      stop()
    }
  }

  ###########################################
  ###########Delete the CellSet
  # url development
  u1 <- ifelse(tm1_ssl==TRUE, "https://", "http://")
  u2 <- tm1_adminhost
  u3 <- ":"
  u4 <- tm1_httpport
  u5 <- "/api/v1/Cellsets('"
  u6 <- CellSetID
  u7 <- "')"

  url <- paste0(u1, u2, u3, u4, u5, u6, u7)

  #Delete request
  tm1_process_return <-
    httr::DELETE(url,
               httr::add_headers("Authorization" = tm1_auth_key),
               httr::add_headers("Content-Type" = "application/json"))

  # check return if error
  if(httr::content(tm1_process_return, "text", encoding = "UTF-8") == "")
  {
    # Do nothing.
  }
  else
  {
    if (is.null(jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$error$message) == FALSE) {
      message(jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$error$message)
      stop()
    }
  }

}
