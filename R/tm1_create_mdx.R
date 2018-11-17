tm1_create_mdx <- function(cube,
                             rowdim1="", rowsub1="", rowel1="",
                             rowdim2="", rowsub2="", rowel2="",
                             rowdim3="", rowsub3="", rowel3="",
                             coldim1="", colsub1="", colel1="",
                             coldim2="", colsub2="", colel2="",
                             titledim1="", titleel1="",
                             titledim2="", titleel2="",
                             titledim3="", titleel3="",
                             titledim4="", titleel4="",
                             titledim5="", titleel5="",
                             titledim6="", titleel6="",
                             titledim7="", titleel7="",
                             titledim8="", titleel8="",
                             titledim9="", titleel9="",
                             titledim10="", titleel10="",
                             rowsuppress=TRUE, colsuppress = TRUE
                             ) {

# ROW DIMSTRINGS GENERATION
  rowdimstrings <- character(0)
if (rowdim1 != "") {

    if (rowel1 != "")
    {
      rowdimstrings[1] <- paste0("{",
                           paste0(
                              paste0("[", rowdim1, "].[", rowdim1, "].[", strsplit(rowel1, "|", TRUE)[[1]], "]"),
                                    collapse = ", "),
                           "}")
    }
  else {
    rowdimstrings[1] <- paste0("{TM1SubsetToSet([",rowdim1, "].[",rowdim1, "], \\\"",rowsub1, "\\\")}")
       }
}

  # ROW DIM 2 STR GENERATION
  if (rowdim2 != "") {

    if (rowel2 != "")
    {
      rowdimstrings[2] <- paste0("{",
                                 paste0(
                                   paste0("[", rowdim2, "].[", rowdim2, "].[", strsplit(rowel2, "|", TRUE)[[1]], "]"),
                                   collapse = ", "),
                                 "}")
    }
    else {
      rowdimstrings[2] <- paste0("{TM1SubsetToSet([",rowdim2, "].[",rowdim2, "], \\\"",rowsub2, "\\\")}")
    }
  }


  # ROW DIM 3 STR GENERATION
  if (rowdim3 != "") {

    if (rowel3 != "")
    {
      rowdimstrings[3] <- paste0("{",
                                 paste0(
                                   paste0("[", rowdim3, "].[", rowdim3, "].[", strsplit(rowel3, "|", TRUE)[[1]], "]"),
                                   collapse = ", "),
                                 "}")
    }
    else {
      rowdimstrings[3] <- paste0("{TM1SubsetToSet([",rowdim3, "].[",rowdim3, "], \\\"",rowsub3, "\\\")}")
    }
  }



  # col DIMSTRINGS GENERATION
  coldimstrings <- character(0)

  if (coldim1 != "") {

    if (colel1 != "")
    {
      coldimstrings[1] <- paste0("{",
                                 paste0(
                                   paste0("[", coldim1, "].[", coldim1, "].[", strsplit(colel1, "|", TRUE)[[1]], "]"),
                                   collapse = ", "),
                                 "}")
    }
    else {
      coldimstrings[1] <- paste0("{TM1SubsetToSet([",coldim1, "].[",coldim1, "], \\\"",colsub1, "\\\")}")
    }
  }

  # col DIM 2 STR GENERATION
  if (coldim2 != "") {

    if (colel2 != "")
    {
      coldimstrings[2] <- paste0("{",
                                 paste0(
                                   paste0("[", coldim2, "].[", coldim2, "].[", strsplit(colel2, "|", TRUE)[[1]], "]"),
                                   collapse = ", "),
                                 "}")
    }
    else {
      coldimstrings[2] <- paste0("{TM1SubsetToSet([",coldim2, "].[",coldim2, "], \\\"",colsub2, "\\\")}")
    }
  }

  # put title dims and elements into an array
  titledims <- character(0)
  titleels <- character(0)
  titlestrings <- character(0)

  for (i in 1:10) {

    titledim <- eval(parse(text = paste0("titledim",i)))

    if (titledim != "") {
      titledims[i] <- eval(parse(text = paste0("titledim",i)))
      titleels[i] <- eval(parse(text = paste0("titleel",i)))
      titlestrings[i] <- paste0("[",titledims[i] , "].[",titledims[i], "].[", titleels[i], "]")
    }

  }


mdx <- "SELECT "

mdx <- paste0( mdx, ifelse(colsuppress==TRUE, "NON EMPTY ", ""))
mdx <- paste0( mdx, paste0(coldimstrings, collapse = "*"))
mdx <- paste0( mdx, " ON COLUMNS")

if (length(rowdimstrings) > 0) {

  mdx <- paste0( mdx, ifelse(rowsuppress==TRUE, ", NON EMPTY ", ", "))
  mdx <- paste0( mdx, paste0(rowdimstrings, collapse = "*"))
  mdx <- paste0( mdx, " ON ROWS ")

}

mdx <- paste0( mdx, " FROM [", cube, "] ")

if ( length(titledims) > 0) {

  condition <- paste0( " WHERE(", paste0(titlestrings, collapse = ", "), ")")
  mdx <- paste0( mdx, condition)
}


return(mdx)

}
