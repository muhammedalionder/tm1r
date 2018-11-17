\name{tm1_create_mdx}
\alias{tm1_create_mdx}
\title{TM1 Generate mdx for a cube view}
\usage{
tm1_create_mdx(cube,
                             rowdim1, rowsub1, rowel1,
                             rowdim2, rowsub2, rowel2,
                             rowdim3, rowsub3, rowel3,
                             coldim1, colsub1, colel1,
                             coldim2, colsub2, colel2,
                             titledim1, titleel1,
                             titledim2, titleel2,
                             titledim3, titleel3,
                             titledim4, titleel4,
                             titledim5, titleel5,
                             titledim6, titleel6,
                             titledim7, titleel7,
                             titledim8, titleel8,
                             titledim9, titleel9,
                             titledim10, titleel10,
                             rowsuppress, colsuppress )
}
\arguments{

\item{cube}{Name of the cube}
\item{rowdim1}{Name of dimension in 1st row}
\item{rowsub1}{Subset of dimension in 1st row}
\item{rowel1}{Element of dimension in 1st row. If multiple, seperated by "|". This should be passed if subset is not provided}
\item{rowdim2}{Name of dimension in 2nd row}
\item{rowsub2}{Subset of dimension in 2nd row}
\item{rowel2}{Element of dimension in 2nd row. If multiple, seperated by "|". This should be passed if subset is not provided}
\item{rowdim3}{Name of dimension in 3rd row}
\item{rowsub3}{Subset of dimension in 3rd row}
\item{rowel3}{Element of dimension in 3rd row. If multiple, seperated by "|". This should be passed if subset is not provided}
\item{coldim1}{Name of dimension in 1st col}
\item{colsub1}{Subset of dimension in 1st col}
\item{colel1}{Element of dimension in 1st col. If multiple, seperated by "|". This should be passed if subset is not provided}
\item{coldim2}{Name of dimension in 2nd col}
\item{colsub2}{Subset of dimension in 2nd col}
\item{colel2}{Element of dimension in 2nd col. If multiple, seperated by "|". This should be passed if subset is not provided}

\item{titledim1}{Name of dimension in title}
\item{titleel1}{Element of dimension in corresponding titledim}
\item{titledim2}{Name of dimension in title}
\item{titleel2}{Element of dimension in corresponding titledim}
\item{titledim3}{Name of dimension in title}
\item{titleel3}{Element of dimension in corresponding titledim}
\item{titledim4}{Name of dimension in title}
\item{titleel4}{Element of dimension in corresponding titledim}
\item{titledim5}{Name of dimension in title}
\item{titleel5}{Element of dimension in corresponding titledim}
\item{titledim6}{Name of dimension in title}
\item{titleel6}{Element of dimension in corresponding titledim}
\item{titledim7}{Name of dimension in title}
\item{titleel7}{Element of dimension in corresponding titledim}
\item{titledim8}{Name of dimension in title}
\item{titleel8}{Element of dimension in corresponding titledim}
\item{titledim9}{Name of dimension in title}
\item{titleel9}{Element of dimension in corresponding titledim}
\item{titledim10}{Name of dimension in title}
\item{titleel10}{Element of dimension in corresponding titledim}


\item{rowsuppress}{TRUE if zeroes are suppressed on rows}
\item{colsuppress}{TRUE if zeroes are suppressed on columns}

}
\description{
Returns mdx as a string to use in the function tm1_get_mdx_view
}
\examples{
\dontrun{

tm1_create_mdx( "SalesCube", rowdim1="account1", rowel1 = "Sales", coldim1="month", colel1="Jan",
titledim1 = "actvsbud", titleel1 = "Actual",
titledim2 = "region", titleel2 = "Argentina",
titledim3 = "model", titleel3 = "S Series 1.8 L Sedan",
rowsuppress=TRUE, colsuppress = FALSE)
}

}
