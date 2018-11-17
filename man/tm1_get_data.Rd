\name{tm1_get_data}
\alias{tm1_get_data}
\title{TM1 Get Data from a Cube}
\usage{
tm1_get_data(tm1_connection, cube,
            element1="", element2="",
            element3="", element4="",
            element5="",element6="",
            element7="", element8="",
            element9="", element10="")
}
\arguments{

\item{tm1_connection}{tm1 connection object returned by the function tm1_connection}
\item{cube}{Name of a cube as a string}
\item{element1}{Element from 1st dimension of cube. Leave empty if there is no corresponding dimension}
\item{element2}{Element from 2nd dimension of cube. Leave empty if there is no corresponding dimension}
\item{element3}{Element from 3rd dimension of cube. Leave empty if there is no corresponding dimension}
\item{element4}{Element from 4th dimension of cube. Leave empty if there is no corresponding dimension}
\item{element5}{Element from 5th dimension of cube. Leave empty if there is no corresponding dimension}
\item{element6}{Element from 6th dimension of cube. Leave empty if there is no corresponding dimension}
\item{element7}{Element from 7th dimension of cube. Leave empty if there is no corresponding dimension}
\item{element8}{Element from 8th dimension of cube. Leave empty if there is no corresponding dimension}
\item{element9}{Element from 9th dimension of cube. Leave empty if there is no corresponding dimension}
\item{element10}{Element from 10th dimension of cube. Leave empty if there is no corresponding dimension}

}
\description{
Gets data from a cube, Supports up-to 10 dimension for now
}
\examples{
\dontrun{
tm1_get_data(
  tm1_connection("localhost", "8881", "admin", "apple"),
  "SalesCube", "Actual", "Argentina", "Total", "Sales", "Jan")

con_obj <- tm1_connection("localhost", "8881", "admin", "apple")
tm1_get_data(con_obj, "SalesCube", "Actual", "Argentina", "Total", "Sales", "Jan")
}
}
