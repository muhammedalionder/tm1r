\name{tm1_get_native_view}
\alias{tm1_get_native_view}
\title{TM1 Get Data from a Native View}
\usage{
tm1_get_native_view(tm1_connection, cube, view, RowElementAsColumn= FALSE)
}
\arguments{

\item{tm1_connection}{tm1 connection object returned by the function tm1_connection}
\item{cube}{Name of the cube}
\item{view}{Name of the view}
\item{RowElementAsColumn}{if False, row elements will be attached to rownames of data frame}

}
\description{
Gets native view data
}
\examples{
\dontrun{

tm1_get_native_view(
tm1_connection("localhost", "8881", "admin", "apple"),
  "SalesCube", "Default", RowElementAsColumn=FALSE)

con_obj <- tm1_connection("localhost", "8881", "admin", "apple")
tm1_get_native_view(con_obj, "SalesCube", "Default")
}
}
