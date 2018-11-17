\name{tm1_delete_view}
\alias{tm1_delete_view}
\title{TM1 Delete View}
\usage{
tm1_delete_view(tm1_connection, cube, view)
}
\arguments{

\item{tm1_connection}{tm1 connection object returned by the function tm1_connection}
\item{cube}{Name of cube}
\item{view}{Name of view to be deleted}

}
\description{
Deletes cube view
}
\examples{
\dontrun{

tm1_delete_view(
  tm1_connection("localhost", "8881", "admin", "apple"),
  "SalesCube", "test")

con_obj <- tm1_connection("localhost", "8881", "admin", "apple")
tm1_delete_view(con_obj, "SalesCube", "test")
}
}
