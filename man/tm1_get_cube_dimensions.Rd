\name{tm1_get_cube_dimensions}
\alias{tm1_get_cube_dimensions}
\title{TM1 Get Dimensions of a Cube}
\usage{
tm1_get_cube_dimensions(tm1_connection, cube)
}
\arguments{

\item{tm1_connection}{tm1 connection object returned by the function tm1_connection}
\item{cube}{Name of a cube as a string}

}
\description{
Gets dimensions of a cube
}
\examples{
\dontrun{
tm1_get_cube_dimensions(
tm1_connection("localhost", "8881", "admin", "apple"),
"SalesCube")

con_obj <- tm1_connection("localhost", "8881", "admin", "apple")
tm1_get_cube_dimensions(con_obj, "SalesCube")
}
}
