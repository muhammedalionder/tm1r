\name{tm1_get_dimension_elements}
\alias{tm1_get_dimension_elements}
\title{TM1 Get Elements of a Dimension}
\usage{
tm1_get_dimension_elements(tm1_connection, dimension)
}
\arguments{

\item{tm1_connection}{tm1 connection object returned by the function tm1_connection}
\item{dimension}{Name of a dimension as a string}

}
\description{
Gets elements of a dimension
}
\examples{
\dontrun{
tm1_get_dimension_elements(
tm1_connection("localhost", "8881", "admin", "apple"),
"region")

con_obj <- tm1_connection("localhost", "8881", "admin", "apple")
tm1_get_dimension_elements(con_obj, "region")
}
}
