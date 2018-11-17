\name{tm1_get_cubes}
\alias{tm1_get_cubes}
\title{TM1 Get Cubes}
\usage{
tm1_get_cubes(tm1_connection, ShowControlObjects = FALSE)
}
\arguments{

\item{tm1_connection}{tm1 connection object returned by the function tm1_connection}
\item{ShowControlObjects}{If TRUE, control cubes are also listed. Default is FALSE}

}
\description{
Gets list of cubes
}
\examples{
\dontrun{
tm1_get_cubes(tm1_connection("localhost", "8881", "admin", "apple"))

con_obj <- tm1_connection("localhost", "8881", "admin", "apple")
tm1_get_cubes(con_obj)
}
}
