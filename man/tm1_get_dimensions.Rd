\name{tm1_get_dimensions}
\alias{tm1_get_dimensions}
\title{TM1 Get Dimensions}
\usage{
tm1_get_dimensions(tm1_connection, ShowControlObjects = FALSE)
}
\arguments{

\item{tm1_connection}{tm1 connection object returned by the function tm1_connection}
\item{ShowControlObjects}{If TRUE, control dimensions are also listed. Default is FALSE}

}
\description{
Gets list of dimensions
}
\examples{
\dontrun{
tm1_get_dimensions(tm1_connection("localhost", "8881", "admin", "apple"))

con_obj <- tm1_connection("localhost", "8881", "admin", "apple")
tm1_get_dimensions(con_obj)
}
}
