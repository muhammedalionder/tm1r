\name{tm1_get_config}
\alias{tm1_get_config}
\title{TM1 Get Configuration}
\usage{
tm1_get_config(tm1_connection)
}
\arguments{

\item{tm1_connection}{tm1 connection object returned by the function tm1_connection}

}
\description{
Gets configuration of tm1 instance
}
\examples{
\dontrun{
tm1_get_config(tm1_connection("localhost", "8881", "admin", "apple"))

con_obj <- tm1_connection("localhost", "8881", "admin", "apple")
tm1_get_config(con_obj)
}
}
