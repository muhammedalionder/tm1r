\name{tm1_get_log}
\alias{tm1_get_log}
\title{TM1 Get Logs of an instance}
\usage{
tm1_get_log(tm1_connection, lognumber)
}
\arguments{

\item{tm1_connection}{tm1 connection object returned by the function tm1_connection}
\item{lognumber}{Number of how many lines of logs you want. Default is 5}

}
\description{
Gets server logs from a tm1 instance
}
\examples{
\dontrun{
tm1_get_log(tm1_connection("localhost", "8881", "admin", "apple"), 10)

con_obj <- tm1_connection("localhost", "8881", "admin", "apple")
tm1_get_log(con_obj)
}
}
