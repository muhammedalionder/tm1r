\name{tm1_logout}
\alias{tm1_logout}
\title{TM1 Log Out}
\usage{
tm1_logout(tm1_connection)
}
\arguments{

\item{tm1_connection}{tm1 connection object returned by the function tm1_connection}

}
\description{
Logs out
}
\examples{
\dontrun{
tm1_logout(tm1_connection("localhost", "8881", "admin", "apple"))

con_obj <- tm1_connection("localhost", "8881", "admin", "apple")
tm1_logout(con_obj)
}
}
