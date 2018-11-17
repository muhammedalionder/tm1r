\name{tm1_delete_subset}
\alias{tm1_delete_subset}
\title{TM1 Delete Subset}
\usage{
tm1_delete_subset(tm1_connection,
  dimension, subset)
}
\arguments{

\item{tm1_connection}{tm1 connection object returned by the function tm1_connection}
\item{dimension}{Name of dimension}
\item{subset}{Name of subset}

}
\description{
Deletes subset from dimensions
}
\examples{
\dontrun{
tm1_delete_subset(tm1_connection("localhost", "8881", "admin", "apple"),
"month", "test")

con_obj <- tm1_connection("localhost", "8881", "admin", "apple")
tm1_delete_subset(con_obj, "month", "test")
}
}
