\name{tm1_delete_element}
\alias{tm1_delete_element}
\title{TM1 Delete Element or Component}
\usage{
tm1_delete_element(tm1_connection,
  dimension, element, parent="")
}
\arguments{

\item{tm1_connection}{tm1 connection object returned by the function tm1_connection}
\item{dimension}{Name of dimension}
\item{element}{Name of element}
\item{parent}{Name of parent of element. If parent is specified, component delete will be done.
If parent is omitted, element will be deleted from dimension}

}
\description{
Deletes element or component from dimensions
}
\examples{
\dontrun{
tm1_delete_element(tm1_connection("localhost", "8881", "admin", "apple"),
"month", "test", "Year")

con_obj <- tm1_connection("localhost", "8881", "admin", "apple")
tm1_delete_element(con_obj, "month", "test")
}
}
