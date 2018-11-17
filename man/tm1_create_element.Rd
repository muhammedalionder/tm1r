\name{tm1_create_element}
\alias{tm1_create_element}
\title{TM1 Create New Element to a Dimension}
\usage{
tm1_create_element(tm1_connection,
  dimension, element, parent="", weight=1)
}
\arguments{

\item{tm1_connection}{tm1 connection object returned by the function tm1_connection}
\item{dimension}{Name of dimension}
\item{element}{Name of new element}
\item{parent}{Name of parent of new element. Leave bland if there is no parent.}
\item{weight}{Weight of the element as a component to the parent. Default is 1}

}
\description{
Inserts a new element to the dimension
}
\examples{
\dontrun{
tm1_create_element(tm1_connection("localhost", "8881", "admin", "apple"),
"month", "test", "Year")

con_obj <- tm1_connection("localhost", "8881", "admin", "apple")
tm1_create_element(con_obj, "month", "test", "Year")
}
}
