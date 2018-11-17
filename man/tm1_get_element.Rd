\name{tm1_get_element}
\alias{tm1_get_element}
\title{TM1 Get Element of a Dimension}
\usage{
tm1_get_element(tm1_connection, dimension, element="", index = 0)
}
\arguments{

\item{tm1_connection}{tm1 connection object returned by the function tm1_connection}
\item{dimension}{Name of a dimension as a string}
\item{element}{Name of element as a string}
\item{index}{Index of element as a numeric}

}
\description{
Gets element detail of a dimension. Name, UniqueName, Type, Level, Index, and Components.
element or index should be specified
}
\examples{
\dontrun{
tm1_get_element(
tm1_connection("localhost", "8881", "admin", "apple"),
"month", "Year")

con_obj <- tm1_connection("localhost", "8881", "admin", "apple")
tm1_get_dimension_elements(con_obj, "month", "", 7)
}
}
