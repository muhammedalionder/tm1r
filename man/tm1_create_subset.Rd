\name{tm1_create_subset}
\alias{tm1_create_subset}
\title{TM1 Create New Subset to a Dimension}
\usage{
tm1_create_subset(tm1_connection,
  dimension, subset, element="", mdx="", overwrite=TRUE)
}
\arguments{

\item{tm1_connection}{tm1 connection object returned by the function tm1_connection}
\item{dimension}{Name of dimension}
\item{subset}{Name of new subset}
\item{element}{Name of elements seperated by | for static subset}
\item{mdx}{mdx of subset for dynamic subset}
\item{overwrite}{TRUE or FALSE. If TRUE, subset is overwritten}

}
\description{
Creates a new subset to the dimension
}
\examples{
\dontrun{
tm1_create_subset(tm1_connection("localhost", "8881", "admin", "apple"),
"month", "Q1Months", element = "Jan|Feb|Mar")

con_obj <- tm1_connection("localhost", "8881", "admin", "apple")
tm1_create_element(con_obj, "month", "all", mdx = "[month].MEMBERS")
}
}
