\name{tm1_create_view}
\alias{tm1_create_view}
\title{TM1 Create View from mdx}
\usage{
tm1_create_view(tm1_connection, cube, view, mdx)
}
\arguments{

\item{tm1_connection}{tm1 connection object returned by the function tm1_connection}
\item{cube}{Name of cube}
\item{view}{Name of view to be created}
\item{mdx}{MDX of view as a string}


}
\description{
Creates cube view with mdx
}
\examples{
\dontrun{
mdx <- "SELECT
  NON EMPTY
   {[month].[Jan],[month].[Feb],[month].[Mar]}
  ON COLUMNS,
  NON EMPTY
     {[account1].[Price],[account1].[Units]}
  ON ROWS
 FROM [SalesCube]
 WHERE
  (
   [actvsbud].[actvsbud].[Actual],
   [region].[region].[Argentina],
   [model].[model].[S Series 1.8 L Sedan]
  )"
tm1_create_view(
  tm1_connection("localhost", "8881", "admin", "apple"),
  "SalesCube", "test", mdx)

con_obj <- tm1_connection("localhost", "8881", "admin", "apple")
tm1_create_view(con_obj, "SalesCube", "test", mdx)
}
}
