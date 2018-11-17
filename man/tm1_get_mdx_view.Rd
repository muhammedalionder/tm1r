\name{tm1_get_mdx_view}
\alias{tm1_get_mdx_view}
\title{TM1 Get Data from an MDX View}
\usage{
tm1_get_mdx_view(tm1_connection, mdx, RowElementAsColumn = FALSE)
}
\arguments{

\item{tm1_connection}{tm1 connection object returned by the function tm1_connection}
\item{mdx}{MDX of view as a string}
\item{RowElementAsColumn}{if False, row elements will be attached to rownames of data frame}

}
\description{
Gets mdx view data
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
tm1_get_mdx_view(
  tm1_connection("localhost", "8881", "admin", "apple"),
  mdx, RowElementAsColumn=FALSE)

con_obj <- tm1_connection("localhost", "8881", "admin", "apple")
tm1_get_mdx_view(con_obj,mdx)
}
}
