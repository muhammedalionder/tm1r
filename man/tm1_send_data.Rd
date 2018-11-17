\name{tm1_send_data}
\alias{tm1_send_data}
\title{TM1 Send Data to a Cube}
\usage{
tm1_send_data(tm1_connection, value, cube,
                                element1, element2, element3, element4, element5,
                                element6, element7, element8, element9, element10,
                                increment)
}
\arguments{

\item{tm1_connection}{tm1 connection object returned by the function tm1_connection}
\item{value}{data value you want to send to cube}
\item{cube}{Name of a cube as a string}
\item{element1}{Element from 1st dimension of cube. Leave empty if there is no dimension}
\item{element2}{Element from 2nd dimension of cube. Leave empty if there is no dimension}
\item{element3}{Element from 3rd dimension of cube. Leave empty if there is no dimension}
\item{element4}{Element from 4th dimension of cube. Leave empty if there is no dimension}
\item{element5}{Element from 5th dimension of cube. Leave empty if there is no dimension}
\item{element6}{Element from 6th dimension of cube. Leave empty if there is no dimension}
\item{element7}{Element from 7th dimension of cube. Leave empty if there is no dimension}
\item{element8}{Element from 8th dimension of cube. Leave empty if there is no dimension}
\item{element9}{Element from 9th dimension of cube. Leave empty if there is no dimension}
\item{element10}{Element from 10th dimension of cube. Leave empty if there is no dimension}
\item{increment}{If TRUE, it will increment cube data by Value. If False, it will replace. This parameter is ignored in sending string values.}

}
\description{
Send data to a cube, Supports up-to 10 dimension for now
}
\examples{
\dontrun{
tm1_send_data(
  tm1_connection("localhost", "8881", "admin", "apple"),
  10,
  "SalesCube",
  "Actual", "Argentina", "S Series 1.8 L Sedan", "Units", "Jan")

con_obj <- tm1_connection("localhost", "8881", "admin", "apple")
tm1_send_data(con_obj,
  10,
  "SalesCube",
  "Actual", "Argentina", "S Series 1.8 L Sedan", "Units", "Jan",
  increment=TRUE)
}
}
