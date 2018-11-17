\name{tm1_send_dataset}
\alias{tm1_send_dataset}
\title{TM1 Send Data Set to a Cube}
\usage{
tm1_send_dataset(tm1_connection, valueset, cube, rowdim, coldim,
                                titledim1, titleel1, titledim2, titleel2,
                                titledim3, titleel3, titledim4, titleel4,
                                titledim5, titleel5, titledim6, titleel6,
                                titledim7, titleel7, titledim8, titleel8)
}
\arguments{

\item{tm1_connection}{tm1 connection object returned by the function tm1_connection}
\item{valueset}{data frame or matrix object holding values you want to send to cube}
\item{cube}{Name of a cube as a string}
\item{rowdim}{Corresponding dimension of the elements on row}
\item{coldim}{Corresponding dimension of the elements on column}

\item{titledim1}{Name of dimension in title}
\item{titleel1}{Element of dimension in corresponding titledim}
\item{titledim2}{Name of dimension in title}
\item{titleel2}{Element of dimension in corresponding titledim}
\item{titledim3}{Name of dimension in title}
\item{titleel3}{Element of dimension in corresponding titledim}
\item{titledim4}{Name of dimension in title}
\item{titleel4}{Element of dimension in corresponding titledim}
\item{titledim5}{Name of dimension in title}
\item{titleel5}{Element of dimension in corresponding titledim}
\item{titledim6}{Name of dimension in title}
\item{titleel6}{Element of dimension in corresponding titledim}
\item{titledim7}{Name of dimension in title}
\item{titleel7}{Element of dimension in corresponding titledim}
\item{titledim8}{Name of dimension in title}
\item{titleel8}{Element of dimension in corresponding titledim}


}
\description{
Send data to a cube, Supports up-to 10 dimension for now
}
\examples{
\dontrun{

sdata <- tm1_connection("localhost", "8881", "admin", "apple")

#valueset
#    Argentina Brazil
#Jan         1      2
#Feb         3      4

tm1_send_dataset(
  sdata,
  valueset = valueset, cube = "SalesCube",
  rowdim = "month", coldim = "region",
  titledim1 = "actvsbud", titleel1 = "Actual",
  titledim2 = "model", titleel2 = "L Series 1.6 L Convertible",
  titledim3 = "account1", titleel3 = "Units")


}
}
