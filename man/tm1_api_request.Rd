\name{tm1_api_request}
\alias{tm1_api_request}
\title{TM1 API Request}
\usage{
tm1_api_request(tm1_connection, url, body ="", type = "GET")
}
\arguments{

\item{tm1_connection}{tm1 connection object returned by the function tm1_connection}
\item{url}{URL address for rest api request}
\item{body}{body text of request}
\item{type}{type of api request. Requests in httr package are supported like GET, POST, DELETE, PATCH}

}
\description{
Makes a api request to tm1 server with url and body specified
}
\examples{
\dontrun{
con_obj <- tm1_connection("localhost", "8881", "admin", "apple")
url <- "https://localhost:8881/api/v1/Cubes('SalesCube')/Dimensions"
tm1_api_request(con_obj, url, type = "GET")

}
}
