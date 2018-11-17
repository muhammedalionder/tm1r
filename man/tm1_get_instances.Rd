\name{tm1_get_instances}
\alias{tm1_get_instances}
\title{TM1 Get Instances}
\usage{
tm1_get_instances(adminhost = "localhost", port = "5898", ssl=TRUE)
}
\arguments{

\item{adminhost}{adminhost of tm1 models}
\item{port}{port of admin server}
\item{ssl}{If TRUE it will be accesses through https}

}
\description{
Returns the list of tm1 instances in the specified adminhost
}
\examples{
\dontrun{
tm1_get_instances()

tm1_get_instances(adminhist = "localhost",
                  port = "5898", ssl = TRUE)

}
}
