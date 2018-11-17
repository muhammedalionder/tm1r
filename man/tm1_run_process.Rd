\name{tm1_run_process}
\alias{tm1_run_process}
\title{TM1 Run a Process}
\usage{
tm1_run_process(tm1_connection,
  process,
  par1name, par1value,
  par2name, par2value,
  par3name, par3value)
}
\arguments{

\item{tm1_connection}{tm1 connection object returned by the function tm1_connection}
\item{process}{Name of a process as a string}
\item{par1name}{Name of a parameter}
\item{par1value}{Value of a parameter}
\item{par2name}{Name of a parameter}
\item{par2value}{Value of a parameter}
\item{par3name}{Name of a parameter}
\item{par3value}{Value of a parameter}

}
\description{
Runs a process
}
\examples{
\dontrun{
tm1_run_process(tm1_connection("localhost", "8881", "admin", "apple"), "test")

con_obj <- tm1_connection("localhost", "8881", "admin", "apple")
tm1_run_process(con_obj, "test")
}
}
