% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read}
\alias{fars_read}
\title{fars_read}
\usage{
fars_read(filename)
}
\arguments{
\item{filename}{A string that contains the path and file name}
}
\value{
The function returns either a dataframe made from the data in the file
or no dataframe with a message " 'file' does not exist" if there is no
file with the name provided
}
\description{
This function uses readr::read_csv to read a .csv file from \code{filename},
and then create a dataframe from the data in the file.
Function will check if the file exists. If it doesn't, it will print message.
}
\examples{
\dontrun{fars_read("accident_2013.csv.bz2")}

}
