\name{getNmFileExtensions}
\alias{getNmFileExtensions}
\title{Gets allowed NONMEM file extensions}
\description{Returns a vector of allowed file extensions for a specific NONMEM file type, e.g. control, report, output table or 
input data.}
\usage{
getNmFileExtensions(parameter)
}
\arguments{
  \item{parameter}{String with the file type. Must be one of "control", "report", "table" or "input"}
}
\value{A vector of appropriate file extensions}
\author{Mango Solutions <support@mango-solutions.com>}
\keyword{environment}