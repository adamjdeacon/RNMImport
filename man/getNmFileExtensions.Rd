\name{getNmFileExtensions}
\alias{getNmFileExtensions}
\title{Gets allowed NONMEM file extensions}
\description{Returns a vector of allowed file extensions for a specific NONMEM file type, e.g. control, list, output table or 
input data.}
\usage{
getNmFileExtensions(parameter)
}
\arguments{
  \item{parameter}{The file type. Must be one of control, list, table or input}
}
\value{A vector of appropriate file extensions}
\author{Mango Solutions <rweeks@mango-solutions.com>}
\keyword{environment}