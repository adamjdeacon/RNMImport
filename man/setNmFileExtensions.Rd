\name{setNmFileExtensions}
\alias{setNmFileExtensions}
\title{Sets NONMEM file extensions}
\description{Sets new NONMEM file extensions for specific file types, e.g. one may want to change the allowed extensions for a 
NONMEM list file.}
\usage{
setNmFileExtensions(parameter, extension)
}
\arguments{
  \item{parameter}{The name of the NONMEM file type. Must be one of control, list, table or input}
  \item{extension}{A vector of new file extensions.}
}
\value{None}
\author{Mango Solutions <rweeks@mango-solutions.com>}
\keyword{environment}