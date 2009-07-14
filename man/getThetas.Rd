\name{getThetas}
\alias{getThetas}
\title{Extract theta estimates from a NONMEM object}
\description{A generic function that extracts theta estimates (and initial estimates and standard errors if specified) from a NONMEM object.
}
\usage{
getThetas(obj, stdErrors = FALSE, initial = FALSE, ...)
}

\arguments{
  \item{obj}{An object of class NMBasicModel, NMRun or NMSimModel}
  \item{stdErrors}{A boolean value that specifies whether standard errors should be returned}
  \item{initial}{A boolean value that specifies whether initial values of theta should be returned}
  \item{\dots}{Additional arguments that apply to different classes. These are problemNum which specifies the run required for NMRun
   and subProblemNum which specifies the simulation required for NMSimModel}
}

\value{
	A matrix of named rows for final estimates, initial estimates, standard errors etc. as applicable
}

\author{Mango Solutions <rweeks@mango-solutions.com>}

\examples{
	x <- importNm("TestData1.ctl", "TestData1.lst", path  = system.file(package = "RNMImport", "unittests/testdata/TestRun"))
	x.testModel <- getProblem(x)
	print(getThetas(x.testModel, stdErrors = FALSE))	
}

\keyword{methods}
\keyword{utilities}

