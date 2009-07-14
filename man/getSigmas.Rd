\name{getSigmas}
\alias{getSigmas}
\title{Extract sigma estimates from a NONMEM object}
\description{ A generic function that extracts sigma estimates (and initial estimates and standard errors if specified) from a NONMEM object.
}
\usage{
getSigmas(obj, stdErrors = FALSE, initial = FALSE, ...)
}

\arguments{
  \item{obj}{An object of class NMBasicModel, NMRun or NMSimModel}
  \item{stdErrors}{A boolean value that specifies whether standard errors should be returned}
  \item{initial}{A boolean value that specifies whether initial values of sigma should be returned}
  \item{\dots}{Additional arguments that apply to different classes. These are problemNum which specifies the run required for NMRun
   and subProblemNum which specifies the simulation required for NMSimModel}
}


\value{
	A 3 dimensional array with a matrix of estimates, and if specified, a matrix of initial values and standard errors
}

\author{Mango Solutions <rweeks@mango-solutions.com>}

\examples{
	x <- importNm("TestData1.ctl", "TestData1.lst", path  = system.file(package = "RNMImport", "unittests/testdata/TestRun"))
	x.testModel <- getProblem(x)
	print(getSigmas(x.testModel, stdErrors = FALSE))	
}

\keyword{methods}
\keyword{utilities}