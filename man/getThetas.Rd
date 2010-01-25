\name{getThetas}
\alias{getThetas}
\title{Extract theta estimates from a NONMEM object}
\description{A generic function that extracts theta estimates (and initial estimates and standard errors if specified) from a NONMEM object.
}
\usage{
getThetas(obj, what = "final", ...)
}

\arguments{
  \item{obj}{NONMEM object}
  \item{what}{[C,+] Character vector of items to extract.  One or more of "final", "stderrors" or "initial"  } 
  \item{\dots}{Additional arguments that apply to different classes. These are problemNum which specifies the run required for NMRun
    		  ,subProblemNum which specifies the simulation required for NMSimModel[NM7], and method for NONMEM 7 problems}
}

\value{
	A matrix of named rows for final estimates, initial estimates, standard errors etc. as applicable
}

\author{Mango Solutions}

\examples{
	x <- importNm("TestData1.ctl", "TestData1.lst", path  = system.file(package = "RNMImport", "unittests/testdata/TestRun"))
	x.testModel <- getProblem(x)
	print(getThetas(x.testModel, what = c("initial", "final")))	
}

\keyword{methods}
\keyword{utilities}