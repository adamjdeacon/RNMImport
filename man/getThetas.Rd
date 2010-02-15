\name{getThetas}
\alias{getThetas}
\title{Extract theta estimates from a NONMEM object}
\description{A generic function that extracts theta estimates (and initial estimates and standard errors if specified) from a NONMEM object.
}
\usage{
getThetas(obj, what = "final", subProblemNum = 1, method = 1, problemNum = 1 )
}

\arguments{
  \item{obj}{NONMEM object}
  \item{what}{[C,+] Character vector of items to extract. One or more of "final", "stderrors" or "initial" (or "shrinkage" for NONMEM 7 basic models)} 
  \item{subProblemNum}{subProblemNum [N,+] Numeric vector of simulation sub-problems to use.  Only applies to simulation models}
  \item{method}{[N,+] Vector of methods to extract when dealing with NONMEM 7 problems}
  \item{problemNum}{[N,1] Number of problem to use - applies to NMRun only}
}

\value{
	A matrix of named rows for final estimates, initial estimates, standard errors etc. as applicable, or a list of matrices if multiple 
	methods are chosen in NONMEM 7
}

\author{Mango Solutions}

\examples{
	x <- importNm("TestData1.ctl", "TestData1.lst", path  = system.file(package = "RNMImport", "unittests/testdata/TestRun"))
	x.testModel <- getProblem(x)
	print(getThetas(x.testModel, what = c("initial", "final")))	
}

\keyword{methods}
\keyword{utilities}