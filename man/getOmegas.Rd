\name{getOmegas}
\alias{getOmegas}
\title{Extract omega estimates from a NONMEM object}
\description{A generic function that extracts omega estimates (and initial estimates and standard errors if specified) from a NONMEM object.
}
\usage{
getOmegas(obj, what = "final", subProblemNum = 1, method = 1, problemNum = 1)
}

\arguments{
  \item{obj}{An object of class NMBasicModel, NMRun, NMSimModel, NMBasicModelNM7, NMSimModelNM7, or nmModel}
  \item{what}{[C,+] Character vector of items to extract. One or more of "final", "stderrors" or "initial" (or "shrinkage" for NONMEM 7 basic models)} 
  \item{subProblemNum}{A[N,+] Numeric vector of simulation sub-problems to use.  Only applies to simulation models}
  \item{method}{[N,+] Vector of methods to extract when dealing with NONMEM 7 problems}
  \item{problemNum}{[N,1] Number of problem to reference - applies to runs only}
  
}

\value{
	For NONMEM 6 - either a list of matrices if multiple "what=" are used, or a single matrix otherwise
 For NONMEM 7 - The same as above for a single method.  If multiple methods, a list of lists or a list of matrices
}

\author{Mango Solutions}

\examples{
	x <- importNm("TestData1.ctl", "TestData1.lst", path  = system.file(package = "RNMImport", "unittests/testdata/TestRun"))
	x.testModel <- getProblem(x)
	print(getOmegas(x.testModel, what = "final"))	
}

\keyword{methods}
\keyword{utilities}


