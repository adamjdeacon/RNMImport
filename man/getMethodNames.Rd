\name{getMethodNames}
\Rdversion{1.1}
\alias{getMethodNames}

\title{
Get method name
}
\description{
 Extracts estimation method names from a run or problem that used NONMEM 7.  These
 can be the names used in the control file, or the report file
}

\usage{
getMethodNames(obj, what = c("report", "control"), problemNum = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{ NMRun, NMBasicModelNM7, or NMSimModelNM7}
  \item{what}{ [C,1] The string "report" or "control" - selects which file should be used for the method names}
  \item{problemNum}{ Problem number in run}
}
\details{
 If what = "control", the names are taken from the $EST control sections
}
\value{
A character vector of method names used
}
\author{
 Mango Solutions
}


\examples{
	x <- importNm("TestData1.ctl", "TestData1.lst", path  = system.file(package = "RNMImport", "unittests/testdata/TestDataNM7"))
	print(getMethodNames(x))
}
\keyword{methods}
\keyword{utilities}


