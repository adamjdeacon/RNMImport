\name{addedData}
\alias{addedData}
\title{Get added data }
\description{
  Obtain the derived/user-added variables from a NONMEM problem object
}
\usage{
addedData(obj, ...)
}
\arguments{
  \item{obj}{NMRun or NMProblem class object}
  \item{\dots}{ problemNum for addedData.NMRun }
}
\details{
  Most of these added variables will probably be created via addDerivedCategorical
}
\value{
	A data.frame containing the derived variables
}
\author{Mango Solutions <support@mango-solutions.com>}
\seealso{ \code{\link{nmData}}, \code{\link{nmDatabyVarType}}}
\examples{
	x <- importNm(conFile = "TestData1.ctl", reportFile = "TestData1.lst", 
	path = system.file(package = "RNMImport", "unittests/testdata/TestRun"))
	y <- getProblem(x)
	y <- addDerivedCategorical(y, "DV", breaks = 3, labels = c("low", "medium", "high"))
	y <- addDerivedCategorical(y, "TIME", breaks = 6)
	print(addedData(y))
}

\keyword{ utilities }
