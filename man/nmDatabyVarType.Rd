\name{nmDatabyVarType}
\alias{nmDatabyVarType}

\title{Get NONMEM data by variable type}
\description{
	Retrieves data stored in a NONMEM object by category of data type, e.g. covariates, 
	"etas", etc.  The type mappings are defined in the configuration data, and may be changed with
	get/setNmVarDescription
}
\usage{
	nmDatabyVarType(obj, varTypes, returnMode = c("singleDF", "DFList"), ...)
}

\arguments{
  \item{obj}{Object of class NMRun or NMProblem}
  \item{varTypes}{ Character vector of types to return.  Allowed types are currently "Parameter", 
  "Lab covariate", "Covariate" and "Eta". }
  \item{returnMode}{ A string, either "singleDF" or "DFList".  If "singleDF", all of the data
		is returned in a consolidated data.frame, otherwise as a list returned by type}
  \item{\dots}{  }
}

\value{
	A data.frame, or list, with the data
}

\author{Mango Solutions}

\seealso{\code{\link{nmData}}, \code{\link{getVarDescription}} }
\examples{
	x <- importNm("TestData1.ctl", "TestData1.lst", path  = system.file(package = "RNMImport", "unittests/testdata/TestRun"))
	x.covariates <- nmDatabyVarType(x, varTypes = "Covariate")
	print(head(x.covariates))
}
\keyword{datagen}
\keyword{utilities}
