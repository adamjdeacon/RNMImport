\name{imposeCategoryFormat}
\alias{imposeCategoryFormat}
\title{Coerce variables of a NONMEM object to categories}
\description{Forces the data in a NONMEM object to conform to the variable formats specified in the metadata. Users will be allowed to choose a subset of 
variables which should be formatted.  Variables that are formatted should be coerced to factors, with the levels set as described by the comma 
separated string stored within the metadata.  
}
\usage{
imposeCategoryFormat(obj, varSubset, ...)
}

\arguments{
  \item{obj}{An object of class NMBasicModel, NMRun, NMSimModel, NMSimDataGen or data.frame}
  \item{varSubset}{The variables to be converted to factors. It can be a vector of variable names or a comma separated string}
  \item{\dots}{Additional arguments that apply to different classes. These are problemNum which specifies the required run 
  for a NMRun object}
}

\value{
	A new object with the required data coerced to factors
}

\author{Mango Solutions <rweeks@mango-solutions.com>}

\examples{
	x <- importNm("TestData1.ctl", "TestData1.lst", path  = system.file(package = "RNMImport", "unittests/testdata/TestRun"))
	x.testModel <- getProblem(x)
	y.testModel <- imposeCategoryFormat(x.testModel, "SEX, SMOK")
}

\keyword{methods}
\keyword{utilities}



