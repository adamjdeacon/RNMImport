\name{runRNMImportTests}
\alias{runRNMImportTests}
\title{ run package regression tests }
\description{
	Executes the package regression tests, and optionally produces one or two 
	html reports. The test suite is run with the RUnit package.
}

\usage{
runRNMImportTests(internalTestDataPath = system.file(package = "RNMImport", 
    "unittests"), externalTestDataPath = "\\\\Mango-data1\\mangowork\\MangoProjects\\RNONMEM2\\data", 
    testScriptPaths = c(system.file(package = "RNMImport", "unittests"), 
        "testing/externaltests"), runIntern = TRUE, runExtern = FALSE, 
    printTestProtocol = TRUE, cleanup = TRUE) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{internalTestDataPath}{ Path to the "internal" package-distributed test data }
  \item{externalTestDataPath}{  Path to the "external" package test data, not distributed with the package}
  \item{testScriptPaths}{ Path where the scripts are located.  By default, will use the package installation directory }
  \item{runExtern}{ Logical flag.  Should the external tests be run? }
  \item{runIntern}{ Logical flag.  Should the internal tests be run? }
  \item{printTestProtocol}{ Logical flag.  Should HTML reports be produced?}
	\item{cleanup}{ Should the test environments be deleted after running the tests?  Meant for development purposes. }
}

\value{
  A list of 1 or 2 elements holding the results of executing the test suites via the function
  \code{runTestSuite}, one element for the internal tests and one for the external. 
}

\author{Mango Solutions}

\examples{
	\dontrun{
	x <- runRNMImportTests(runExtern = FALSE, printTestProtocol = FALSE)
	summary(x[[1]])
	}
}
\keyword{ debugging }
