\name{runRNMImportTests}
\alias{runRNMImportTests}
\title{ run package regression tests }
\description{
	Executes the package regression tests, and optionally produces one or two 
	html reports. The test suite is run with the RUnit package.
}

\usage{
runRNMImportTests(
		testDataPaths = c(system.file(package="RNMImport", "unittests"), 
				"\\\\Mango-data1\\mangowork\\MangoProjects\\RNONMEM2\\data"), 
		testScriptPaths = c(system.file(package="RNMImport", "unittests"), "testing/externaltests"),
		runExtern = FALSE,
		printTestProtocol = TRUE,
		cleanup = TRUE
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{testDataPaths}{ Paths to locate the data for the suites.  Up to 4 may be specified, but the first one should 
  be located on the user's computer.   }
  \item{testScriptPaths}{ Path where the scripts are located.  By default, will use the package installation directory }
  \item{runExtern}{ Logical flag.  Should the external tests be run? }
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
