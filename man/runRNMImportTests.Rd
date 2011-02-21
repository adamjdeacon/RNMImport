\name{runRNMImportTests}
\alias{runRNMImportTests}
\title{run package regression tests}
\usage{runRNMImportTests(internalTestDataPath="testing", externalTestDataPath="//Mango-data1/mangowork/MangoProjects/RNONMEM2/data", testScriptPaths=c(system.file(package
    = "RNMImport", "unittests"), "testing/externaltests"), runIntern=TRUE, runExtern=FALSE, printTestProtocol=TRUE, cleanup=TRUE)
}
\description{Executes the package regression tests, and optionally produces one or two html reports. The test suite is run with the RUnit package.}
\value{A list of 1 or 2 elements holding the results of executing the test suites via the function
\code{runTestSuite}, one element for the internal tests and one for the external.}
\author{Mango Solutions}
\keyword{debugging}
\arguments{\item{internalTestDataPath}{Path to the "internal" test data.  In SVN, it can be found under importing/testing/testdata.  This data s used by distributed unit tests}
\item{externalTestDataPath}{Path to the "external" package test data, found on mango-data1.  This is used by "external" test scripts}
\item{testScriptPaths}{Path where the scripts are located.  By default, will use the package installation directory}
\item{runIntern}{Logical flag.  Should the internal tests be run?}
\item{runExtern}{Logical flag.  Should the external tests be run?}
\item{printTestProtocol}{Logical flag.  Should HTML reports be produced?}
\item{cleanup}{Should the test environments be deleted after running the tests?  Meant for development purposes.}
}
\examples{\dontrun{
x <- runRNMImportTests(runExtern = FALSE, printTestProtocol = FALSE)
summary(x[[1]])
}}

