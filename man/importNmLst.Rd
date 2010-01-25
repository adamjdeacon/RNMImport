\name{importNmReport}
\alias{importNmReport}
\title{Import the information in an output report file}

\description{This routine imports the contents in a NONMEM output report file, and then parses different sections of it
while ignoring others.}
\usage{importNmReport(fileName, path = NULL, controlStatements = NULL, textReport = FALSE) }
\arguments{
	\item{fileName}{Name of the list file}
	\item{path}{(optional) Path to the list file.  Can be a path stored by setNmPath if it is in round brackets. }
	\item{controlStatements}{(optional) Control statements from the run associated with this output.  Using it 
	allows cleaner parsing of the report file}
	\item{textReport}{(optional) Logical flag.  If TRUE, certain messages will be logged to the stdReport log.}
}

\value{A named list with elements holding parsed statements in the list file}
\author{Mango Solutions}

\examples{
	x <- importNmReport("TestData1.lst", path = system.file(package = "RNMImport", "unittests/testdata/TestRun"))
	print(x)

}

\keyword{IO}
