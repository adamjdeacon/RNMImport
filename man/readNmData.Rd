\name{readNmData}
\alias{readNmData}

\title{Read a NONMEM input or output data table file}
\description{
  Reads a data table accepted by NONMEM for input or produced as output into a 
  \code{data.frame}.  The routine accepts various options which can be passed to NONMEM itself
  (such as "ignore" characters) and parses the table accordingly.
  
}
\usage{
	readNmData(file, ignore = NULL, accept = NULL, translate = NULL, records = NULL, null = NA, sep = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{ Name of the data file to read }
  \item{ignore}{ ignore option, see NM?$DATA[IGNORE] }
  \item{accept}{ accept option, see NM?$DATA[ACCEPT] }
  \item{translate}{ translate option, see NM?$DATA[TRANSLATE] }
  \item{records}{ number of records, see NM?$DATA[RECORDS] }
  \item{null}{ value to replace NA, see NM?$DATA[NULL] }
  \item{sep}{ character that separates fields }
}
\details{
 	Any table file headers found are removed from the data - repeated headers are also removed 
}
\value{
	A data.frame holding the table
}
\note{
	At the moment, there is an issue with the "ignore" field, in that it does not handle multiple ignore
	characters.  This will be dealt with in future releases.
}
\author{ Mango Solutions }

\examples{
	inData <- readNmData(file = system.file( package = "RNMImport", "unittests/testdata/TestRun/Data1"), ignore = "@" ) 
	print(head(inData))
	outData <- readNmData(file = system.file( package = "RNMImport", "unittests/testdata/TestRun/Data1"))
	print(head(outData))
}
\keyword{ IO }