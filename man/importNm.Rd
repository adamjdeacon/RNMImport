\name{importNm}
\alias{importNm}
\title{Import a NONMEM run}
\usage{importNm(conFile, reportFile = NULL, path = NULL, dropInputColumns = TRUE)}

\description{Imports a NONMEM run based on a control file and an output list file.  This function assumes that 
all of the input and output data tables are in the same directory as the control file, though the input
tables are allowed to be missing.}

\arguments{
	\item{conFile}{[C,1] control file name}
	\item{reportFile}{[C,1] The name of the list file.  In the future this parameter will be optional, 
	in which case importNm will attempt' to deduce the file name on its own based on the name of the conFile
	and the list file extensions stored in the meta-data.}
	\item{path}{The path where the files are located.  Can be the name of a path stored with setNmPath if it
	is surrounded by round brackets}
	\item{dropInputColumns} {Logical flag.  If TRUE, those columns of the input data flagged for dropping in the 
	 $INPUT statement will not be imported with the run}
	 \item{textReport}{Logical flag.  If TRUE, various standard text outputs will be logged to stdReport}
}

\value{An NMRun object that holds the information of the NONMEM run}

\examples{
	x <- importNm("TestData1.ctl", "TestData1.lst", path  = system.file(package = "RNMImport", "unittests/testdata/TestRun"))
	print(x)
}

\author{Mango Solutions <support@mango-solutions.com>}
\keyword{IO}