\name{nmData}
\alias{nmData}
\title{Extract input and / or output data tables from a NONMEM object}
\description{ A generic function that extracts input and output data tables from a NONMEM object into either a single consolidated
data.frame or a list. 
}
\usage{
nmData(obj, dataTypes = c("input", "output"), returnMode = c("singleDF", "DFList"), applySubset = FALSE,  ...)
}

\arguments{
  \item{obj}{An object of class NMRun, or one that inherits from NMProblem}
  \item{dataTypes}{Which type of data should be returned, must be "input" and/or "output"}
  \item{returnMode}{ Which type of data should be returned, must be "input" and/or "output".
  If "singleDF", attempts to return a consolidated data.frame of output. and input data, if "DFList" returns 
  a list with separate input and output data}
  \item{subset}{Controls the application of a "subset" to the data when it is returned. In general this is used to remove dosing records from the data. If NULL or FALSE, nothing is done. Otherwise, it should be a character vector of subsetting expressions 
   (such as "MDV = 1", etc.) which will be applied in succession.}
  \item{\dots}{Additional arguments that apply to different classes}

}


\value{
	data.frame or list with "input" and "output" components
}

\author{Mango Solutions <fgochez@mango-solutions.com> }

\examples{
	x <- importNm("TestData1.ctl", "TestData1.lst", path  = system.file(package = "RNMImport", "unittests/testdata/TestRun"))
	x.data <- nmData(x)
	print(head(x.data))
	x.data <- nmData(x, returnMode = "DFList" )
	print(lapply(x.data, head))
	y <- importNm("TestData1SIM.con", "TestData1SIM.lst", path  = system.file(package = "RNMImport", "unittests/testdata/TestSimRun"))
	y.outputData <- nmData(y, subProblemNum = 2:4, dataTypes = "output")
	print(summary(y.outputData))
	
}

\keyword{datagen}
\keyword{utilities}
