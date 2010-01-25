\name{importNm7Tables}
\alias{importNm7Tables}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Import new NONMEM 7 files}
\description{
	Imports one of the new files produced by NONMEM 7: .phi, .cor, .cov, etc. into a  list of data.frames.	
}
\usage{
importNm7Tables(file, path = "", type = c("cov", "ext", "phi"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{ Name of the file }
  \item{path}{ Path to the file (including NONMEM path in brackets)}
  \item{type}{ Type of the file: One of "cov", "ext", or "phi".   }
}
\details{
  The "cov" type covers .cor, .cov and .coi files.
}
\value{
  a list of data.frames holding the content of the individual sub-tables within each file.  Each
 table will have as an attribute "method", holding the name of the method associated to it
}

\author{Mango Solutions}

\examples{
	phiTables <- importNm7Tables(type = "phi", "TestData1.phi", path = system.file(package = "RNMImport", "unittests/testdata/TestDataNM7" ))
	print(phiTables[[1]])
	print(attr(phiTables[[1]], "method"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{IO}
