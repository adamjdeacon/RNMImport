\name{importNmMod}
\alias{importNmMod}
\title{Read a NONMEM control file}
\usage{importNmMod(fileName = NULL, path = NULL, version = "VI", textReport = FALSE) }
\description{Parses a NONMEM control file, and returns its contents as a list of parsed elements.  These elements
will correspond to the actual control statements, e.g. $PK, $THETA, $PROBLEM, etc.  Some of the elements
are kept as pure text.}
\arguments{
	\item{fileName}{[C,1]Name of the control file}
	\item{path}{[C,1](optional) path to the control file, can be a stored path enclosed in round brackets}
	\item{version}{[C,1] NONMEM version that the control file targets.  Currently, "VII" or "7" denotes NONMEM 7, and
	other values are not differentiated}
	\item{textReport}{[L,1] Logical flag. If TRUE, textual information is logged to the stdReport log upon loading.  }
}

\examples{
	conFile <- system.file(package = "RNMImport", "unittests/testdata/control4.con")
	conContents <- importNmMod(file = conFile)
	print(conContents)
}
\value{A list with parsed components of the control file}
\author{Mango Solutions}
\keyword{IO}
