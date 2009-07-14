\name{importNmMod}
\alias{importNmMod}
\title{Read a NONMEM control file}
\usage{importNmMod(fileName, path)}
\description{Parses a NONMEM control file, and returns its contents as a list of parsed elements.  These elements
will correspond to the actual control statements, e.g. $PK, $THETA, $PROBLEM, etc.  Some of the elements
are kept as pure text.}
\arguments{
	\item{fileName}{Name of the control file}
	\item{path}{(optional) path to the control file, can be a stored path enclosed in round brackets}
}

\examples{
	conFile <- system.file(package = "RNMImport", "unittests/testdata/control4.con")
	conContents <- importNmMod(file = conFile)
	print(conContents)
}
\value{A list with parsed components of the control file}
\author{Mango Solutions}
\keyword{IO}
