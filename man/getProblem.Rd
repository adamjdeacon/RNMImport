\name{getProblem}
\alias{getProblem}
\title{Retrieve NONMEM problem}
\description{
  Extracts a specific problem from a NONMEM run.  Problems are chunks of NONMEM modelling code that begin with
  \code{$PROB}.
}
\usage{
	getProblem(run, problemNumber = 1)
}

\arguments{
  \item{run}{NMRun class object}
  \item{problemNumber}{Integer with the number of the object requested.  }
}

\value{
 	An object of a class that extends NMProblem
}
\note{
	At the moment, only one problem can be imported for each run.  Multi-problem runs cannot yet be handled, but will
	in the near future.
}
\author{Mango Solutions <support@mango-solutions.com>}
\examples{
	x <- importNm("TestData1.ctl", "TestData1.lst", path  = system.file(package = "RNMImport", "unittests/testdata/TestRun"))
	x.prob <- getProblem(x, 1)
	print(x.prob)
}
\keyword{ utilities }

