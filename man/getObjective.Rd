\name{getObjective}
\alias{getObjective}

\title{Get Objective Value}

\description{Retrieves the objective function of an NMRun, NMBasicModel, or NMSimModel object }
 
 \usage{getObjective(obj, addMinInfo = TRUE, ...) }
 
 \arguments{
 	\item{obj}{An object of class NMRun, NMBasicModel or NMSimModel}
	\item{addMinInfo}{A logical value indicating whether the attribute containing the minimisation
	information from the NONMEM run should be included with the returned vector} 	
 	
 	\item{...}{Not implemented yet}
 	}
 	
 \value{A numeric vector with an additional attribute if addMinInfo is set to TRUE}
 
 \examples{
 	x <- importNm("TestData1notab.ctl", path  = system.file(package = "RNMImport", "unittests/testdata/TestRunNoTab"))
	print(getObjective(x, problemNum = 1))
 }
 
 \author{Mango Solutions <support@mango-solutions.com>}
 \keyword{methods}