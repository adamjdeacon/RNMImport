\name{MiscExtractors}
\alias{getFileinfo}
\alias{getControlStatements}
\alias{getControltext}
\alias{getReporttext}
\alias{getNmVersion}

\title{Various miscellaneous "getter"/extractor functions}

\description{\code{getFileinfo} gets information about the report and control files, 
	getControlStatements extracts parsed control file statements as a list, getControltext extracts raw control file text,
	 getReporttext extracts raw report file text, getNmVersion returns information about the NONMEM version used to create a run or problem}
 
 \usage{
 		getFileinfo(run)
 		getControlStatements(obj, ...)
 		getControltext(run)
 		getReporttext(run)
 		getNmVersion(obj)
	}
 
 \arguments{
 	\item{run}{An object of class NMRun}
 	\item{obj}{An object of class NMRun, or one that extends NMProblem}
 	\item{...}{If used on an NMRun object, should be \code{problemNum}} %TODO
 	}
 	
 \value{\code{getFileInfo}: A data.frame holding information about the source file of the NONMEM run, and the 
 report. \code{getControlStatements}: The parsed control file statements corresponding to a particular 
 NONMEM problem. \code{getControltext}, \code{getReporttext}: The raw text of a control file that generated a run,
 or the report file that was produced respectively.  \code{getNmVersion}: Named character vector with NONMEM major and minor versions.}
 
 \author{Mango Solutions <support@mango-solutions.com>}
 
 \keyword{methods}
 \keyword{utilities}

