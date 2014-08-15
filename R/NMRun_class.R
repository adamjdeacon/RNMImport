
validity.NMRun <- function(object)
{
	if(length(object@problems) < 1)
		return("Run has no problem results!")
	# check that all problelms extend the "NMProblem" class
	
	extendsProbClass <- sapply(object@problems, function(x) extends(class(x), "NMProblem"))
	if(!all(extendsProbClass))
		return("One or more classes in the set of problems do not extend NMProblem")
	
	
	if(object@numProblems != length(object@problems))
		return("Indicated number of problems in object@numProblems is not equal to the length of the problems list!")
	# controlText = "character", 
	# reportText = "character",
	if(length(object@controlText) == 0 || length(object@reportText) == 0)
		return("Empty control file or report file text")
	if(nrow(object@controlFileInfo) == 0 || nrow(object@reportFileInfo) == 0)
		return("Control file information or report file information is empty")
	
	TRUE
}

# declared the "Date" S3 class
setOldClass("Date")

#' This is the basic class for handling a standard NONMEM run.  In essence, it holds
#' several inidividual problems which may be of type NMBasicModel, NMSimModel, and NMSimDataGen.
#' It also holds the control file and list output file text
#' @slot controlText [C,+] Text of the control file, without comments
#' @slot controlComments [C,1] Comments of each line of the control file
#' @slot controlFileInfo [data.frame] Information about the control file 
#' @slot reportFileInfo Information about the list file
#' @slot numProblems [N,1] - Number of problems in the run
#' @slot problems [list] - List of the actual problem results 
#' @slot reportText Text of the lst output file
#' @author Mango Solutions

setClass(
	"NMRun", 
	representation(
			controlText = "character", 
			reportText = "character",
			nmVersionMajor = "character",
			nmVersionMinor = "numeric",
			controlComments = "character",
			controlFileInfo = "data.frame", 
			reportFileInfo = "data.frame",
			numProblems = "numeric",
			problems = "list"
			),
		validity = validity.NMRun
		)
