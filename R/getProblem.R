


#' Retrieves a desired problem from a full NONMEM run object.
#' @title Extract an individual NONMEM problem
#' @param run An NMRun class object
#' @param problemNumber Number of the problem to retrieve
#' @return The given problem number (NMBasicModel, NMSimDataGen, or NMSimModel)
#' @author Mango Solutions
#' @note If \code{problemNum} is out of the acceptable range of problems, an exception will be generated.  The same
#' applies if \code{run} is not an NMRun object
#' @keywords manip
#' @export

getProblem <- function(run, problemNumber = 1)
{
	assertClass(run, "NMRun")
	if(problemNumber < 1 | problemNumber > run@numProblems)
	{
		
        # build an error message
        
        msg <- problemNumber %pst% " is not a valid problem number\n"
		RNMImportStop(msg, call = match.call())
		
	}
	run@problems[[problemNumber]]
}
