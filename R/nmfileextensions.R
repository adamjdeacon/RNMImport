

#' Sets the allowed file extensions for various NONMEM file types.  These are control, report, output tables, and 
#' input data tables.  When loading a file, the package will fail if its extension is not admissable. 
#' @title file allowed file extensions
#' @param parameter The name of the file type. These are \code{control}, \code{report}, \code{table} and \code{input}
#' @param extension A character vector of new file type extensions
#' @return None
#' @author Mango Solutions
#' @keywords environment
#' @examples
#' \dontrun{
#'      setNmFileExtensions("control", c("mod", "con") )
#' }
#' @export

setNmFileExtensions <- function(parameter, extension)
{
    if(!(parameter %in% c("control", "report", "table", "input")))
        RNMImportStop(msg = "Name of file type not recognised.\n")
    if(parameter == "control")
        .RNMImportEnv$fileExtensions[["control"]] <- extension
    if(parameter == "report")
        .RNMImportEnv$fileExtensions[["report"]] <- extension
    if(parameter == "table")
        .RNMImportEnv$fileExtensions[["outputTable"]] <- extension
    if(parameter == "input")    
        .RNMImportEnv$fileExtensions[["inputData"]] <- extension
}

#' Returns a vector of allowed file extensions for a specific NONMEM file type, e.g. control, report, output table or 
#' input data.Gets the conventional file name extensions used by NONMEM.
#' @title Gets allowed NONMEM file extensions
#' @param parameter String with the file type. Must be one of "control", "report", "table" or "input"
#' @return A vector of allowed file type extensions
#' @author Mango Solutions
#' @examples 
#' getNmFileExtensions("control")
#' @keywords environment
#' @export

getNmFileExtensions <- function(parameter)
{
    switch(parameter,
            "control" = .RNMImportEnv$fileExtensions[["control"]], 
            "report" = .RNMImportEnv$fileExtensions[["report"]], 
            "table" = .RNMImportEnv$fileExtensions[["outputTable"]], 
            "input" = .RNMImportEnv$fileExtensions[["inputData"]],
            RNMImportStop(msg = "Name of file type not recognised.\n"))
}