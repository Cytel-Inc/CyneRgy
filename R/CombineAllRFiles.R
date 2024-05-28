#################################################################################################### .
#   Program/Function Name: CombineAllRFiles
#   Author: Author Name J. Kyle Wathen and Subhajit Sengupta
#   Description: This function is used to combine all .R files in a directory into a single file for use in Cytel products. 
#   Change History:
#   Last Modified Date: 04/26/2024
#################################################################################################### .
#' This function combines the contents of all R files in a specified directory into one file.
#'
#' @param strOutFileName The name of the output file. If not provided, the function will return the combined content.
#' @param strDirectory The directory where the R files are located. Defaults to the current working directory.
#' @param strFileNameToIgnore The name of any file to be ignored during the combination process. Defaults to NA.
#' @description
#' This function combines the contents of all R files in a specified directory into one file.
#' It also replaces any sequence of one or more `#` characters with a single `#`.
#'
#' 
#' @return A list containing the following elements:
#' \itemize{
#'   \item{nQtyCombinedFiles: The number of files combined.}
#'   \item{strCombinedContents: The combined content of all the R files (only if strOutFileName is NA).}
#'   \item{strReturn: A string summarizing the operation, including the names of the combined files.}
#' }
#' @examples
#' \dontrun{
#'   result <- CombineAllRFiles(strOutFileName = "combined.R", strDirectory = "/path/to/your/directory")
#'   print(result$strReturn)
#' }
#'
#' @seealso \code{\link[base]{list.files}}, \code{\link[base]{file}}, \code{\link[base]{readLines}}, \code{\link[base]{writeLines}}
#' 
#' @export
CombineAllRFiles <- function(strOutFileName = NA, strDirectory = "", strFileNameToIgnore = NA, strListName = NA) {
    bReturnContents <- FALSE 
    if(is.na(strOutFileName)) {
        bReturnContents <- TRUE 
    }
    
    # Get the list of files in the specified directory
    vFileList <- list.files(path = strDirectory, pattern = "\\.R$|\\.r$", full.names = TRUE)
    
    # Create or open the output file
    strCombinedContents <- ""
    if(!bReturnContents)
        outputStream <- file(strOutFileName, open = "w")
    
    strPrefixFunctionNames <- ""
    if( !is.na( strListName ) )
    {
        # The user is combining files and wants them in a list 
        strCombinedContents <- paste0( strListName, " <- list()")
        strPrefixFunctionNames <- paste0( strListName, "$" )
    }
    
    # Vector to store the names of the combined files
    vCombineFiles <- c()
    
    # Loop through each file in the directory
    iFileCount <- 0
    for (strFileName in vFileList) 
    {
        # Skip the file if its name contains strFileNameToIgnore
        if (!is.na(strFileNameToIgnore) && grepl(strFileNameToIgnore, strFileName)) 
        {
            next
        }
        
        iFileCount <- iFileCount + 1 
        # Read the content of the current file
        strFileContent       <- readLines(strFileName, warn = FALSE)
        strFileTimeStamp     <- file.info(strFileName)$mtime
        strFormatedTimeStamp <- format(strFileTimeStamp, "%Y-%m-%d %H:%M:%S")
        
        # Insert a comment with file name and timestamp
        strComment     <- paste("\n")
        strComment     <- paste0(strComment, "##################################################################################### #\n")
        strComment     <- paste0(strComment, "# File ", iFileCount , ": ", basename(strFileName), " Timestamp: ", strFormatedTimeStamp, " ####\n")
        strComment     <- paste0(strComment, "##################################################################################### #\n\n")
        
        #if( strPrefixFunctionNames != "" )
        #    strFileContent <- gsub("(^\\s*)([a-zA-Z0-9._]+)(\\s*<-\\s*function)", paste0("\\1", strPrefixFunctionNames, "\\2\\3"), strFileContent)
        
        
        strFileContent <- c(strComment, strFileContent)
        
        # Write the content to the output file or a append to the other input read in
        #if(bReturnContents) {
            #strCombinedContents <- paste(paste0(strFileContent, collapse= "\n"), strCombinedContents, collapse = "\n")
            strCombinedContents <- paste( strCombinedContents, paste0(strFileContent, collapse= "\n"),  collapse = "\n")
        #}
        # if(!bReturnContents)
        #     writeLines(strFileContent, outputStream)
        # else {
        #     strCombinedContents <- paste(paste0(strFileContent, collapse= "\n"), strCombinedContents, collapse = "\n")
        # }
        
        # Add the name of the combined file to the vector
        vCombineFiles <- c(vCombineFiles, basename(strFileName))
    }
    
    if( strPrefixFunctionNames != "" )
    {
        exprs <- parse(text = strCombinedContents)
        for (i in seq_along(exprs)) {
            expr <- exprs[[i]]
            if (is.call(expr) && (expr[[1]] == as.name("<-") || expr[[1]] == as.name("=")) && is.call(expr[[3]]) && expr[[3]][[1]] == as.name("function")) {
                # This is a function definition, so modify the function name
                expr[[2]] <- as.name(paste0(strPrefixFunctionNames, expr[[2]]))
                exprs[[i]] <- expr
            }
        }
        
        # Convert the modified expressions back into a string
        strCombinedContents <- gsub("`", "", paste(sapply(exprs, deparse), collapse = "\n"))
    }
    
    if(!bReturnContents)
        writeLines(strCombinedContents, outputStream)
    lReturn <- list(nQtyCombinedFiles = iFileCount)
    
    # Close the output file
    if(!bReturnContents)
        close(outputStream)
    else {
        # Remove all "\n" and replace any duplicate white spaces with a single white space
        strCombinedContents <- gsub("\n", " ", strCombinedContents)
        strCombinedContents <- gsub("\\s+", " ", strCombinedContents)
        
        # Remove any duplicate '#' characters
        strCombinedContents <- gsub("#+", "#", strCombinedContents)
        
        lReturn$strCombinedContents <- strCombinedContents
    }
    
    # Print the names of the combined files
    lReturn$strReturn <- paste(paste(iFileCount, "Files combined successfully:\n"), paste(vCombineFiles, collapse = "\n"), "\n")
    
    return(lReturn)
}

