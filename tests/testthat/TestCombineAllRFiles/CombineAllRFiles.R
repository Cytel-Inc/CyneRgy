#################################################################################################### .
#   Program/Function Name:
#   Author: Author Name
#   Description: This function is used to combine all .R files in a directory into a single file for use in Cytel products. 
#   Change History:
#   Last Modified Date: 01/26/2024
#################################################################################################### .
#' @name CombineAllRFiles
#' @title CombineAllRFiles
#' @description { Description: This function is used to combine all .R files in a directory into a single file for use in Cytel products.  }
#' @export
CombineAllRFiles <- function( strOutFileName, strDirectory = "" )
{

    
    # Get the list of files in the specified directory
    vFileList <- list.files(path = strDirectory, full.names = TRUE)
    
    # Create or open the output file
    outputStream <- file(strOutFileName, open = "w")
    
    # Vector to store the names of the combined files
    vCombinedViles <- character(0)
    
    # Loop through each file in the directory
    for (strFileName in vFileList) 
    {
        
        # Read the content of the current file
        strFileContent <- readLines(strFileName)
        
        # Insert a comment with file name and timestamp
        strComment     <- paste("# File:", basename(strFileName), "Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
        strFileContent <- c( strComment, strFileContent )
        
        # Write the content to the output file
        writeLines(strFileContent, outputStream)
        
        # Add the name of the combined file to the vector
        vCombinedViles <- c(vCombinedViles, basename(strFileName))
    }
    
    # Close the output file
    close(outputStream)
    
    # Print the names of the combined files
    cat("Files combined successfully:", paste(vCombinedViles, collapse = ", "), "\n")
}

