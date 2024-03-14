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
    vFileList <- list.files(path = strDirectory, pattern = "\\.R$|\\.r$", full.names = TRUE)
     
    # Create or open the output file
    outputStream <- file(strOutFileName, open = "w")
    
    # Vector to store the names of the combined files
    vCombinedViles <- c()
    
    # Loop through each file in the directory
    iFileCount <- 0
    for (strFileName in vFileList) 
    {
        iFileCount <- iFileCount + 1 
        # Read the content of the current file
        strFileContent       <- readLines(strFileName, warn = FALSE)
        strFileTimeStamp     <- file.info(strFileName)$mtime
        strFormatedTimeStamp <- format(strFileTimeStamp, "%Y-%m-%d %H:%M:%S")
        
        
        # Insert a comment with file name and timestamp
        strComment     <- paste("\n")
        strComment     <- paste0( strComment, "##################################################################################### #\n")
        strComment     <- paste0(strComment, "# File ", iFileCount , ": ", basename(strFileName), " Timestamp: ", strFormatedTimeStamp, " ####\n")
        strComment     <- paste0( strComment, "##################################################################################### #\n\n")
      
        strFileContent <- c( strComment, strFileContent )
        
        # Write the content to the output file
        writeLines(strFileContent, outputStream)
        
        # Add the name of the combined file to the vector
        vCombinedViles <- c(vCombinedViles,basename(strFileName) )
    }
    
    # Close the output file
    close(outputStream)
    
    # Print the names of the combined files
    
    strReturn <- paste( paste( iFileCount, "Files combined successfully:\n"), paste(vCombinedViles, collapse = "\n"), "\n")
    return( list( nQtyCombinedFiles = iFileCount, strResults = strReturn ) )
}



