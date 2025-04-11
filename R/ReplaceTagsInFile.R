#################################################################################################### .
#   Program/Function Name: ReplaceTagsInFile
#   Author: J. Kyle Wathen
#   Description: This function is used to replace {{tags}} in template files.
#   Change History:
#   Last Modified Date: 12/21/2023
#################################################################################################### .
#' @name ReplaceTagsInFile
#' @title Replace Tags in a File
#'
#' @description This function replaces {{tags}} in template files with corresponding values.
#'
#' @param strFileName The name of the file to use as input. Tags, defined by {{tags}}, will be replaced with the corresponding values.
#' @param vTags A vector of tag names, e.g., FUNCTION_NAME, VARIABLE_NAME, that will be replaced with the values in vReplace.
#' @param vReplace A vector of values to replace the tags with.
#' @return A logical value (TRUE/FALSE) indicating whether the function was successful.
#' @examples
#' \dontrun{
#' vTags    <- c("FUNCTION_NAME", "CREATION_DATE")
#' vReplace <- c(strNewFunctionName, strToday)
#' strFileName <- "MyTemplate.R" # A file that contains {{FUNCTION_NAME}} and {{CREATION_DATE}}
#' ReplaceTagsInFile(strFileName, vTags, vReplace)
#' }
#' @export
#################################################################################################### .

ReplaceTagsInFile <- function( strFileName, vTags, vReplace )
{
    bFileExists     <- file.exists( strFileName )
    if( bFileExists )
    {
        strInput <- readLines( strFileName )
        lData    <- list()
        nQtyTags <- length( vTags )
        for( iTag in 1:nQtyTags )
        {
            lData[[vTags[ iTag ]]] <- vReplace[ iTag ]
        }
        
        strRet  <- WhiskerKeepUnrender( strInput, lData )
        writeLines( strRet, con = strFileName )
        
    }
    
    return( bFileExists )
    
}
