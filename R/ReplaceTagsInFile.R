#################################################################################################### .
#   Program/Function Name: ReplaceTagsInFile
#   Author: J. Kyle Wathen
#   Description: This function is used to replace {{tags}} in template files.
#   Change History:
#   Last Modified Date: 12/21/2023
#################################################################################################### .
#' Replace tags in a file.  
#' @param strFileName The name of the file to use as input.  In tags, defined by {{tags}}, will be replaced with the corresponding values.
#' @param vTags Vector of tag names, eg FUNCTION_NAME, VARIABLE_NAME that will be replaced with the values in vReplace
#' @param vreplace Vector of values to replace the tags with.
#' @return A TRUE/FALSE value if the functions was successful. 
#' @description { Description: This function is used to replace {{tags}} in template files. }
#' @example 
#' \dontrun{
#' vTags    <- c("FUNCTION_NAME",  "CREATION_DATE")
#' vReplace <- c(strNewFunctionName, strToday)
#' strFileName <- "MyTemplate.R" # A file that contains {{FUNCTION_NAME}} and {{CREATION_DATE}}
#' ReplaceTagsInFile( strFileName, vTags, vReplace )}
ReplaceTagsInFile <- function( strFileName, vTags, vReplace )
{
    bFileExists     <- file.exists( strFileName )
    if( bFileExists )
    {
        strInput <- readLines( strFileName )
        lData    <- list()
        nQtyTags <- length(vTags)
        for( iTag in 1:nQtyTags )
        {
            lData[[vTags[ iTag ]]] <- vReplace[ iTag ]
        }
        
        strRet  <- WhiskerKeepUnrender(strInput, lData)
        writeLines( strRet, con = strFileName )
        
    }
    
    return( bFileExists )
    
}
