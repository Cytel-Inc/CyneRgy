#################################################################################################### .
#   Program/Function Name: ReplaceTagsInFile
#   Author: J. Kyle Wathen
#   Description: This function is used to replace {{tags}} in template files.
#   Change History:
#   Last Modified Date: 12/21/2023
#################################################################################################### .
#' @name ReplaceTagsInFile
#' @title ReplaceTagsInFile
#' @description { Description: This function is used to replace {{tags}} in template files. }
#' @export
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
