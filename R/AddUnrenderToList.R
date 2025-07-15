#################################################################################################### .
#' @name AddUnrenderToList
#' @title Add Unrendered Tags to List
#'
#' @description
#' This function is used internally. 
#' It scans a template string for tags in the format `{{tag}}` and adds any tags not already present
#' in the provided list to the list, assigning their unrendered form (`{{tag}}`) as the value.
#'
#' @param sTemplate A string template that may contain tags in the form `{{tag}}`.
#' @param lList A named list of existing tag-value pairs. The function will not overwrite any existing keys.
#'
#' @return A named list with the original elements of `lList`, plus any new tags found in the template that were not already included.
#'
#' @seealso \code{\link[stringr]{str_extract_all}}, \code{\link[base]{substring}}, \code{\link[base]{names}}
#' @export
#################################################################################################### .


AddUnrenderToList <- function(sTemplate, lList)
{
    lRes     <- stringr::str_extract_all(sTemplate, "\\{\\{[^{}]*\\}\\}")
    vTags    <- lRes[[1]]
    lRetList <- lList
    
    if (length(vTags) > 0)
    {
        for( iTag in 1:length(vTags))
        {
            vNames <- names(lRetList)
            strFullTag <- vTags[iTag]
            if (nchar(strFullTag) <= 4)
            {
                next
            }
            sTag   <- substring( strFullTag, 3, nchar(vTags[iTag]) - 2)
            if (length(vNames) == 0)
            {
                lRetList[[sTag]] <- vTags[iTag]
            } else if (!sTag %in% vNames)
            {
                lRetList[[sTag]] <- vTags[iTag]
            }
        }
    }
    
    return(lRetList)
}