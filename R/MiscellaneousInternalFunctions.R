
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

WhiskerKeepUnrender <- function(sTemplate, lList)
{
    lExList  <- AddUnrenderToList(sTemplate, lList)
    sRes     <- whisker::whisker.render(sTemplate, lExList)
    return (sRes)
}
