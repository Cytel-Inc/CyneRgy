#################################################################################################### .
#' @name WhiskerKeepUnrender
#' @title Render Template While Preserving Unmatched Tags
#'
#' @description
#' This function is used internally. 
#' It renders a template string using the `whisker` package, but preserves any tags (`{{tag}}`)
#' that do not have a matching key in the provided list. It does so by first augmenting the list with any
#' missing tags using their unrendered form, then calling `whisker.render()`.
#'
#' @param sTemplate A string template containing `{{tag}}` placeholders.
#' @param lList A named list of tag-value pairs to use in rendering. Tags not found in this list will be preserved in unrendered form.
#'
#' @return A rendered string with all known tags replaced, and unknown tags kept as `{{tag}}`.
#'
#' @seealso \code{\link{AddUnrenderToList}}, \code{\link[whisker]{whisker.render}}
#' @export
#################################################################################################### .


WhiskerKeepUnrender <- function(sTemplate, lList)
{
    lExList  <- AddUnrenderToList(sTemplate, lList)
    sRes     <- whisker::whisker.render(sTemplate, lExList)
    return (sRes)
}