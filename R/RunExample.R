#################################################################################################### .
#' @title Launch Example from the CyneRgy Package
#' 
#' @description 
#' This function opens an example included in the CyneRgy package. A new instance of RStudio will launch and open the requested example. 
#' To obtain a current list of available examples, call `CyneRgy::RunExample()`, and it will display the list.
#' 
#' @note The function requires RStudio to open the example projects automatically. If the RStudio API is unavailable, a manual process will be needed.
#' 
#' @examples 
#' \dontrun{
#' CyneRgy::RunExample("TreatmentSelection")
#' }
#' 
#' @export
#################################################################################################### .

RunExample <- function(strExample) {
    strPackage <- "CyneRgy"
    # locate all the examples that exist  call using runExample("myapp")
    validExamples <- list.files(system.file("Examples", package = strPackage))

    validExamplesMsg <-
        paste0(
            "Valid examples are: '",
            paste(validExamples, collapse = "', '"),
            "'")

    # if an invalid example is given, throw an error
    if (missing(strExample) || !nzchar(strExample) ||  !strExample %in% validExamples) {
        print( paste0( 
            'Please run `RunExample()` with a valid example app as an argument.',
            validExamplesMsg ))
    
    }
    else
    {
        # find and launch the app
        
        strRStudioProjFile <- system.file("Examples", strExample, package = strPackage)
        if (rstudioapi::isAvailable()) {
            rstudioapi::openProject(strRStudioProjFile, newSession = TRUE)
        } else {
            warning("RStudio API is not available. Please open the project manually.")
        }
    }
}
