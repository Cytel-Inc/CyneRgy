#' @name RunExample
#' @title RunExample
#' @description {This function is used to open the examples included in this package.  
#'   For a current list of example call
#'   CyneRgy::RunExample() and you will get a list of the available examples.
#'
#'   This function opens the example in R Studio. 
#'   }
#' @examples \dontrun{{CyneRgy::RunExample( "TreatmentSelection" )
#' }}
#' @export
RunExample <- function(example) {
    strPackage <- "CyneRgy"
    # locate all the examples that exist  call using runExample("myapp")
    validExamples <- list.files(system.file("Examples", package = strPackage))

    validExamplesMsg <-
        paste0(
            "Valid examples are: '",
            paste(validExamples, collapse = "', '"),
            "'")

    # if an invalid example is given, throw an error
    if (missing(example) || !nzchar(example) ||  !example %in% validExamples) {
        print( paste0( 
            'Please run `RunExample()` with a valid example app as an argument.',
            validExamplesMsg ))
    
    }
    else
    {
        # find and launch the app
        
        strRStudioProjFile <- system.file("Examples", example,"TreatmentSelection.Rproj", package = strPackage)
        if (rstudioapi::isAvailable()) {
            rstudioapi::openProject(strRStudioProjFile, newSession = TRUE)
        } else {
            warning("RStudio API is not available. Please open the project manually.")
        }
    }
}
