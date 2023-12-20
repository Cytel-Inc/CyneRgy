#################################################################################################### .
#   Program/Function Name: CreateCyneRgyFuntion
#   Author: J. Kyle Wathen
#   Description: This function will create a new file containing the template for the desired CyneRgy function.
#   Change History:
#   Last Modified Date: 12/19/2023
#################################################################################################### .
#' Create New CyneRgy Function Using Templates
#' Create a new function to call for Cytel products using the correct template.
#' @name CreateCyneRgyFuntion
#' @title CreateCyneRgyFuntion
#' @description { Description: This function will create a new file containing the template for the desired CyneRgy function. }
#' @export
CreateCyneRgyFuntion <- function( strFunctionType, strNewFileName = "", strDirectory = NA )
{
    #TODO: Make sure the strFunctionType is a valid type, eg PatientSimulator, Analysis ect
    # Make sure the file does not already exist
    # create it and open it
    strPackage <- "CyneRgy"
    # Examples
    vValidExamples <- list.files(system.file("Templates", package = strPackage), full.names = TRUE)
    
    # Check if strFunctionType is a valid example
    if (!(strFunctionType %in% tools::file_path_sans_ext(basename(vValidExamples)))) {
        stop("Invalid strFunctionType. Please provide a valid example.")
    }
    
    # Find the full path of the selected example
    strSelectedExample <- vValidExamples[grep(strFunctionType, basename(vValidExamples))]
    
    # Check if the file already exists in the destination directory
    if (!is.na(strDirectory) && file.exists(file.path(strDirectory, basename(strSelectedExample)))) {
        stop("File already exists in the destination directory.")
    }
    
    # Determine the destination directory
    if (is.na(strDirectory)) {
        strDirectory <- getwd()  # Use the current working directory if not specified
    }
    
    # Create the full path for the new file
    strNewFilePath <- file.path(strDirectory, ifelse(strNewFileName == "", basename(strSelectedExample), strNewFileName))
    
    # Copy the file to the destination directory
    file.copy(strSelectedExample, strNewFilePath, overwrite = TRUE)
    
    # Print a message indicating success
    cat("File copied successfully to:", strNewFilePath, "\n")
    
    # Open the file in RStudio
    strIgnore <- rstudioapi::navigateToFile( strNewFilePath )
}
