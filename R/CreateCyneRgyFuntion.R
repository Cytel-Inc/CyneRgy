#################################################################################################### .
#   Program/Function Name: CreateCyneRgyFuntion
#   Author: J. Kyle Wathen
#   Description: This function will create a new file containing the template for the desired CyneRgy function.
#   Change History:
#   Last Modified Date: 12/19/2023
#################################################################################################### .
#' Create new CyneRgy Function using provided templates. These R function that is created can be used in connection with Cytel-R integration.
#' @description { Description: This function will create a new file containing the template for the desired CyneRgy function. }
#' @export
CreateCyneRgyFuntion <- function( strFunctionType, strNewFunctionName = "", strDirectory = NA )
{
    strNewFileExt <- ".R"
    strNewFileName <- strNewFunctionName
    
    
    #TODO: Make sure the strFunctionType is a valid type, eg PatientSimulator, Analysis ect
    # Make sure the file does not already exist
    # create it and open it
    strPackage <- "CyneRgy"
    
    # Exiting template names, remove extensions
    vValidExamples <- tools::file_path_sans_ext(list.files(system.file("Templates", package = strPackage)) )
    vValidExamplesFullPath <- list.files(system.file("Templates", package = strPackage), full.names = TRUE) 
    
    validExamplesMsg <-
        paste0(
            "Valid values for strFunctionType are: '",
            paste(vValidExamples, collapse = "', '"),
            "'")
    
    # Check if strFunctionType is a valid example
    if (!(strFunctionType %in% tools::file_path_sans_ext(basename(vValidExamples)))) {
        print( paste0( 
            'Please run `CreateCyneRgyFuntion()` with a valid strFunctionType argument name.',
            validExamplesMsg ))
        return()
    }
    
    # Find the full path of the selected example
    strSelectedExample <- vValidExamplesFullPath[grep(strFunctionType, vValidExamples)]
    
    # Check if the file already exists in the destination directory
    if (!is.na(strDirectory) && file.exists(file.path(strDirectory, basename(strSelectedExample)))) {
        stop("File already exists in the destination directory.")
    }
    
    # Determine the destination directory
    if (is.na(strDirectory)) {
        strDirectory <- getwd()  # Use the current working directory if not specified
    }
    
    # Create the full path for the new file
    strNewFilePath <- paste0(strDirectory, "/",ifelse(strNewFileName == "", basename(strSelectedExample), strNewFileName), strNewFileExt)
    
    # Check if the file name exists and if so update it
    # Create the file name
    bFileExists <- file.exists( strNewFilePath )
    # Need to find the file name since it already exists
    nIndex <- 0
    while( bFileExists )
    {
        nIndex      <- nIndex + 1
        #strFileName <- paste( strPkgDir, "/R/", strFunctionName, nIndex, ".R", sep ="" )
        strNewFilePath <- paste0(strDirectory, "/",ifelse(strNewFileName == "", basename(strSelectedExample), strNewFileName), nIndex,  strNewFileExt)
        
        bFileExists <- file.exists( strNewFilePath )
    }
    
    # Copy the file to the destination directory
    file.copy(strSelectedExample, strNewFilePath )
    
    # Print a message indicating success
    cat("File copied successfully to:", strNewFilePath, "\n")
    
    strToday           <- format(Sys.Date(), format="%m/%d/%Y")
    
    # Update the tags in the file that was copied  
    vTags    <- c("FUNCTION_NAME",  "CREATION_DATE")
    vReplace <- c(strNewFunctionName, strToday)
    ReplaceTagsInFile( strNewFilePath, vTags, vReplace )
    
    
    # Open the file in RStudio
    strIgnore <- rstudioapi::navigateToFile( strNewFilePath )
}
