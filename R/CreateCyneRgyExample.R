#################################################################################################### .
#   Program/Function Name: CreateCyneRgyExample
#   Author: Subhajit Sengupta
#   Description: This function will create a new example directory with necessary files using the desired CyneRgy template.
#   Change History:
#   Last Modified Date: 03/18/2024
#################################################################################################### .
#' Create new CyneRgy example using provided templates. The directory created can be used in connection with Cytel-R integration.
#' @description { Description: This function will create a new directory containing the necessary files for the desired CyneRgy template. }
#' @export
CreateCyneRgyExample <- function( strFunctionType, strNewExampleName = "", strDirectory = NA)
{
    strNewFileExt  <- ".R"
    
    if(strNewExampleName == ""){
        stop("You need to provide an example name")
    }else{
        strNewDirName <- strNewExampleName    
    }
    
    
    #TODO: Make sure the strFunctionType is a valid type, eg GenerateArrivalTimes etc.
    # Make sure the file does not already exist
    # create it and open it
    strPackage <- "CyneRgy"
    
    # Exiting template names, remove extensions
    vValidExamples         <- tools::file_path_sans_ext(list.files(system.file("Templates", package = strPackage)) )
    vValidExamplesFullPath <- list.files(system.file("Templates", package = strPackage), full.names = TRUE) 
    
    validExamplesMsg <-
        paste0(
            "Valid values for strFunctionType are: '",
            paste(vValidExamples, collapse = "', '"),
            "'")
    
    
    # Check if strFunctionType is a valid example
    if ( missing( strFunctionType ) || !nzchar(strFunctionType) || !(strFunctionType %in% vValidExamples) ) 
    {
        print( paste0( 
            'Please run `CreateCyneRgyExample()` with a valid strFunctionType argument name.',
            validExamplesMsg ))
        return()
    }
    
    # Find the full path of the selected example
    strSelectedExample <- vValidExamplesFullPath[grep(strFunctionType, vValidExamples)]
    
    # Check if the file already exists in the destination directory
    #if (!is.na(strDirectory) && file.exists(file.path(strDirectory, basename(strSelectedExample)))) {
    #    stop("File already exists in the destination directory.")
    #}
    
    if (!is.na(strDirectory) && dir.exists(file.path(strDirectory, strNewDirName))) {
        stop("Directory already exists in the destination directory.")
    }
    
    # Determine the destination directory
    if (is.na(strDirectory)) {
        strDirectory <- getwd()  # Use the current working directory if not specified
    }
    
    strTopDirPath <- paste0(strDirectory, "/")
    # ExampleTemplate directory path
    exampleTemplateDirPath <- system.file("ExampleTemplate", package = strPackage)
    # Copy the file to the destination directory
    bSuccess <- file.copy(exampleTemplateDirPath, strTopDirPath, recursive=TRUE )
    
    #strNewDirName = ifelse(strNewDirName == "", basename(strSelectedExample), strNewFileName)
    
    if(bSuccess)
    {
        strToPath <- paste0(strTopDirPath, strNewDirName)
        bSuccess  <- file.rename(from = paste0(strTopDirPath,"ExampleTemplate"), to = strToPath )
        
        #Note: GitHub does not add blank directories and since the RCode directory is blank we need to add it here
        if( bSuccess )
            dir.create( paste0( strToPath, "/RCode"))
    }else{
        stop("Directory creation Problem!")
    }
    
    if(bSuccess){
        #Rcode 
        strRCodeFileName <- paste0(strTopDirPath, strNewDirName, "/", "RCode", "/", strNewDirName, strNewFileExt)
        bSuccess         <- file.copy(strSelectedExample, strRCodeFileName)
    }else{
        stop("File copy problem inside RCode!")
    }
    
    if(bSuccess){
        strRmdFileNameFrom <- paste0(strTopDirPath, strNewDirName, "/", "Description.Rmd")
        strRmdFileNameTo   <- paste0(strTopDirPath, strNewDirName, "/", strNewDirName, ".Rmd")
        file.rename(from = strRmdFileNameFrom, to = strRmdFileNameTo)
        
        strRprojFileNameFrom <- paste0(strTopDirPath, strNewDirName, "/", "Example.Rproj")
        strRprojFileNameTo   <- paste0(strTopDirPath, strNewDirName, "/", strNewDirName, ".Rproj")
        file.rename(from = strRprojFileNameFrom, to = strRprojFileNameTo)
    }else{
       stop("Renaming problem!") 
    }
    
    # Print a message indicating success
    cat("Directory copied successfully to:", strTopDirPath, "\n")
    
    strToday           <- format(Sys.Date(), format="%m/%d/%Y")
    
    # Update the tags in the file that was copied  
    vTags    <- c("FUNCTION_NAME",  "CREATION_DATE")
    vReplace <- c(strNewDirName, strToday)
    ReplaceTagsInFile( strRCodeFileName, vTags, vReplace )
    
    
    # Update the tags in the file that was copied  
    vTags    <- c("EXAMPLE_NAME")
    vReplace <- c(strNewDirName)
    ReplaceTagsInFile( strRmdFileNameTo, vTags, vReplace )
    
    #cat("Tag replaced!")
    cat("Example created!")
}