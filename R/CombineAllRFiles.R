#################################################################################################### .
#   Program/Function Name: CombineAllRFiles
#   Author: Author Name J. Kyle Wathen and Subhajit Sengupta
#   Description: This function is used to combine all .R files in a directory into a single file for use in Cytel products.
#   Change History:
#   Last Modified Date: 04/26/2024
#################################################################################################### .
#' @name CombineAllRFiles
#' @title Combine All R Files
#'
#' @description
#' This function combines the contents of all R files in a specified directory into one file.
#' Files are combined in alphabetical order and the output file itself is automatically excluded when it is located in the input directory.
#'
#' @param strOutFileName The name of the output file. If not provided, the function will return the combined content.
#' @param strDirectory The directory where the R files are located. Defaults to the current working directory.
#' @param strFileNameToIgnore The name, or part of the name, of any file to ignore. Defaults to `NA`.
#'
#' @return A list containing the following elements:
#'   \item{nQtyCombinedFiles}{The number of files combined.}
#'   \item{strCombinedContents}{The combined content of all the R files (only if strOutFileName is NA).}
#'   \item{strReturn}{A string summarizing the operation, including the names of the combined files.}
#'
#' @examples
#' \dontrun{
#'   result <- CombineAllRFiles(
#'       strOutFileName = "combined.R",
#'       strDirectory = "/path/to/your/directory"
#'   )
#'   print( result$strReturn )
#' }
#'
#' @seealso \code{\link[base]{list.files}}, \code{\link[base]{file}}, \code{\link[base]{readLines}}, \code{\link[base]{writeLines}}
#' @export
#################################################################################################### .

CombineAllRFiles <- function( strOutFileName = NA, strDirectory = "", strFileNameToIgnore = NA )
{
    bReturnContents <- length( strOutFileName ) == 1 && is.na( strOutFileName )
    if( !nzchar( strDirectory ) )
        strDirectory <- getwd()

    if( !dir.exists( strDirectory ) )
    {
        lReturn <- list( nQtyCombinedFiles = 0L )
        if( bReturnContents )
            lReturn$strCombinedContents <- ""
        lReturn$strReturn <- "0 files combined: the input directory does not exist."
        return( lReturn )
    }

    strDirectory <- normalizePath( strDirectory, winslash = "/", mustWork = TRUE )
    vFileList    <- sort( list.files( path = strDirectory, pattern = "\\.[Rr]$", full.names = TRUE ) )

    if( !bReturnContents )
    {
        if( length( strOutFileName ) != 1 || !nzchar( strOutFileName ) )
            stop( "strOutFileName must be a single file name or NA.", call. = FALSE )
        strOutputPath <- normalizePath( strOutFileName, winslash = "/", mustWork = FALSE )
        vInputPaths   <- normalizePath( vFileList, winslash = "/", mustWork = TRUE )
        vFileList     <- vFileList[ vInputPaths != strOutputPath ]
    }

    if( length( strFileNameToIgnore ) == 1 && !is.na( strFileNameToIgnore ) )
        vFileList <- vFileList[ !grepl( strFileNameToIgnore, basename( vFileList ), fixed = TRUE ) ]

    vCombinedLines <- character()
    vCombineFiles  <- basename( vFileList )
    for( nFileIndex in seq_along( vFileList ) )
    {
        strFileName          <- vFileList[ nFileIndex ]
        strFormattedTimeStamp <- format( file.info( strFileName )$mtime, "%Y-%m-%d %H:%M:%S" )
        vHeader              <- c( "", "##################################################################################### #",
                                   paste0( "# File ", nFileIndex, ": ", basename( strFileName ), " Timestamp: ", strFormattedTimeStamp, " ####" ),
                                   "##################################################################################### #", "" )
        vCombinedLines       <- c( vCombinedLines, vHeader, readLines( strFileName, warn = FALSE ) )
    }

    if( bReturnContents )
        lReturn <- list( nQtyCombinedFiles = length( vFileList ), strCombinedContents = paste( vCombinedLines, collapse = "\n" ) )
    else
    {
        writeLines( vCombinedLines, strOutFileName )
        lReturn <- list( nQtyCombinedFiles = length( vFileList ) )
    }

    lReturn$strReturn <- paste0( length( vFileList ), " files combined successfully:\n", paste( vCombineFiles, collapse = "\n" ) )
    return( lReturn )
}

