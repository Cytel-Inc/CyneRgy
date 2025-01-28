#################################################################################################### .
#   Program/Function Name: CheckPackageAvailability
#   Author: Anoop Rawat
#   Description: This function checks if requested packages are available for use on East Horizon.
#   Change History:
#   Last Modified Date: 01/27/2025
#################################################################################################### .
#' @name CheckPackageAvailability
#' @title CheckPackageAvailability
#' @description { Description: This function takes a vector of package names and checks their 
#' availability against a predefined list of East Horizon supported packages. It returns a list containing 
#' both available and unavailable packages as well as a message string.
#' }
#' @param vRequestedPackages A character vector containing the names of packages to check
#' @return A list containing three elements:
#'   \item{vAvailable}{Character vector of available package names}
#'   \item{vUnavailable}{Character vector of unavailable package names}
#'   \item{strMessage}{Message about unavailable packages and how to request them}
#' @export
CheckPackageAvailability <- function(vRequestedPackages)
{
    # FIXME: Define the predefined software packages with versions
    # This needs to be replaced with actual list on East Horizon
    lSoftwarePackages <- list(
        "dplyr"   = "1.1.4",
        "ggplot2" = "3.5.1",
        "tidyr"   = "1.3.0",
        "shiny"   = "1.7.4",
        "dummy" = "1.2.3"
    )
    
    # Check which packages are available and unavailable
    vAvailablePackages   <- intersect(vRequestedPackages, names(lSoftwarePackages))
    vUnavailablePackages <- setdiff(vRequestedPackages, names(lSoftwarePackages))
    
    # Create a message based on status
    strMessage <- ""
    if (length(vUnavailablePackages) > 0) {
        strMessage <- sprintf(
            "The following packages are currently not available on East Horizon: %s.\nTo request these packages, please email support@cytel.com. Please note that all package requests will be evaluated, and we may not be able to accommodate requests for packages with invalid licenses.",
            paste(vUnavailablePackages, collapse = ", ")
        )
    } else {
        strMessage <- "\nAll checked packages are available on East Horizon.\n"
    }
    
    # Only print if the function is called directly (not from another function)
    if (sys.parent() == 0) {
        cat(strMessage)
    }
    
    # Create result list
    lResult <- list(
        vAvailable   = vAvailablePackages,
        vUnavailable = vUnavailablePackages,
        strMessage   = strMessage
    )
    
    return(invisible(lResult))
}