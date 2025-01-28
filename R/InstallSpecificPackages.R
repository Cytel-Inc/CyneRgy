#################################################################################################### .
#   Program/Function Name: InstallSpecificPackages
#   Author: Anoop Rawat
#   Description: This function installs the version of R packages that are available on East Horizon.
#   Change History:
#   Last Modified Date: 01/27/2025
#################################################################################################### .
#' @name InstallSpecificPackages
#' @title InstallSpecificPackages
#' @description { Description: This function takes a vector of package names and installs them with 
#' predefined versions using the remotes package. It handles package availability checking and 
#' provides feedback on installation status.
#' }
#' @param vPackagesToInstall A character vector containing the names of packages to install
#' @export
InstallSpecificPackages <- function(vPackagesToInstall)
{
    # Check and install remotes package if not available
    if (!requireNamespace("remotes", quietly = TRUE))
    {
        install.packages("remotes")
    }
    
    # FIXME: Define the predefined software packages with versions
    # This needs to be replaced with actual list on East Horizon
    lSoftwarePackages <- list(
        "dplyr"   = "1.1.3",
        "ggplot2" = "3.5.1",
        "tidyr"   = "1.3.0",
        "shiny"   = "1.7.4",
        "dummy" = "1.2.3"
    )
    
    # Check package availability
    lAvailability <- CheckPackageAvailability(vPackagesToInstall)
    
    # Initialize vectors to track installation results
    vNewlyInstalled <- character(0)
    vAlreadyInstalled <- character(0)
    vFailedInstalls <- character(0)
    
    # Install each available package with specified version
    for (strPkg in lAvailability$vAvailable)
    {
        strVersion <- lSoftwarePackages[[strPkg]]
        
        # Check if package is already installed with required version
        bPackageInstalled <- FALSE
        if (requireNamespace(strPkg, quietly = TRUE)) {
            strInstalledVersion <- as.character(packageVersion(strPkg))
            if (strInstalledVersion == strVersion) {
                vAlreadyInstalled <- c(vAlreadyInstalled, strPkg)
                bPackageInstalled <- TRUE
            }
        }
        
        # Only attempt installation if needed
        if (!bPackageInstalled) {
            tryCatch(
                {
                    remotes::install_version(strPkg, version = strVersion)
                },
                error = function(e)
                {
                    cat("Error installing", strPkg, "version", strVersion, ":", e$message, "\n")
                }
            )
            if (requireNamespace(strPkg, quietly = TRUE)) {
                strInstalledVersion <- as.character(packageVersion(strPkg))
                if (strInstalledVersion == strVersion) {
                    # If the correct version of the package is installed
                    vNewlyInstalled <- c(vNewlyInstalled, strPkg)
                } else {
                    # If the package was already installed but the installation of correct version failed
                    vFailedInstalls <- c(vFailedInstalls, strPkg)
                }
            } else {
                # If the package was not installed and the installation attempt failed
                vFailedInstalls <- c(vFailedInstalls, strPkg)
            }
        }
    }
    
    # Prepare comprehensive installation report
    cat("\nInstallation Summary:\n")
    if (length(vNewlyInstalled) > 0) {
        cat(sprintf("Newly installed packages: %s\n", 
                    paste(vNewlyInstalled, collapse = ", ")))
    }
    if (length(vAlreadyInstalled) > 0) {
        cat(sprintf("Already installed packages with correct versions: %s\n", 
                    paste(vAlreadyInstalled, collapse = ", ")))
    }
    if (length(vFailedInstalls) > 0) {
        cat(sprintf("Failed installations: %s\n", 
                    paste(vFailedInstalls, collapse = ", ")))
    }
    
    # Print message for unavailable packages
    if (length(lAvailability$vUnavailable) > 0) {
        cat(lAvailability$strMessage)
    }
    
    return(invisible(NULL))
}