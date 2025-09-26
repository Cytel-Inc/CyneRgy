library( ggplot2 )
library( readxl )
library( readr )
library( dplyr )
library( whisker)
library( stringr )

######################################################################################################################## .
# Follow the steps here: https://cytel-inc.github.io/CyneRgy/articles/ConsecutiveStudiesBinary.html
######################################################################################################################## .

# Step 1 - Process the East Horizon Explore results for Phase 2
# The CSV file will contain 1 row for each IA that was conducted and the FA.  However, if the trial is stopped early for efficacy or futility
# it will only contain the IAs that occur.  Therefore, we must find the last analysis for each simulated trial. 

# TO DO: Verify that the path to the results file is correct and accessible from RStudio ####
dfEastHorExp <- read_csv("Outputs/Ph2_results.csv")


# Build a dataframe with only 1 row per simulated trial with the last analysis.  The last analysis is the analysis (IA or FA) that makes a futility or efficacy decision.
dfLastAnalysisResults <-  group_by(dfEastHorExp, SimIndex) %>%
                                slice_max(AnalysisIndex) %>%
                                ungroup()

# Step 2 - The Ph3 is only conducted when the Ph2 is successful (Efficacy) so create a dataframe of the simulated trials that are successful
# Select trials that are successful so we can build posterior of true delta when a Go decision is made
dfConditionalPostOnPh2Success <- dfLastAnalysisResults[ dfLastAnalysisResults$Decision == "Efficacy", ] %>% 
                                    dplyr::select( TrueProbabilityControl,TrueProbabilityExperimental )

# Step 3 - Load the SimulatePatientOutcomeBinaryWithAssurancePh3.R file

# TO DO: Verify that the path to SimulatePatientOutcomeBinaryWithAssurancePh3.R is correct and accessible from RStudio ####
strFilePath <- "SimulatePatientOutcomeBinaryWithAssurancePh3.R"
vFileLines <- readLines(strFilePath)

# Step 4 - Build the replacement LoadData function with actual values
matPh2Success <- as.matrix(dfConditionalPostOnPh2Success)

vRows <- apply(matPh2Success, 1, function(row) paste(row, collapse = ", "))
strMatrix <- paste0("matrix(c(\n    ",
                    paste(vRows, collapse = ",\n    "),
                    "), byrow = TRUE, ncol = 2)")

vNewLoadData <- c(
    "LoadData <- function( ) {",
    paste0("  dfRetValues <- data.frame(", strMatrix, ")"),
    "  names(dfRetValues) <- c('TrueProbabilityControl','TrueProbabilityExperimental')",
    "  return(dfRetValues)",
    "}"
)

# Step 5 - Locate and replace the old LoadData definition
nStartLine <- grep("^LoadData <- function", vFileLines)
nEndLine   <- nStartLine - 1 + which(grepl("^}", vFileLines[nStartLine:length(vFileLines)]))[1]

if (nEndLine < length(vFileLines)) {
    vAfter <- vFileLines[(nEndLine+1):length(vFileLines)]
} else {
    vAfter <- character(0)
}

vFileLines <- c(
    vFileLines[1:(nStartLine-1)],
    vNewLoadData,
    vAfter
)

# Step 6 - Export and replace the SimulatePatientOutcomeBinaryWithAssurancePh3.R file
writeLines(vFileLines, strFilePath)

message("Script run successfully!")
