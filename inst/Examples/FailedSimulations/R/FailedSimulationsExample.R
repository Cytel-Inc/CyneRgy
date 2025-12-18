

source( "R/SortTrialsByCode.R" )
source( "R/InternalFunction.R" )
source ( "R/BugCheck.R" )

#----------------------------------------------------------------------------- -
# Before running the following code below complete the following steps:
#  1. Create an output folder
#  2. Extract the failed test results
#  3. Copy the path of the extracted folder into the strInputRoot variable
#  4. Repeat this for your output folder into strOutputRoot
#  5. Replace all "\" with "/"
#  6. Once these steps are completed, the code is ready to run
#----------------------------------------------------------------------------- -


# ----------------------------- RDS Sorter -----------------------------
SortTrialsByRCode( 
        strInputRoot      = "FailedRDSResults",                 
        strOutputRoot     = "SortedRDS",
        bRecursiveTrials = TRUE,   
        vChooseName = c( "script" ),
        bDryRun = FALSE,
        bPurgeOutputRoot = TRUE
)

#----------------------------------------------------------------------------- -
# After this code is ran, your output folder should have some folders.
# These folders will be sorted by the names of the r files that caused
# East Horizon to crash.
# If a set of tests does not have R code attached, they will be sorted
# into a no R script folder.
#----------------------------------------------------------------------------- -

#----------------------------------------------------------------------------- -
# Steps:
# 1. Set strRds to the path of one the RDS files
#    from the previous functions output
# 2. Set strRFile the the path of the R file 
#    in the same folder at your RDS file from the previous output
# 3. Replace all "\" with "/"
# 4. The code is ready to run
#----------------------------------------------------------------------------- -


# ----------------------------- Bug Check -----------------------------
strRds   <- "SortedRDS/SimulatePatientOutcome_25-SEP-2025/SimulatePatientOutcome_25-SEP-2025.rds"
strRFile <- "SortedRDS/SimulatePatientOutcome_25-SEP-2025/SimulatePatientOutcome_25-SEP-2025.R"


lErrorMessage <- ErrorCheck( strRds, strRFile )
# lErrorMessage$call_string  # the exact call to the function used
# lErrorMessage$error        # error text if the bug trips
# lErrorMessage$value        # value if it succeeded


#----------------------------------------------------------------------------- -
# Once the code is ran, lErrorMessage will return a bunch of details 
# regarding the bugged function.
# It will return the error, call to the function used, the code of the function,
# the function name, and the function arguments.
# The commands below show the specific call to the function used,
# the error the code caused, and the value if the code worked.
#----------------------------------------------------------------------------- -
