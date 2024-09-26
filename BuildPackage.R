######################################################################################################################## .
# This document is used to add new functions and corresponding tests to the CyneRgy package ####
######################################################################################################################## .



library( PREP ) # See https://biopharmsoftgrp.github.io/PREP/
library( tools)

######################################################################################################################## .
# This loop can be helpful if you have exiting R files you want to add, just add them to a directory called        ####
# AddUsingPREP and the loop below will add them and the corresponding tests using PREP
######################################################################################################################## .
vFilesToAdd <- tools::file_path_sans_ext( dir( "AddUsingPREP") )

for( iFile in 1:length( vFilesToAdd ) )
{
    PREP::AddFunctionToPkg( vFilesToAdd[ iFile ], "" )    # This will create the file and corresponding test file
    file.copy( paste0( "AddUsingPREP/", vFilesToAdd[ iFile ], ".R"), paste0( "R/", vFilesToAdd[ iFile ], ".R" ), overwrite = TRUE )
    
}

######################################################################################################################## .
# Adding the logo.png ####
######################################################################################################################## .
library( usethis)
library( pkgdown)
usethis::use_logo( "NewCyneRgyLogoDark.png")
pkgdown::build_favicons( overwrite = TRUE)


######################################################################################################################## .
# Add new functions here ####
######################################################################################################################## .

PREP::AddFunctionToPkg("CreateCyneRgyFuntion", "This function will create a new file containing the template for the desired CyneRgy function.")

PREP::AddFunctionToPkg("ReplaceTagsInFile", "This function is used to replace {{tags}} in template files.")


PREP::AddFunctionToPkg("CombineAllRFiles", "This function is used to combine all .R files in a directory into a single file for use in Cytel products. ")

PREP::AddFunctionToPkg( "GetDecision", "This function takes a string for the desired decision, design and look info and return the correct decision value. ")
######################################################################################################################## .
# Adding a new Example ####
######################################################################################################################## .

To add an exmaple:
    1. create the folder
2. Write the example 
3. Add a file similar to the the ones int the vignettes that references the example
4. Update the _pkgdown.yml file 


######################################################################################################################## .
# Build package and website ####
######################################################################################################################## .


devtools::document(roclets = c('rd', 'collate', 'namespace', 'vignette'))
devtools::build()
devtools::install()
pkgdown::build_site()



######################################################################################################################## .
# Add examples  ####
######################################################################################################################## .
library( CyneRgy )
setwd( "./inst/Examples")
CyneRgy::CreateCyneRgyExample( strFunctionType = "SimulatePatientOutcome.Continuous", strNewExampleName = "ChildhoodAnxiety" )
