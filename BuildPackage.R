######################################################################################################################## .
# This document is used to add new functions and corresponding tests to the CyneRgy package ####
######################################################################################################################## .

library( PREP ) # See https://biopharmsoftgrp.github.io/PREP/
library( tools)
vFilesToAdd <- tools::file_path_sans_ext( dir( "AddUsingPREP") )

for( iFile in 1:length( vFilesToAdd ) )
{
    PREP::AddFunctionToPkg( vFilesToAdd[ iFile ], "" )    # This will create the file and corresponding test file
    file.copy( paste0( "AddUsingPREP/", vFilesToAdd[ iFile ], ".R"), paste0( "R/", vFilesToAdd[ iFile ], ".R" ), overwrite = TRUE )
    
}



######################################################################################################################## .
# Add new functions here ####
######################################################################################################################## .

PREP::AddFunctionToPkg("CreateCyneRgyFuntion", "This function will create a new file containing the template for the desired CyneRgy function.")


######################################################################################################################## .
# Build package and website ####
######################################################################################################################## .


devtools::document(roclets = c('rd', 'collate', 'namespace', 'vignette'))
devtools::build()
devtools::install()
pkgdown::build_site()