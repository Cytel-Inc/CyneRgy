#################################################################################################### .
#   Program/Function Name:
#   Author: Author Name
#   Description: Test file for CombineAllRFiles
#   Change History:
#   Last Modified Date: 01/26/2024
#################################################################################################### .


context( "CombineAllRFiles")

test_that("Test- CombineAllRFiles", {
  
    nExpectedRet <- 0
    lRet         <- CyneRgy::CombineAllRFiles( "UploadCytel.R", "INVALID_DIRECTORY" )
    expect_equal( lRet$nQtyCombinedFiles, 0, label = "Invalid directory test failed."  )
    
    nExpectedRet <- 10
    lRet         <- CyneRgy::CombineAllRFiles( "UploadCytel.R", "TestCombineAllRFiles")
    expect_equal( lRet$nQtyCombinedFiles, nExpectedRet, label = "Valid directory test failed."   )
    
    # Remove the created file 
    file.remove( "UploadCytel.R" )
})
