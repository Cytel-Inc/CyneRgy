#################################################################################################### .
#   Program/Function Name:
#   Author: Author Name J. Kyle Wathen
#   Description: Test file for SelectSpecifiedNumberOfExpWithHighestResponses
#   Change History:
#   Last Modified Date: 2/6/2024
#################################################################################################### .


context( "SelectSpecifiedNumberOfExpWithHighestResponses")

test_that("Test- SelectSpecifiedNumberOfExpWithHighestResponses", {
    lRetExp <- list( TreatmentID = c(3,2), AllocRation = c( 2, 1 ), ErrorCode = 0 )    
    
    UserParam   <- readRDS( "TestInputs/SelectSpecifiedNumberOfExpWithHighestResponses/UserParam.Rds")
    SimData     <- readRDS( "TestInputs/SelectSpecifiedNumberOfExpWithHighestResponses/SimData.Rds")
    DesignParam <- readRDS( "TestInputs/SelectSpecifiedNumberOfExpWithHighestResponses/DesignParam.Rds")
    LookInfo    <- readRDS( "TestInputs/SelectSpecifiedNumberOfExpWithHighestResponses/LookInfo.Rds")
    lRet        <- SelectSpecifiedNumberOfExpWithHighestResponses(SimData, DesignParam, LookInfo, UserParam )
        
    expect_equal( lRet$TreatmentID, lRetExp$TreatmentID, info = "Test 1: The test for TreatmentID failed.", label ="Test 1: The test for TreatmentID failed." )
    expect_equal( lRet$AllocRatio, lRetExp$AllocRatio, info = "Test 1: The test for AllocRatio failed.", label ="Test 1: The test for AllocRatio failed.." )
    
    # Test 2 - Make all outcomes for patients on treatment 3 a 0. ####  
    # This should make treatment 2 the best and 1 second best
    
    SimData$Response[ SimData$TreatmentID == 3] <- 0
    lRetExp <- list( TreatmentID = c(2,1), AllocRation = c( 2, 1 ), ErrorCode = 0 )    
    lRet    <- SelectSpecifiedNumberOfExpWithHighestResponses(SimData, DesignParam, LookInfo, UserParam )
    
    expect_equal( lRet$TreatmentID, lRetExp$TreatmentID, info = "Test 2: The test for TreatmentID failed.", label ="Test 2: The test for TreatmentID failed." )
    expect_equal( lRet$AllocRatio, lRetExp$AllocRatio, info = "Test 2: The test for AllocRatio failed.", label ="Test 2: The test for AllocRatio failed.." )
    
})
