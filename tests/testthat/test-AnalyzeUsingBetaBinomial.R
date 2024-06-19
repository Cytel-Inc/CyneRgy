#################################################################################################### .
#   Program/Function Name:
#   Author: Author Name
#   Description: Test file for AnalyzeUsingBetaBinomial
#   Change History:
#   Last Modified Date: 01/02/2024
#################################################################################################### .


context( "AnalyzeUsingBetaBinomial")

test_that("Test- AnalyzeUsingBetaBinomial", {
    nQtyOfPatientsPerArm <- 125
    nQtyOfPatients       <- 2*nQtyOfPatientsPerArm
    
    vTreatmentID        <- c( rep(0,nQtyOfPatientsPerArm ), rep( 1, nQtyOfPatientsPerArm) )
    vPatientResponseStd <- rbinom( nQtyOfPatientsPerArm,1, 0.25 )
    vPatientResponseExp <- rbinom( nQtyOfPatientsPerArm,1, 0.5 )
    vPatientResponse    <- c( vPatientResponseStd, vPatientResponseExp )
    
    vRandomIndex        <- sample( 1:nQtyOfPatients,  size = nQtyOfPatients )
    
    vPatientResponse    <- vPatientResponse[ vRandomIndex ]
    vTreatmentID        <- vTreatmentID[ vRandomIndex ]
    
    SimData     <- list( TreatmentID = vTreatmentID, Response = vPatientResponse)
    LookInfo    <- list( NumLooks = 3, CurrLooKIndex = 1, CumCompleters = c( nQtyOfPatients/2, nQtyOfPatients ))
    DesignParam <- list( SampleSize = nQtyOfPatients, MaxCompleters = nQtyOfPatients)
    
    #lRet1    <- AnalyzeUsingBetaBinomial( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
    
    #lExpRet1 <- list( TestStat = NULL, ErrorCode = 0, Decision = 0, Delta = 0 ) 
    
    #expect_equal( lRet1, lExpRet1, info = "Test 1: Return list did not match")
    
    lRet2    <- AnalyzeUsingBetaBinomial( SimData, DesignParam, LookInfo = LookInfo, UserParam = NULL )
    
    lExpRet2 <<- list( TestStat = NULL, ErrorCode = 0, Decision = 0, Delta = 0 ) 
    
    expect_equal( lRet2, lExpRet2, info = "Test 2: Return list did not match")
    # Example test that will fail
    # nRet         <- 1
    # nExpectedRet <- 10


    # expect_equal( nRet, nExpectedRet, info = "The test failed...", label ="Test for ..." )
})
