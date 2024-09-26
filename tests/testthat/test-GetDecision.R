#################################################################################################### .
#   Program/Function Name:
#   Author: Author Name
#   Description: Test file for GetDecision
#   Change History:
#   Last Modified Date: 08/21/2024
#################################################################################################### .


context( "GetDecision")

test_that("Test- GetDecision", {
    
    ######################################################################################################################## .
    # Test for Right Tailed Tests ####
    ######################################################################################################################## .
    # Efficacy only design (RejType = 0), right tailed ####
    DesignParam <- list( TailType = 1)
    LookInfo    <- list( RejType = 0, CurrLookIndex = 1, NumLooks = 2 )
    nRet        <- GetDecision( "Efficacy", DesignParam, LookInfo )
    nExp        <- 2
    expect_equal(  nRet, nExp, label = "Right sided test(RejType = 0), Design Type: Efficacy Only; At IA Efficacy Decision - Test Failed"  )
    
    nRet        <- GetDecision( "Continue", DesignParam, LookInfo )
    nExp        <- 0
    expect_equal(  nRet, nExp, label = "Right sided test(RejType = 0), Design Type: Efficacy Only; At IA Continue Decision - Test Failed"  )
    
    LookInfo$CurrLookIndex <- 2
    nRet        <- GetDecision( "Efficacy", DesignParam, LookInfo )
    nExp        <- 2
    expect_equal(  nRet, nExp, label = "Right sided test(RejType = 0), Design Type: Efficacy Only; At FA Efficacy Decision - Test Failed"  )
    
    nRet        <- GetDecision( "Futility", DesignParam, LookInfo )
    nExp        <- 0
    expect_equal(  nRet, nExp, label = "Right sided test(RejType = 0), Design Type: Efficacy Only; At FA Efficacy Decision - Test Failed"  )
    
    
    
    
    # Efficacy only design (RejType = 2), right tailed ####
    DesignParam <- list( TailType = 1)
    LookInfo    <- list( RejType = 2, CurrLookIndex = 1, NumLooks = 2 )
    nRet        <- GetDecision( "Efficacy", DesignParam, LookInfo )
    nExp        <- 2
    expect_equal(  nRet, nExp, label = "Right sided test (RejType = 2), Design Type: Efficacy Only; At IA Efficacy Decision - Test Failed"  )
    
    nRet        <- GetDecision( "Continue", DesignParam, LookInfo )
    nExp        <- 0
    expect_equal(  nRet, nExp, label = "Right sided test(RejType = 2), Design Type: Efficacy Only; At IA Continue Decision - Test Failed"  )
    
    LookInfo$CurrLookIndex <- 2
    nRet        <- GetDecision( "Efficacy", DesignParam, LookInfo )
    nExp        <- 2
    expect_equal(  nRet, nExp, label = "Right sided test(RejType = 2), Design Type: Efficacy Only; At FA Efficacy Decision - Test Failed"  )
    
    nRet        <- GetDecision( "Futility", DesignParam, LookInfo )
    nExp        <- 0
    expect_equal(  nRet, nExp, label = "Right sided test(RejType = 2), Design Type: Efficacy Only; At FA Efficacy Decision - Test Failed"  )
    
    
    
    # Futility only design (RejType = 1), right tailed ####
    DesignParam <- list( TailType = 1)
    LookInfo    <- list( RejType = 1, CurrLookIndex = 1, NumLooks = 2 )
    nRet        <- GetDecision( "Futility", DesignParam, LookInfo )
    nExp        <- 3
    expect_equal(  nRet, nExp, label = "Right sided test(RejType = 1), Design Type: Futility Only; At IA Futility Decision - Test Failed"  )
    
    nRet        <- GetDecision( "Continue", DesignParam, LookInfo )
    nExp        <- 0
    expect_equal(  nRet, nExp, label = "Right sided test(RejType = 1), Design Type: Futility Only; At IA Continue Decision - Test Failed"  )
    
    LookInfo$CurrLookIndex <- 2
    nRet        <- GetDecision( "Futility", DesignParam, LookInfo )
    nExp        <- 3
    expect_equal(  nRet, nExp, label = "Right sided test(RejType = 1), Design Type: Futility Only; At FA Futility Decision - Test Failed"  )
    
    nRet        <- GetDecision( "Efficacy", DesignParam, LookInfo )
    nExp        <- 0
    expect_equal(  nRet, nExp, label = "Right sided test(RejType = 1), Design Type: Futility Only; At FA Efficacy Decision - Test Failed"  )
    
    
    
    
    # Futility only design (RejType = 3), right tailed ####
    DesignParam <- list( TailType = 1)
    LookInfo    <- list( RejType = 3, CurrLookIndex = 1, NumLooks = 2 )
    nRet        <- GetDecision( "Futility", DesignParam, LookInfo )
    nExp        <- 3
    expect_equal(  nRet, nExp, label = "Right sided test(RejType = 3), Design Type: Futility Only; At IA Futility Decision - Test Failed"  )
    
    nRet        <- GetDecision( "Continue", DesignParam, LookInfo )
    nExp        <- 0
    expect_equal(  nRet, nExp, label = "Right sided test(RejType = 3), Design Type: Futility Only; At IA Continue Decision - Test Failed"  )
    
    LookInfo$CurrLookIndex <- 2
    nRet        <- GetDecision( "Futility", DesignParam, LookInfo )
    nExp        <- 3
    expect_equal(  nRet, nExp, label = "Right sided test(RejType = 3), Design Type: Futility Only; At FA Futility Decision - Test Failed"  )
    
    nRet        <- GetDecision( "Efficacy", DesignParam, LookInfo )
    nExp        <- 0
    expect_equal(  nRet, nExp, label = "Right sided test(RejType = 3), Design Type: Futility Only; At FA Efficacy Decision - Test Failed"  )
    
    
    
    # Efficacy Futility design (RejType = 4), right tailed ####
    DesignParam <- list( TailType = 1)
    LookInfo    <- list( RejType = 4, CurrLookIndex = 1, NumLooks = 2 )
    nRet        <- GetDecision( "Futility", DesignParam, LookInfo )
    nExp        <- 3
    expect_equal(  nRet, nExp, label = "Right sided test(RejType = 4), Design Type: Efficacy Futility; At IA Futility Decision - Test Failed"  )
    
    nRet        <- GetDecision( "Continue", DesignParam, LookInfo )
    nExp        <- 0
    expect_equal(  nRet, nExp, label = "Right sided test(RejType = 4), Design Type: Efficacy Futility; At IA Continue Decision - Test Failed"  )
    
    nRet        <- GetDecision( "Efficacy", DesignParam, LookInfo )
    nExp        <- 2
    expect_equal(  nRet, nExp, label = "Right sided test(RejType = 4), Design Type: Efficacy Futility; At IA Efficacy Decision - Test Failed"  )
    
    
    LookInfo$CurrLookIndex <- 2
    nRet        <- GetDecision( "Futility", DesignParam, LookInfo )
    nExp        <- 3
    expect_equal(  nRet, nExp, label = "Right sided test(RejType = 4), Design Type: Efficacy Futility; At FA Futility Decision - Test Failed"  )
    
    nRet        <- GetDecision( "Efficacy", DesignParam, LookInfo )
    nExp        <- 2
    expect_equal(  nRet, nExp, label = "Right sided test(RejType = 4), Design Type: Efficacy Futility; At FA Efficacy Decision - Test Failed"  )
    
    
    
    
    # Efficacy Futility design (RejType = 5), right tailed ####
    DesignParam <- list( TailType = 1)
    LookInfo    <- list( RejType = 5, CurrLookIndex = 1, NumLooks = 2 )
    nRet        <- GetDecision( "Futility", DesignParam, LookInfo )
    nExp        <- 3
    expect_equal(  nRet, nExp, label = "Right sided test(RejType = 5), Design Type: Efficacy Futility; At IA Futility Decision - Test Failed"  )
    
    nRet        <- GetDecision( "Continue", DesignParam, LookInfo )
    nExp        <- 0
    expect_equal(  nRet, nExp, label = "Right sided test(RejType = 5), Design Type: Efficacy Futility; At IA Continue Decision - Test Failed"  )
    
    nRet        <- GetDecision( "Efficacy", DesignParam, LookInfo )
    nExp        <- 2
    expect_equal(  nRet, nExp, label = "Right sided test(RejType = 5), Design Type: Efficacy Futility; At IA Efficacy Decision - Test Failed"  )
    
    
    LookInfo$CurrLookIndex <- 2
    nRet        <- GetDecision( "Futility", DesignParam, LookInfo )
    nExp        <- 3
    expect_equal(  nRet, nExp, label = "Right sided test(RejType = 5), Design Type: Efficacy Futility; At FA Futility Decision - Test Failed"  )
    
    nRet        <- GetDecision( "Efficacy", DesignParam, LookInfo )
    nExp        <- 2
    expect_equal(  nRet, nExp, label = "Right sided test(RejType = 5), Design Type: Efficacy Futility; At FA Efficacy Decision - Test Failed"  )
    
    
    
    ######################################################################################################################## .
    # Test for Left Tailed Tests ####
    ######################################################################################################################## .
    # Efficacy only design (RejType = 0), left tailed ####
    DesignParam <- list( TailType = 0)
    LookInfo    <- list( RejType = 0, CurrLookIndex = 1, NumLooks = 2 )
    nRet        <- GetDecision( "Efficacy", DesignParam, LookInfo )
    nExp        <- 1
    expect_equal(  nRet, nExp, label = "Left sided test(RejType = 0), Design Type: Efficacy Only; At IA Efficacy Decision - Test Failed"  )
    
    nRet        <- GetDecision( "Continue", DesignParam, LookInfo )
    nExp        <- 0
    expect_equal(  nRet, nExp, label = "Left sided test(RejType = 0), Design Type: Efficacy Only; At IA Continue Decision - Test Failed"  )
    
    LookInfo$CurrLookIndex <- 2
    nRet        <- GetDecision( "Efficacy", DesignParam, LookInfo )
    nExp        <- 1
    expect_equal(  nRet, nExp, label = "Left sided test(RejType = 0), Design Type: Efficacy Only; At FA Efficacy Decision - Test Failed"  )
    
    nRet        <- GetDecision( "Futility", DesignParam, LookInfo )
    nExp        <- 0
    expect_equal(  nRet, nExp, label = "Left sided test(RejType = 0), Design Type: Efficacy Only; At FA Efficacy Decision - Test Failed"  )
    
    
    
    
    # Efficacy only design (RejType = 2), right tailed ####
    DesignParam <- list( TailType = 0)
    LookInfo    <- list( RejType = 2, CurrLookIndex = 1, NumLooks = 2 )
    nRet        <- GetDecision( "Efficacy", DesignParam, LookInfo )
    nExp        <- 1
    expect_equal(  nRet, nExp, label = "Left sided test (RejType = 2), Design Type: Efficacy Only; At IA Efficacy Decision - Test Failed"  )
    
    nRet        <- GetDecision( "Continue", DesignParam, LookInfo )
    nExp        <- 0
    expect_equal(  nRet, nExp, label = "Left sided test(RejType = 2), Design Type: Efficacy Only; At IA Continue Decision - Test Failed"  )
    
    LookInfo$CurrLookIndex <- 2
    nRet        <- GetDecision( "Efficacy", DesignParam, LookInfo )
    nExp        <- 1
    expect_equal(  nRet, nExp, label = "Left sided test(RejType = 2), Design Type: Efficacy Only; At FA Efficacy Decision - Test Failed"  )
    
    nRet        <- GetDecision( "Futility", DesignParam, LookInfo )
    nExp        <- 0
    expect_equal(  nRet, nExp, label = "Left sided test(RejType = 2), Design Type: Efficacy Only; At FA Efficacy Decision - Test Failed"  )
    
    
    
    # Futility only design (RejType = 1), right tailed ####
    DesignParam <- list( TailType = 0)
    LookInfo    <- list( RejType = 1, CurrLookIndex = 1, NumLooks = 2 )
    nRet        <- GetDecision( "Futility", DesignParam, LookInfo )
    nExp        <- 3
    expect_equal(  nRet, nExp, label = "Left sided test(RejType = 1), Design Type: Futility Only; At IA Futility Decision - Test Failed"  )
    
    nRet        <- GetDecision( "Continue", DesignParam, LookInfo )
    nExp        <- 0
    expect_equal(  nRet, nExp, label = "Left sided test(RejType = 1), Design Type: Futility Only; At IA Continue Decision - Test Failed"  )
    
    LookInfo$CurrLookIndex <- 2
    nRet        <- GetDecision( "Futility", DesignParam, LookInfo )
    nExp        <- 3
    expect_equal(  nRet, nExp, label = "Left sided test(RejType = 1), Design Type: Futility Only; At FA Futility Decision - Test Failed"  )
    
    nRet        <- GetDecision( "Efficacy", DesignParam, LookInfo )
    nExp        <- 0
    expect_equal(  nRet, nExp, label = "Left sided test(RejType = 1), Design Type: Futility Only; At FA Efficacy Decision - Test Failed"  )
    
    
    
    
    # Futility only design (RejType = 3), right tailed ####
    DesignParam <- list( TailType = 0)
    LookInfo    <- list( RejType = 3, CurrLookIndex = 1, NumLooks = 2 )
    nRet        <- GetDecision( "Futility", DesignParam, LookInfo )
    nExp        <- 3
    expect_equal(  nRet, nExp, label = "Left sided test(RejType = 3), Design Type: Futility Only; At IA Futility Decision - Test Failed"  )
    
    nRet        <- GetDecision( "Continue", DesignParam, LookInfo )
    nExp        <- 0
    expect_equal(  nRet, nExp, label = "Left sided test(RejType = 3), Design Type: Futility Only; At IA Continue Decision - Test Failed"  )
    
    LookInfo$CurrLookIndex <- 2
    nRet        <- GetDecision( "Futility", DesignParam, LookInfo )
    nExp        <- 3
    expect_equal(  nRet, nExp, label = "Left sided test(RejType = 3), Design Type: Futility Only; At FA Futility Decision - Test Failed"  )
    
    nRet        <- GetDecision( "Efficacy", DesignParam, LookInfo )
    nExp        <- 0
    expect_equal(  nRet, nExp, label = "Left sided test(RejType = 3), Design Type: Futility Only; At FA Efficacy Decision - Test Failed"  )
    
    
    
    # Efficacy Futility design (RejType = 4), right tailed ####
    DesignParam <- list( TailType = 0)
    LookInfo    <- list( RejType = 4, CurrLookIndex = 1, NumLooks = 2 )
    nRet        <- GetDecision( "Futility", DesignParam, LookInfo )
    nExp        <- 3
    expect_equal(  nRet, nExp, label = "Left sided test(RejType = 4), Design Type: Efficacy Futility; At IA Futility Decision - Test Failed"  )
    
    nRet        <- GetDecision( "Continue", DesignParam, LookInfo )
    nExp        <- 0
    expect_equal(  nRet, nExp, label = "Left sided test(RejType = 4), Design Type: Efficacy Futility; At IA Continue Decision - Test Failed"  )
    
    nRet        <- GetDecision( "Efficacy", DesignParam, LookInfo )
    nExp        <- 1
    expect_equal(  nRet, nExp, label = "Left sided test(RejType = 4), Design Type: Efficacy Futility; At IA Efficacy Decision - Test Failed"  )
    
    
    LookInfo$CurrLookIndex <- 2
    nRet        <- GetDecision( "Futility", DesignParam, LookInfo )
    nExp        <- 3
    expect_equal(  nRet, nExp, label = "Left sided test(RejType = 4), Design Type: Efficacy Futility; At FA Futility Decision - Test Failed"  )
    
    nRet        <- GetDecision( "Efficacy", DesignParam, LookInfo )
    nExp        <- 1
    expect_equal(  nRet, nExp, label = "Left sided test(RejType = 4), Design Type: Efficacy Futility; At FA Efficacy Decision - Test Failed"  )
    
    
    
    
    # Efficacy Futility design (RejType = 5), right tailed ####
    DesignParam <- list( TailType = 0)
    LookInfo    <- list( RejType = 5, CurrLookIndex = 1, NumLooks = 2 )
    nRet        <- GetDecision( "Futility", DesignParam, LookInfo )
    nExp        <- 3
    expect_equal(  nRet, nExp, label = "Left sided test(RejType = 5), Design Type: Efficacy Futility; At IA Futility Decision - Test Failed"  )
    
    nRet        <- GetDecision( "Continue", DesignParam, LookInfo )
    nExp        <- 0
    expect_equal(  nRet, nExp, label = "Left sided test(RejType = 5), Design Type: Efficacy Futility; At IA Continue Decision - Test Failed"  )
    
    nRet        <- GetDecision( "Efficacy", DesignParam, LookInfo )
    nExp        <- 1
    expect_equal(  nRet, nExp, label = "Left sided test(RejType = 5), Design Type: Efficacy Futility; At IA Efficacy Decision - Test Failed"  )
    
    
    LookInfo$CurrLookIndex <- 2
    nRet        <- GetDecision( "Futility", DesignParam, LookInfo )
    nExp        <- 3
    expect_equal(  nRet, nExp, label = "Left sided test(RejType = 5), Design Type: Efficacy Futility; At FA Futility Decision - Test Failed"  )
    
    nRet        <- GetDecision( "Efficacy", DesignParam, LookInfo )
    nExp        <- 1
    expect_equal(  nRet, nExp, label = "Left sided test(RejType = 5), Design Type: Efficacy Futility; At FA Efficacy Decision - Test Failed"  )
    
    
})
