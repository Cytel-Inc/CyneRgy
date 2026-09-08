######################################################################################################################## .
#' @name testChildPsychology
#' @title Test the legacy ChildPsychology callback contract
#' @description Define three testthat checks for the legacy `ChildPsychology` callback with null, supplied, and
#' omitted user parameters. The callback must be available in the test environment before this file is run.
#' @author Audrey Wathen, J. Kyle Wathen
#' @return No return value. The script registers and executes testthat expectations when sourced by a test runner.
######################################################################################################################## .

testthat::context( "ChildPsychology" )

testthat::test_that( "Test- ChildPsychology",
{
    nQtyOfPatientsPerArm <- 250
    NumSub               <- 2 * nQtyOfPatientsPerArm

    TreatmentID <- c( rep( 0, nQtyOfPatientsPerArm ), rep( 1, nQtyOfPatientsPerArm ) )
    Mean        <- c( 0, 0 )
    StdDev      <- c( 1, 1 )

    # Test 1 UserParam = NULL ####
    lRet1 <- tryCatch(
        {
            ChildPsychology( NumSub, TreatmentID, Mean, StdDev, UserParam = NULL )
        },
        error = function( e )
        {
            NULL
        }
    )

    lExpRet1 <- list( TestStat = NULL, ErrorCode = 0, Decision = 0, Delta = 0 )

    testthat::expect_equal( lRet1, lExpRet1, info = "Test 1: Return list did not match" )

    # Test 2 UserParam is defined ####
    UserParam <- list( dProbOfZeroOutcomeCtrl = 0.1, dProbOfZeroOutcomeExp = 0.1 )

    lRet2 <- tryCatch(
        {
            ChildPsychology( NumSub, TreatmentID, Mean, StdDev, UserParam )
        },
        error = function( e )
        {
            NULL
        }
    )

    lExpRet2 <- list( TestStat = NULL, ErrorCode = 0, Decision = 0, Delta = 0 )

    testthat::expect_equal( lRet2, lExpRet2, info = "Test 2: Return list did not match" )

    # Test 3 omit UserParam ####
    lRet3 <- tryCatch(
        {
            ChildPsychology( NumSub, TreatmentID, Mean, StdDev )
        },
        error = function( e )
        {
            NULL
        }
    )

    lExpRet3 <- list( TestStat = NULL, ErrorCode = 0, Decision = 0, Delta = 0 )

    testthat::expect_equal( lRet3, lExpRet3, info = "Test 3: Return list did not match" )
} )
#' @title Test the legacy ChildPsychology callback contract

#' @description Define three testthat checks for the legacy `ChildPsychology` callback with null, supplied, and

#' omitted user parameters. The callback must be available in the test environment before this file is run.
