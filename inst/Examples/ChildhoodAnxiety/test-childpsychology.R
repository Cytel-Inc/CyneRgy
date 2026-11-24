######################################################################################################################## .
#' @name test-childpsychology
#' @title Test childhood anxiety response generators
#' @description Verify bounded responses, deterministic arm means, and missing-parameter handling for both
#'   childhood anxiety response generators.
#' @author Audrey Wathen, J. Kyle Wathen
#' @return This script creates example results or test expectations in the R session; it is not an engine
#'   integration function.
#' @details Source and test both outcome generators from this example's working directory.
######################################################################################################################## .

testthat::test_that( "Childhood anxiety generators return valid bounded responses", {
    for ( strGeneratorFile in c( "R/SimulatePatientOutcomeCHU9.R", "R/SimulatePatientOutcomeCHU9V2.R" ) ) {
        lGeneratorEnv <- new.env( )
        source( strGeneratorFile, local = lGeneratorEnv )

        nSubjects <- 40
        vTreatmentID <- rep( 0:1, each = nSubjects / 2 )
        lUserParam <- list(
            dMeanFollowUpCtrl = 25, dMeanFollowUpExp = 15,
            dStdDevFollowUpCtrl = 0, dStdDevFollowUpExp = 0,
            dMeanBaselineCtrl = 25, dMeanBaselineExp = 25,
            dStdDevBaselineCtrl = 0, dStdDevBaselineExp = 0
        )
        vMean <- if ( grepl( "V2", strGeneratorFile ) ) {
            c( 0, 10 )
        } else {
            c( 25, 25 )
        }
        lResult <- lGeneratorEnv$SimulatePatientOutcome(
            nSubjects, rep( 0, nSubjects ), vTreatmentID, vMean, c( 0, 0 ), lUserParam
        )

        testthat::expect_identical( lResult$ErrorCode, 0L )
        testthat::expect_length( lResult$Response, nSubjects )
        testthat::expect_equal( lResult$Response[ vTreatmentID == 0 ], rep( 0, nSubjects / 2 ) )
        testthat::expect_equal( lResult$Response[ vTreatmentID == 1 ], rep( 10, nSubjects / 2 ) )
        testthat::expect_true( all( lResult$Response >= -36 & lResult$Response <= 36 ) )

        lMissing <- lGeneratorEnv$SimulatePatientOutcome(
            nSubjects, rep( 0, nSubjects ), vTreatmentID, vMean, c( 0, 0 )
        )
        testthat::expect_identical( lMissing$ErrorCode, -1L )
    }
} )
