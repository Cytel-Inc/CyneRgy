######################################################################################################################## .
#' @name AnalyzeUsingBayesianNormals
#' @title Analyze continuous outcomes using Bayesian normal models
#' @description Compute posterior normal parameters and apply interim predictive-probability or final
#' posterior-probability decision rules for a two-arm continuous-outcome trial.
#' @author J. Kyle Wathen and Laurent Spiess
#' @param SimData Data frame which consists of data generated in current simulation.
#' @param DesignParam List of Design and Simulation Parameters required to perform analysis.
#' @param LookInfo List containing Design and Simulation Parameters, which might be required to perform analysis.
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#' Note: UserParam values should be referenced in the main function before
#' being passed to helper functions. Passing UserParam directly to a helper
#' may prevent East Horizon from automatically populating the required parameters.
#' If UserParam is supplied, the list must contain the following named elements:
#' \describe{
#'    \item{UserParam$dPriorMeanCtrl}{Prior mean for control (Ctrl) used in analysis.}
#'    \item{UserParam$dPriorStdDevCtrl}{Prior standard deviation for control (Ctrl) used in analysis}
#'    \item{UserParam$dPriorMeanExp}{Prior mean for experimental (Exp) used in analysis.}
#'    \item{UserParam$dPriorStdDevExp}{Prior standard deviation for experimental (Exp) used in analysis}
#'    \item{UserParam$dSigma}{The known sampling variance.  Note, make sure this is the same as the sampling varaince in East Horizon.}
#'    \item{UserParam$dMAV}{Minimum Acceptable Value (MAV)}
#'    \item{UserParam$dPU}{A value in [0, 1] that specifies the upper cuttoff for efficacy.  If posterior probability is greater than PU a Go decision is made.}
#'    \item{UserParam$dPUFutility}{A value in [0, 1] that specifies the threshold probability of futility stopping. If the predictive probability of a No Go decision at the end exceeds this value, the trial is stopped early for futility.}
#'    }
#' @return A named list containing the decision, error code, posterior probability, true treatment effect,
#' posterior parameters, observed arm means, and simulated arm means.
######################################################################################################################## .

AnalyzeUsingBayesianNormals <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
    bInterimAnalysis <- FALSE   # Assuming a fixed design, the next if statement will check this

    if( missing( LookInfo ) == FALSE && !is.null( LookInfo ) )
    {
        # Step 1 - If this is the IA then subset the data to include only those for the first look. East Horizon sends all simulated data

        if( LookInfo$CurrLookIndex == 1 )
        {
            bInterimAnalysis <- TRUE
            SimData <- SimData[ 1:LookInfo$CumCompleters[ LookInfo$CurrLookIndex ], ]
        }

    }
    # Set default values
    nError              <- 0
    nDecision          <- 0

    # Extract UserParam values so East Horizon can identify required parameters; passing UserParam directly to a helper
    # may prevent East Horizon from automatically populating the required parameters.
    dSigma           <- UserParam$dSigma
    dPriorStdDevExp  <- UserParam$dPriorStdDevExp
    dPriorStdDevCtrl <- UserParam$dPriorStdDevCtrl
    dPriorMeanExp    <- UserParam$dPriorMeanExp
    dPriorMeanCtrl   <- UserParam$dPriorMeanCtrl

    lPostParams <- ComputePosteriorParametersNormal(
        SimData$Response[ SimData$TreatmentID == 0 ],
        SimData$Response[ SimData$TreatmentID == 1 ],
        dSigma,
        dPriorStdDevCtrl,
        dPriorStdDevExp,
        dPriorMeanCtrl,
        dPriorMeanExp
    )

    # Step 2 - Compute the posterior parameters for each treatment - Need to update the prior

    if( bInterimAnalysis )
    {
        # Currently at the interim analysis
        # Step 3.1 - If we are at the interim then we need to check futility.
        #            If the probability that we will conclude the trial with a No Go is large, the trial stops for futility

        #Need to compute the probability of a No GO at the end given the current data.  This requires simulating the remainder of the trial
        nQtyRepsPP       <- 5000
        vPostMeanCtrl    <- rnorm( nQtyRepsPP, lPostParams$dPostMeanCtrl, sqrt( lPostParams$dPostVarCtrl ) )
        vPostMeanExp     <- rnorm( nQtyRepsPP, lPostParams$dPostMeanExp, sqrt( lPostParams$dPostVarExp ) )

        # Set variables to track the predictive probability
        nQtyFutility     <- 0
        vCurrentExpPats  <- SimData$Response[ SimData$TreatmentID == 1 ]
        vCurrentCtrlPats <- SimData$Response[ SimData$TreatmentID == 0 ]

        # Loop to simulate the remainder of the trial using the sampled vPiC and vPiE
        # At the end of the study run the analysis using the current patients and the future patients.
        nQtyFuturePatients <- LookInfo$CumCompleters[ 2 ] - LookInfo$CumCompleters[ 1 ]
        nQtyFuturePatientsPerArm <- nQtyFuturePatients / 2
        for( i in 1:nQtyRepsPP )
        {
            # Futility Check - Step 1, simulate the remaining patients in the trial ####
            # Simulate the future data based on post samples and combine with current data at the interim.
            vExpPats  <- c( vCurrentExpPats, rnorm( nQtyFuturePatientsPerArm, vPostMeanExp[ i ], dSigma ) )
            vCtrlPats <- c( vCurrentCtrlPats, rnorm( nQtyFuturePatientsPerArm, vPostMeanCtrl[ i ], dSigma ) )

            # Futility Check - Step 2, Compute the posterior parameters for this trial ####
            lPostParamsAtTrialEnd <- ComputePosteriorParametersNormal(
                vCtrlPats,
                vExpPats,
                dSigma,
                dPriorStdDevCtrl,
                dPriorStdDevExp,
                dPriorMeanCtrl,
                dPriorMeanExp
            )

            # Futility Check - Step 3 - Final Analysis, of this trial, need to sample the posterior distribution of each treatment ####
            vMeanCtrl<- rnorm( 10000, lPostParamsAtTrialEnd$dPostMeanCtrl, sqrt( lPostParamsAtTrialEnd$dPostVarCtrl ) )
            vMeanExp <- rnorm( 10000, lPostParamsAtTrialEnd$dPostMeanExp, sqrt( lPostParamsAtTrialEnd$dPostVarExp ) )

            # Compute the posterior probability that the treatment effect is above 0.8
            # dPostProbGrt = Pr( pi_E - pi_C > 0.8 | Data )
            dPostProbGrt <- mean( ifelse( vMeanExp - vMeanCtrl > UserParam$dMAV, 1, 0 ) )

            #Note: At this point we have sampled the posterior at the end of the trial 10,000 times.   If it is close to the boundarly then we
            #      want to sample more.  If it is 10% less than the boundary then we can conclude futility.  This is just to speed up computations
            #      and avoid larger posterior samples in clear cases
            if( dPostProbGrt <  UserParam$dPU +0.05 & dPostProbGrt >=  UserParam$dPU - 0.1 )  # The trial concluded futility
            {
                # Close to the boundary, want a more accurate estimate, sample more
                vMeanCtrl <- c( vMeanCtrl, rnorm( 40000, lPostParamsAtTrialEnd$dPostMeanCtrl, sqrt( lPostParamsAtTrialEnd$dPostVarCtrl ) ) )
                vMeanExp  <- c( vMeanExp, rnorm( 40000, lPostParamsAtTrialEnd$dPostMeanExp, sqrt( lPostParamsAtTrialEnd$dPostVarExp ) ) )

                dPostProbGrt <- mean( ifelse( vMeanExp - vMeanCtrl > UserParam$dMAV, 1, 0 ) )
                if( dPostProbGrt < UserParam$dPU )
                    nQtyFutility <- nQtyFutility + 1
            }
            else if( dPostProbGrt <  UserParam$dPU - 0.1 )
            {
                # Futility reached, don't need
                nQtyFutility <- nQtyFutility + 1
            }

            # As an alternative, one could compute the lower bound of the CI at a confidence limit = 0.6 and it would be very similar, but much faster,
            # than the Bayesian analysis
            # The test at the end is frequentist and a Go decision is made if the lower limit of the confidence interval is
            # greater than 0.8, otherwise a No Go is made
            # ttest       <- t.test( vExpPats, vStdPats, conf.level = 0.6 )
            # dLowerLimit <- ttest$conf.int[1]
            #if( dLowerLimit < 0.8 )  # This would be a No Go
            #    nQtyFutility <- nQtyFutility + 1
        }

        dProbStopAtEnd <- nQtyFutility / nQtyRepsPP
        if( dProbStopAtEnd > UserParam$dPUFutility ) # Futility
            nDecision <- 3
        else
            nDecision <- 0

        dPostProbGrt <-dProbStopAtEnd
    }
    else
    {
        # Step 3.1 - Final Analysis - Need to sample the posterior distribution of each treatment
        vMeanCtrl <- rnorm( 50000, lPostParams$dPostMeanCtrl, sqrt( lPostParams$dPostVarCtrl ) )
        vMeanExp  <- rnorm( 50000, lPostParams$dPostMeanExp, sqrt( lPostParams$dPostVarExp ) )

        # Compute the posterior probability that the treatment effect is above 0.8
        # dPostProbGrt = Pr( pi_E - pi_C > 0.8 | Data )
        dPostProbGrt <- mean( ifelse( vMeanExp - vMeanCtrl > UserParam$dMAV, 1, 0 ) )

        # Step 4 - If the posterior probability is greater than 80% --> Go Decision, otherwise No Go Decision (eg futility)
        if( dPostProbGrt > UserParam$dPU )
            nDecision <- 2
        else
            nDecision <- 3
    }

    # Note: the SimData$vTrueDelta vector was added to the SimData via the return in the SimulatePateintOutcomeNormalAssurance

    lReturn <- list( Decision = as.integer( nDecision ),
                    ErrorCode = as.integer( nError ),
                    PostProb = dPostProbGrt,
                    Delta      = as.double( SimData$vTrueDelta[ 1 ] ), # This is needed for true value plots in East Horizon
                    dTrueDelta = as.double( SimData$vTrueDelta[ 1 ] ),
                    dCtrlPostMean = as.double( lPostParams$dPostMeanCtrl ),
                    dCtrlPostVar = as.double( lPostParams$dPostVarCtrl ),
                    dExpPostMean = as.double( lPostParams$dPostMeanExp ),
                    dExpPostVar = as.double( lPostParams$dPostVarExp ),
                    dObsMeanCtrl = as.double( mean( SimData$Response[ SimData$TreatmentID == 0 ] ) ),
                    dObsMeanExp = as.double( mean( SimData$Response[ SimData$TreatmentID == 1 ] ) ),
                    dSimMeanCtrl = as.double( SimData$dSimMeanCtrl[ 1 ] ),
                    dSimMeanExp = as.double( SimData$dSimMeanExp[ 1 ] ) )

    return( lReturn )
}

# Helper function to compute the posterior parameters ####
#' @name ComputePosteriorParametersNormal
#' @title Compute normal posterior parameters
#' @description Compute posterior means and variances for control and experimental arms under independent normal priors.
#' @author J. Kyle Wathen and Laurent Spiess
#' @param vCtrlData Vector of data for the Control treatment
#' @param vExpData Vector of data for the experimental treatment
#' @param dSigma Known sampling variance
#' @param dPriorStdDevCtrl Prior standard deviation for control
#' @param dPriorStdDevExp Prior standard deviation for experimental
#' @param dPriorMeanCtrl Prior mean for control
#' @param dPriorMeanExp Prior mean for experimental
#' Note: Passing UserParam directly to a helper may prevent East Horizon from automatically populating the required parameters.
#' @return A named list containing posterior means and variances for the control and experimental arms.
ComputePosteriorParametersNormal <- function( vCtrlData, vExpData, dSigma, dPriorStdDevCtrl, dPriorStdDevExp, dPriorMeanCtrl, dPriorMeanExp )
{
    # Compute the posterior parameters for the Std treatment
    dObsMeanCtrl  <- mean( vCtrlData )
    nQtyPatsCtrl  <- length( vCtrlData )
    # Posterior precision = 1/variance
    dPostPrecCtrl <- ( 1 / dPriorStdDevCtrl ^ 2 + nQtyPatsCtrl / dSigma ^ 2 )
    dPostMeanCtrl <- ( dPriorMeanCtrl / dPriorStdDevCtrl ^ 2 + dObsMeanCtrl * nQtyPatsCtrl / dSigma ^ 2 ) / dPostPrecCtrl
    dPostVarCtrl  <- 1 / dPostPrecCtrl

    # Compute the posterior parameters for the Exp treatment
    dObsMeanExp  <- mean( vExpData )
    nQtyPatsExp  <- length( vExpData )
    # Posterior precision = 1/variance
    dPostPrecExp <- ( 1 / dPriorStdDevExp ^ 2 + nQtyPatsExp / dSigma ^ 2 )
    dPostMeanExp <- ( dPriorMeanExp / dPriorStdDevExp ^ 2 + dObsMeanExp * nQtyPatsExp / dSigma ^ 2 ) / dPostPrecExp
    dPostVarExp  <- 1 / dPostPrecExp

    lPostParams <- list( dPostMeanCtrl = dPostMeanCtrl,
                         dPostVarCtrl  = dPostVarCtrl,
                         dPostMeanExp  = dPostMeanExp,
                         dPostVarExp   = dPostVarExp )
    return( lPostParams )
}
