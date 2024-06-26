
##################################################################################### #
# File 1: AnalyzeUsingBetaBinomial.R Timestamp: 2024-01-30 10:50:03 ####
##################################################################################### #


######################################################################################################################## .
#' Analyze Using Bayesian Beta-Binomial Model for Binary Data
#' Analyze for efficacy using a beta prior to compute the posterior probability that experimental is better than standard of care. 
#' @param SimData Data frame which consists of data generated in current simulation.
#' @param DesignParam List of Design and Simulation Parameters required to perform analysis.
#' @param LookInfo List containing Design and Simulation Parameters, which might be required to perform analysis.
#' @param UserParam A list of user defined parameters in East. The default must be NULL.
#' If UseParam is supplied, the list must contain the following named elements:
#' \describe{
#'    \item{UserParam$dAlphaS}{}
#'    \item{UserParam$dBetaS}{}
#'    \item{UserParam$dAlphaE}{}
#'    \item{UserParam$dBetaE}{}
#'    \item{UserParam$dMAV}{}
#'    \item{UserParam$dUpperCutoffEfficacy}{ A value (0,1) that specifies the upper cutoff for the efficacy check. Above this value will declare efficacy }
#'    \item{UserParam$dLowerCutoffForFutility}{A value (0,1) that specified the lower cutoff for the futility check. Below this value will declare futility. }
#'    }
#' @description In this version, the analysis for efficacy is to assume a beta prior to compute the posterior probability that experimental is better than standard of care.
#'              The futility is based on a Bayesian predictive probability.  
#'              The prior for the prediction and the analysis do NOT need to be the same.  
#'              This function requires more info in the glDesign than the previous AnalyzeUsingBetaBinomBayesianModel
#'              
#' @return TestStat A double value of the computed test statistic
#' @return Decision An integer value: Decision = 0 --> No boundary crossed
#'                                    Decision = 1 --> Lower Efficacy Boundary Crossed
#'                                    Decision = 2 --> Upper Efficacy Boundary Crossed
#'                                    Decision = 3 --> Futility Boundary Crossed
#'                                    Decision = 4 --> Equivalence Boundary Crossed
#' @return ErrorCode An integer value:  ErrorCode = 0 --> No Error
#                                       ErrorCode > 0 --> Non fatal error, current simulation is aborted but the next simulations will run
#                                       ErrorCode < 0 --> Fatal error, no further simulation will be attempted

#'@note In this example we assume a Bayesian model and use posterior probabilities for decision making
#       If user variables are not specified we assume non-informative uniform prior equivalent to observing two patient outcomes:
#       pi_S ~ beta( 1, 1 ); 
#       pi_E ~ beta( 1, 1 ); 
#' @note 
#' When using simulation to obtain the frequentist Operating Characteristic (OC) of a Bayesian design, you should set dLowerCutoffForFutility = 0
#' when simulating under the null case in order to obtain the false-positive rate of the non-binding futility rule.  
#' When you set dLowerCutoffForFutility > 0, simulation will provide the OC of the binding futility rule because the rule is ALWAYS followed. 
#'@note Helpful Hints:
#'       There is often info that East sends to R that are not shown in a given example.  It can be very helpful to save the input 
#'       objects and then load them into your R session and inspect them.  This can be done with the following R code in your function.
#'
#'       saveRDS( SimData,     "SimData.Rds")
#'       saveRDS( DesignParam, "DesignParam.Rds" )
#'       saveRDS( LookInfo,    "LookInfo.Rds" )
#'
#'       The above code will save each of the input objects to a file so they may be examined within R.
#' TODO(Kyle): I am not sure how to define the alpha and beta user parameters. Could you define and add to documentation?
#' @export
######################################################################################################################## .
AnalyzeUsingBetaBinomial <- function( SimData,DesignParam,LookInfo, UserParam=NULL)  
{

    # The below lines set the values of the parameters if a user does not specify a value
    AnalyzeLocalFunction <- function(  SimData, DesignParam, LookInfo, UserParam = NULL ){
        # This is a local funciton with the correct signature but is local so we cannot call if from Solara/East or C++ it is invalid
        # Do some local stuff
        return( 2 )
    }
    if( is.null( UserParam ) )
    {
        # Default values are non-informative uniform prior
        UserParam <- list(dAlphaS = 0.5, dBetaS = 0.5, 
                          dAlphaE = 0.5, dBetaE = 0.5, 
                          dMAV = 0, 
                          dUpperCutoffEfficacy = 0.975,
                          dLowerCutoffForFutility = 0.1 )
    }

    
    
    # Pull important information from the input parameters that were sent from East
    nQtyOfLooks          <- LookInfo$NumLooks
    nLookIndex           <- LookInfo$CurrLookIndex
    nQtyOfEvents         <- LookInfo$CumEvents[ nLookIndex ]
    
    nQtyOfPatsInAnalysis <- LookInfo$CumCompleters[ nLookIndex ]
    
    # Create the vector of simulated data for this IA - East sends all of the simulated data
    vPatientOutcome      <- SimData$Response[ 1:nQtyOfPatsInAnalysis ]
    vPatientTreatment    <- SimData$TreatmentID[ 1:nQtyOfPatsInAnalysis ]
    
    # Create vectors of data for each treatment 
    vOutcomesS           <- vPatientOutcome[ vPatientTreatment == 0 ]
    vOutcomesE           <- vPatientOutcome[ vPatientTreatment == 1 ]
  
    # Perform the desired analysis - for this case a Bayesian analysis.  If Posterior Probability is > Cutoff --> Efficacy ####
    # The function ProbExpGreaterStdPlusDeltaBeta is provided below in this file.
    
    lRet                 <- ProbExpGreaterStdPlusDeltaBeta( vOutcomesS, vOutcomesE, UserParam$dAlphaS, UserParam$dBetaS, UserParam$dAlphaE, UserParam$dBetaE, UserParam$dMAV )
    nDecision            <- ifelse( lRet$dPostProb > UserParam$dUpperCutoffEfficacy, 2, 0 )  # Above the cutoff --> Efficacy ( 2 is East code for Efficacy)
    
    if( nDecision == 0 )
    {
        # Did not hit efficacy, so check futility 
        # We are at the FA, efficacy decision was not made yet so the decision is futility
        if( nLookIndex == nQtyOfLooks ) 
        {
            nDecision <- 3 # East code for futility 
        }
        else 
        {
            # Check for futility ####
            if( lRet$dPostProb <  UserParam$dLowerCutoffForFutility ) # We are at the FA, efficacy decision was not made yet so the decision is futility
                nDecision <- 3 # East code for futility 
        }
        
    }
    
    if( !file.exists( paste0( "SimData", nLookIndex, ".Rds") ))
    {
        saveRDS( SimData, paste0( "SimData", nLookIndex, ".Rds") )
        saveRDS( DesignParam, paste0( "DesignParam", nLookIndex, ".Rds") )
        saveRDS( LookInfo, paste0( "LookInfo", nLookIndex, ".Rds") )
    }
    Error 	<- 0
    #retval 	= 0
    
    
    return(list(TestStat = as.double(lRet$dPostProb), ErrorCode = as.integer(Error), Decision = as.integer( nDecision ) )) }

# This function is valid and has the correct parameter signature 
Foo2 <- function( SimData,DesignParam,LookInfo, UserParam = NULL)
{
  
}

# Another test function for validation - This shoudl be a valid funciton
AnalyzeUsingBetaBinomialMultiLine <- function( SimData, 
                                        DesignParam,
                                        LookInfo, 
                                        UserParam =    NULL)  
{

}


# Another test function for validation - This shoudl be a valid funciton
AnalyzeUsingBetaBinomialMultiLine2 <- function( SimData, 
                                        DesignParam,
                                        LookInfo, 
                                        UserParam =NULL
                                        )  
{

}

# This function should be found but is INVALID because it does not have UserParam 
AnalyzeUsingBetaBinomialMultiLineInvalidParams <- function( SimData, 
                                        DesignParam,
                                        LookInfo
                                        )  
{

}

# This function is valid and has the correct parameter signature BUT the function  reports it as invalid because there is not a space in UserParam=NULL.  
# Javascript needs to be fixed, see above. 
Foo3 <- function( SimData,DesignParam,LookInfo, UserParam = NULL){  Foo<-( SimData,DesignParam,LookInfo, UserParam = NULL) {return(0) } }
Foo4 <- function( SimData,DesignParam,LookInfo, UserParam = NULL){  
    Foo<-( SimData,DesignParam,LookInfo, UserParam = NULL) {return(0) } }

# Function for performing statistical analysis using a Beta-Binomial Bayesian model

ProbExpGreaterStdPlusDeltaBeta <- function(vOutcomesS, vOutcomesE, dAlphaS, dBetaS, dAlphaE, dBetaE, dDelta = 0.0) 
{
    # In the beta-binomial model if we make the assumption that 
    # pi ~ Beta( a, b )
    # then the posterior of pi is:
    # pi | data ~ Beta( a + # success, b + # non-successes )
    
    # Compute the posterior parameters for control treatment 
    dAlphaS <- dAlphaS + sum( vOutcomesS )
    dBetaS  <- dBetaS  + length( vOutcomesS ) - sum( vOutcomesS )
    
    # Compute the posterior parameters for Exp treatment 
    dAlphaE  <- dAlphaE + sum( vOutcomesE )
    dBetaE   <- dBetaE  + length( vOutcomesE ) - sum( vOutcomesE )
    
    # There are much more efficient ways to compute this, but for simplicity, we are just sampling the posteriors
    vPiStd     <- rbeta( 10000, dAlphaS, dBetaS )
    vPiExp     <- rbeta( 10000, dAlphaE,  dBetaE  )
    dPostProb  <- ifelse( vPiExp > vPiStd + dDelta, 1, 0 )
    dPostProb  <- sum( dPostProb )/length( dPostProb )
    
    return(list(dPostProb = dPostProb))
}




# Function to compute Bayesian predictive probability of success
ComputeBayesianPredictiveProbabilityWithBayesianAnalysis <- function(dataS, dataE, priorAlphaS, priorBetaS, priorAlphaE, priorBetaE, nQtyOfPatsS, nQtyOfPatsE, nSimulations, finalBoundary, lAnalysisParams) {
    # Compute the posterior parameters based on observed data
    posteriorAlphaS <- priorAlphaS + sum(dataS)
    posteriorBetaS  <- priorBetaS + length(dataS) - sum(dataS)
    
    posteriorAlphaE <- priorAlphaE + sum(dataE)
    posteriorBetaE  <- priorBetaE + length(dataE) - sum(dataE)
    
    #lAnalysisParams <- list( dAlphaCtrl = priorAlphaS,
    #                         dBetaCtrl  = priorBetaS,
    #                         dAlphaExp  = priorAlphaE,
    #                         dBetaExp   = priorBetaE )
    
    # Initialize counters for successful trials
    successfulTrials <- 0
    
    # Simulate the remaining trials and compute the predictive probability
    for (i in 1:nSimulations) {
        # Sample response rates from posterior distributions
        posteriorRateS <- rbeta(1, posteriorAlphaS, posteriorBetaS)
        posteriorRateE <- rbeta(1, posteriorAlphaE, posteriorBetaE)
        
        # Simulate patient outcomes for for the current virtual trial based on sampled rates
        # The data at the end of the trial is a combination of the data at the interim, dataS, and the simulated data to the end of the trial, remainingDataS
        remainingDataS <- SimulatePatientOutcome(nQtyOfPatsS - length(dataS), posteriorRateS)
        combinedDataS  <- c(dataS, remainingDataS)
        
        remainingDataE <- SimulatePatientOutcome(nQtyOfPatsE - length(dataE), posteriorRateE)
        combinedDataE  <- c(dataE, remainingDataE)
        
        
        # Perform the analysis with combined data to check if the trial is successful
        result <- ProbSGreaterEBeta(combinedDataS, combinedDataE, lAnalysisParams )
        
        # Check if the result meets the cutoff for success
        if (result$dPostProb <= finalBoundary) {
            successfulTrials <- successfulTrials + 1
        }
    }
    
    # Compute the Bayesian predictive probability of success
    predictiveProbabilityS <- successfulTrials / nSimulations
    
    # Return the result
    return(list(predictiveProbabilityS = predictiveProbabilityS))
}




##################################################################################### #
# File 2: AnalyzeUsingEastManualFormula.R Timestamp: 2024-01-30 10:50:03 ####
##################################################################################### #



######################################################################################################################## .
#' @param AnalyzeUsingEastManualFormula
#' @title Compute the statistic using formula 28.2 in the East manual.
#' @param SimData Data frame which consists of data generated in current simulation.
#' @param DesignParam List of Design and Simulation Parameters required to perform analysis.
#' @param LookInfo List containing Design and Simulation Parameters, which might be required to perform analysis.
#' @param UserParam A list of user defined parameters in East. The default must be NULL. For this example, user defined parameters are not included. 
#' @description Use the formula 28.2 in the East manual to compute the statistic.  The purpose of this example is to demonstrate how the analysis and decision making can be modified in a simple approach.  
#'              The test statistic is compared to the upper boundary computed and sent by East as an input. This example does NOT include a futility rule. 
#' @return TestStat A double value of the computed test statistic
#' @return Decision An integer value: Decision = 0 --> No boundary crossed
#'                                    Decision = 1 --> Lower Efficacy Boundary Crossed
#'                                    Decision = 2 --> Upper Efficacy Boundary Crossed
#'                                    Decision = 3 --> Futility Boundary Crossed
#'                                    Decision = 4 --> Equivalence Boundary Crossed
#' @return ErrorCode An integer value:  ErrorCode = 0 --> No Error
#                                       ErrorCode > 0 --> Non fatal error, current simulation is aborted but the next simulations will run
#                                       ErrorCode < 0 --> Fatal error, no further simulation will be attempted

#'@note Helpful Hints:
#'       There is often info that East sends to R that are not shown in a given example.  It can be very helpful to save the input 
#'       objects and then load them into your R session and inspect them.  This can be done with the following R code in your function.
#'
#'       saveRDS( SimData,     "SimData.Rds")
#'       saveRDS( DesignParam, "DesignParam.Rds" )
#'       saveRDS( LookInfo,    "LookInfo.Rds" )
#'
#'       The above code will save each of the input objects to a file so they may be examined within R.
#' @export
######################################################################################################################## .

AnalyzeUsingEastManualFormula<- function(SimData, DesignParam, LookInfo, UserParam = NULL)
{
    
    # Input objects can be saved through the following lines:
    
    #setwd( "[ENTER THE DIRECTORY WHERE YOU WANT TO SAVE DATA]")
    #saveRDS( SimData, "SimData.Rds")
    #saveRDS( DesignParam, "DesignParam.Rds" )
    #saveRDS( LookInfo, "LookInfo.Rds" )
    
    
    # Retrieve necessary information from the objects East sent
    nLookIndex           <- LookInfo$CurrLookIndex
    nQtyOfEvents         <- LookInfo$CumEvents[ nLookIndex ]
    nQtyOfPatsInAnalysis <- LookInfo$CumCompleters[ nLookIndex ]
    
    # Create the vector of simulated data for this IA - East sends all of the simulated data
    vPatientOutcome      <- SimData$Response[ 1:nQtyOfPatsInAnalysis ]
    vPatientTreatment    <- SimData$TreatmentID[ 1:nQtyOfPatsInAnalysis ]
    
    # Create vectors of data for each treatment - E is Experimental and S is Standard of Care 
    vOutcomesS           <- vPatientOutcome[ vPatientTreatment == 0 ]
    vOutcomesE           <- vPatientOutcome[ vPatientTreatment == 1 ]
    
    nQtyOfResponsesOnE   <- sum( vOutcomesE )
    nQtyOfPatsOnE        <- length( vOutcomesE )
    
    nQtyOfResponsesOnS   <- sum( vOutcomesS )
    nQtyOfPatsOnS        <- length( vOutcomesS )
    
    # Compute the estimates in equation 28.2 from the East user manual
    dPiHatExperimental   <- nQtyOfResponsesOnE/nQtyOfPatsOnE
    dPiHatControl        <- nQtyOfResponsesOnS/nQtyOfPatsOnS
    
    dPiHatj              <- ( nQtyOfResponsesOnE +  nQtyOfResponsesOnS )/( nQtyOfPatsOnE + nQtyOfPatsOnS )
    
    # Equation 28.2 in East manual
    dZj                  <- ( dPiHatExperimental - dPiHatControl )/sqrt( dPiHatj*( 1- dPiHatj ) * ( 1/nQtyOfPatsOnE + 1/nQtyOfPatsOnS)  ) 
    
    # A decision of 2 means success, 0 means continue the trial
    nDecision            <- ifelse( dZj > LookInfo$EffBdryUpper[ nLookIndex], 2, 0 )  
    
    if( nDecision == 0 )
    {
        # For this example, there is NO futility check but this is left for consistency with other examples 
        
    }
    
    Error <-  0
    
    
    return(list(TestStat = as.double(dZj), ErrorCode = as.integer(Error), Decision = as.integer( nDecision ) ))
}

##################################################################################### #
# File 3: AnalyzeUsingPropLimitsOfCI.R Timestamp: 2024-01-30 10:50:03 ####
##################################################################################### #


######################################################################################################################## .
# TODO(Kyle)-Could you check that this documentation is correct (specifically the description)
#' @param AnalyzeUsingPropLimitsOfCI
#' @title Analyze using a simplified limits of confidence interval design
#' @param SimData Data frame which consists of data generated in current simulation.
#' @param DesignParam List of Design and Simulation Parameters required to perform analysis.
#' @param LookInfo List containing Design and Simulation Parameters, which might be required to perform analysis.
#' @param UserParam A list of user defined parameters in East. The default must be NULL.
#'                  If UserParam is supplied, the list must contain the following named elements:
#'                  UserParam$dLowerLimit - A value (0,1) that specifes the lower limit for the confidence interval. 
#'                  UserParam$dUpperLimit - A value (0,1) that specifies the upper limit for the confidence interval.
#'                  UserParam$dConfLevel - A value (0,1) that specifies the confidence level for the prop.test function in base R.
#' @description  In this simplified example of upper and lower confidence boundary designs, if it is likely that the treatment difference is above the Minimum Acceptable Value (MAV) then a Go decision is made.  
#'               If a Go decision is not made, then if is is unlikely that the treatment difference is above the Target Value (TV) a No Go decision is made.      
#'               In this example, the prop.test from base R is utilized to analyze the data and compute at user-specified confidence interval (dConfLevel).  
#'               We set the defauly without user-specified variables to assume the MAV = 0.1 and TV=0.2. The team would like to make a Go decision if there is at least a 90% chance that the difference in treatment is greater than the MAV.  If a Go decision is not made, then a No Go decision is made if there is less than a 10% chance the difference is greater than the TV.  
#'               Using a frequentist CI an approximation to this design can be done by the logic described below.
#'               At an Interim Analysis, If the Lower Limit of the CI, denoted by LL, is greater than user-specified dLowerLimit then a Go decision is made.  Specifically, if LL > UserParam$dLowerLimit --> Go
#'               If a Go decision is not made, then if the Upper Limit of the CI, denoted by UL, is less than user-specified dUpperLimit a No Go decision is made.  Specifically, if UL < UserParam$dUpperLimit --> No Go
#'               Otherwise, continue to the next analysis. At the Final Analysis: If the Lower Limit of the CI, denoted by LL, is greater than dLowerLimit then a Go decision is made.  Specifically, if LL > UserParam$dLowerLimit --> Go
#'               Otherwise, a No Go decision is made
#'              
#' @return TestStat A double value of the computed test statistic
#' @return Decision An integer value: Decision = 0 --> No boundary crossed
#'                                    Decision = 1 --> Lower Efficacy Boundary Crossed
#'                                    Decision = 2 --> Upper Efficacy Boundary Crossed
#'                                    Decision = 3 --> Futility Boundary Crossed
#'                                    Decision = 4 --> Equivalence Boundary Crossed
#' @return ErrorCode An integer value:  ErrorCode = 0 --> No Error
#                                       ErrorCode > 0 --> Non fatal error, current simulation is aborted but the next simulations will run
#                                       ErrorCode < 0 --> Fatal error, no further simulation will be attempted
#'@note In this example, the boundary information that is computed and sent from East is ignored in order to implement this decision approach.
#'@note Helpful Hints:
#'       There is often info that East sends to R that are not shown in a given example.  It can be very helpful to save the input 
#'       objects and then load them into your R session and inspect them.  This can be done with the following R code in your function.
#'
#'       saveRDS( SimData,     "SimData.Rds")
#'       saveRDS( DesignParam, "DesignParam.Rds" )
#'       saveRDS( LookInfo,    "LookInfo.Rds" )
#'
#'       The above code will save each of the input objects to a file so they may be examined within R.
#' @export
######################################################################################################################## .
AnalyzeUsingPropLimitsOfCI<- function(SimData, DesignParam, LookInfo, UserParam = NULL)
{
    if( is.null( UserParam ) )
    {
        UserParam <- list(dLowerLimit = 0.1, dConfLevel = 0.8, dUpperLimit = 0.2)
    }
    
    # Retrieve necessary information from the objects East sent
    nQtyOfLooks          <- LookInfo$NumLooks
    nLookIndex           <- LookInfo$CurrLookIndex
    nQtyOfEvents         <- LookInfo$CumEvents[ nLookIndex ]
    
    # Input objects can be saved through the following lines:
    
    #setwd( "[ENTER THE DIRECTORY WHERE YOU WANT TO SAVE DATA]")
    #saveRDS( SimData, "SimData.Rds")
    #saveRDS( DesignParam, "DesignParam.Rds" )
    #saveRDS( LookInfo, "LookInfo.Rds" )
    
    


    
    nQtyOfPatsInAnalysis <- LookInfo$CumCompleters[ nLookIndex ]
    
    # Create the vector of simulated data for this IA - East sends all of the simulated data
    vPatientOutcome      <- SimData$Response[ 1:nQtyOfPatsInAnalysis ]
    vPatientTreatment    <- SimData$TreatmentID[ 1:nQtyOfPatsInAnalysis ]
    
    # Create vectors of data for each treatment 
    vOutcomesS           <- vPatientOutcome[ vPatientTreatment == 0 ]
    vOutcomesE           <- vPatientOutcome[ vPatientTreatment == 1 ]
    
    # Perform the desired analysis, then determine if the lower limit of the confidence interval is greater than the user-specified value
    mData                <- cbind(table(vOutcomesS), table(vOutcomesE))
    lAnalysisResult      <- prop.test(mData, alternative = "two.sided", correct = FALSE, conf.level = UserParam$dConfLevel)
    dLowerLimitCI        <- lAnalysisResult$conf.int[ 1 ]
    # A decision of 2 means success, 0 means continue the trial
    nDecision            <- ifelse( dLowerLimitCI > UserParam$dLowerLimit, 2, 0 )  
    
    if( nDecision == 0 )
    {
        # Check futility 
        dUpperLimitCI        <- lAnalysisResult$conf.int[ 2 ]
        
        # Did not hit a Go decision, so check No Go
        # We are at the FA, efficacy decision was not made yet so the decision is futility
        if( nLookIndex == nQtyOfLooks ) 
        {
            # The final analysis was reached and a Go decision could not be made, thus a No Go decision is made
            nDecision <- 3 # East code for futility 
        }
        # At the IA check the No Go since a Go decision was not made
        else if( dUpperLimitCI < UserParam$dUpperLimit )  
            nDecision <- 3 # East code for futility 
        
    }
    
    Error 	= 0

    
    
    return(list(TestStat = as.double(dLowerLimitCI), ErrorCode = as.integer(Error), Decision = as.integer( nDecision ) ))
}



##################################################################################### #
# File 4: AnalyzeUsingPropTest.R Timestamp: 2024-01-30 10:50:03 ####
##################################################################################### #


######################################################################################################################## .
#' @param AnalyzeUsingPropTest
#' @title Analyze using the prop.test function in base R.
#' @param SimData Data frame which consists of data generated in current simulation.
#' @param DesignParam List of Design and Simulation Parameters required to perform analysis.
#' @param LookInfo List containing Design and Simulation Parameters, which might be required to perform analysis.
#' @param UserParam A list of user defined parameters in East. The default must be NULL.
#' @description This example utilizes the prop.test function in base R to perform the analysis. The p-value from prop.test is used to compute the Z statistic that is compared to the upper boundary computed and sent by East as an input.  
#'              This example does NOT include a futility rule. 
#'              
#' @return TestStat A double value of the computed test statistic
#' @return Decision An integer value: Decision = 0 --> No boundary crossed
#'                                    Decision = 1 --> Lower Efficacy Boundary Crossed
#'                                    Decision = 2 --> Upper Efficacy Boundary Crossed
#'                                    Decision = 3 --> Futility Boundary Crossed
#'                                    Decision = 4 --> Equivalence Boundary Crossed
#' @return ErrorCode An integer value:  ErrorCode = 0 --> No Error
#                                       ErrorCode > 0 --> Non fatal error, current simulation is aborted but the next simulations will run
#                                       ErrorCode < 0 --> Fatal error, no further simulation will be attempted

#'@note Helpful Hints:
#'       There is often info that East sends to R that are not shown in a given example.  It can be very helpful to save the input 
#'       objects and then load them into your R session and inspect them.  This can be done with the following R code in your function.
#'
#'       saveRDS( SimData,     "SimData.Rds")
#'       saveRDS( DesignParam, "DesignParam.Rds" )
#'       saveRDS( LookInfo,    "LookInfo.Rds" )
#'
#'       The above code will save each of the input objects to a file so they may be examined within R.
######################################################################################################################## .

#TODO(Kyle)- I am not sure where to substitute in the user parameters since most seems to be sent from East and then analyzed using prop.test
#' @export
AnalyzeUsingPropTest<- function(SimData, DesignParam, LookInfo, UserParam = NULL)
{
    
    
    # Retrieve necessary information from the objects East sent
    nLookIndex           <- LookInfo$CurrLookIndex
    nQtyOfEvents         <- LookInfo$CumEvents[ nLookIndex ]
    
    # Input objects can be saved through the following lines:
    
    #setwd( "[ENTER THE DIRECTORY WHERE YOU WANT TO SAVE DATA]")
    #saveRDS( SimData, "SimData.Rds")
    #saveRDS( DesignParam, "DesignParam.Rds" )
    #saveRDS( LookInfo, "LookInfo.Rds" )
    nQtyOfPatsInAnalysis <- LookInfo$CumCompleters[ nLookIndex ]
    
    
  
    
    # Create the vector of simulated data for this IA - East sends all of the simulated data
    vPatientOutcome      <- SimData$Response[ 1:nQtyOfPatsInAnalysis ]
    vPatientTreatment    <- SimData$TreatmentID[ 1:nQtyOfPatsInAnalysis ]
    
    # Create vectors of data for each treatment 
    vOutcomesS           <- vPatientOutcome[ vPatientTreatment == 0 ]
    vOutcomesE           <- vPatientOutcome[ vPatientTreatment == 1 ]
    
    # Perform the desired analysis 
    mData                <- cbind(table(vOutcomesS), table(vOutcomesE))
    lAnalysisResult      <- prop.test(mData, alternative = "greater", correct = FALSE)
    dPValue              <- lAnalysisResult$p.value
    dZValue              <- qnorm( 1 - dPValue )
    nDecision            <- ifelse( dZValue > LookInfo$EffBdryUpper[ nLookIndex], 2, 0 )  # A decision of 2 means success, 0 means continue the trial
    
    if( nDecision == 0 )
    {
        # if needed, check futility
        
        
        
    }
    
    Error 	= 0
    
    
    return(list(TestStat = as.double(dZValue), ErrorCode = as.integer(Error), Decision = as.integer( nDecision ) ))
}



##################################################################################### #
# File 5: CombineAllRFiles.R Timestamp: 2024-01-30 10:50:03 ####
##################################################################################### #


#################################################################################################### .
#   Program/Function Name:
#   Author: Author Name
#   Description: This function is used to combine all .R files in a directory into a single file for use in Cytel products. 
#   Change History:
#   Last Modified Date: 01/26/2024
#################################################################################################### .
#' @name CombineAllRFiles
#' @title CombineAllRFiles
#' @description { Description: This function is used to combine all .R files in a directory into a single file for use in Cytel products.  }
#' @export
CombineAllRFiles <- function( strOutFileName, strDirectory = "" )
{

    
    # Get the list of files in the specified directory
    vFileList <- list.files(path = strDirectory, full.names = TRUE)
    
    # Create or open the output file
    outputStream <- file(strOutFileName, open = "w")
    
    # Vector to store the names of the combined files
    vCombinedViles <- character(0)
    
    # Loop through each file in the directory
    for (strFileName in vFileList) 
    {
        
        # Read the content of the current file
        strFileContent <- readLines(strFileName)
        
        # Insert a comment with file name and timestamp
        strComment     <- paste("# File:", basename(strFileName), "Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
        strFileContent <- c( strComment, strFileContent )
        
        # Write the content to the output file
        writeLines(strFileContent, outputStream)
        
        # Add the name of the combined file to the vector
        vCombinedViles <- c(vCombinedViles, basename(strFileName))
    }
    
    # Close the output file
    close(outputStream)
    
    # Print the names of the combined files
    cat("Files combined successfully:", paste(vCombinedViles, collapse = ", "), "\n")
}



##################################################################################### #
# File 6: CreateCyneRgyFuntion.R Timestamp: 2024-01-30 10:50:03 ####
##################################################################################### #


#################################################################################################### .
#   Program/Function Name: CreateCyneRgyFuntion
#   Author: J. Kyle Wathen
#   Description: This function will create a new file containing the template for the desired CyneRgy function.
#   Change History:
#   Last Modified Date: 12/19/2023
#################################################################################################### .
#' Create new CyneRgy Function using provided templates. These R function that is created can be used in connection with Cytel-R integration.
#' @description { Description: This function will create a new file containing the template for the desired CyneRgy function. }
#' @export
CreateCyneRgyFuntion <- function( strFunctionType, strNewFunctionName = "", strDirectory = NA )
{
    strNewFileExt <- ".R"
    strNewFileName <- strNewFunctionName
    
    
    #TODO: Make sure the strFunctionType is a valid type, eg PatientSimulator, Analysis ect
    # Make sure the file does not already exist
    # create it and open it
    strPackage <- "CyneRgy"
    
    # Exiting template names, remove extensions
    vValidExamples <- tools::file_path_sans_ext(list.files(system.file("Templates", package = strPackage)) )
    vValidExamplesFullPath <- list.files(system.file("Templates", package = strPackage), full.names = TRUE) 
    
    validExamplesMsg <-
        paste0(
            "Valid values for strFunctionType are: '",
            paste(vValidExamples, collapse = "', '"),
            "'")
    
    # Check if strFunctionType is a valid example
    if (!(strFunctionType %in% tools::file_path_sans_ext(basename(vValidExamples)))) {
        print( paste0( 
            'Please run `CreateCyneRgyFuntion()` with a valid strFunctionType argument name.',
            validExamplesMsg ))
        return()
    }
    
    # Find the full path of the selected example
    strSelectedExample <- vValidExamplesFullPath[grep(strFunctionType, vValidExamples)]
    
    # Check if the file already exists in the destination directory
    if (!is.na(strDirectory) && file.exists(file.path(strDirectory, basename(strSelectedExample)))) {
        stop("File already exists in the destination directory.")
    }
    
    # Determine the destination directory
    if (is.na(strDirectory)) {
        strDirectory <- getwd()  # Use the current working directory if not specified
    }
    
    # Create the full path for the new file
    strNewFilePath <- paste0(strDirectory, "/",ifelse(strNewFileName == "", basename(strSelectedExample), strNewFileName), strNewFileExt)
    
    # Check if the file name exists and if so update it
    # Create the file name
    bFileExists <- file.exists( strNewFilePath )
    # Need to find the file name since it already exists
    nIndex <- 0
    while( bFileExists )
    {
        nIndex      <- nIndex + 1
        #strFileName <- paste( strPkgDir, "/R/", strFunctionName, nIndex, ".R", sep ="" )
        strNewFilePath <- paste0(strDirectory, "/",ifelse(strNewFileName == "", basename(strSelectedExample), strNewFileName), nIndex,  strNewFileExt)
        
        bFileExists <- file.exists( strNewFilePath )
    }
    
    # Copy the file to the destination directory
    file.copy(strSelectedExample, strNewFilePath )
    
    # Print a message indicating success
    cat("File copied successfully to:", strNewFilePath, "\n")
    
    strToday           <- format(Sys.Date(), format="%m/%d/%Y")
    
    # Update the tags in the file that was copied  
    vTags    <- c("FUNCTION_NAME",  "CREATION_DATE")
    vReplace <- c(strNewFunctionName, strToday)
    ReplaceTagsInFile( strNewFilePath, vTags, vReplace )
    
    
    # Open the file in RStudio
    strIgnore <- rstudioapi::navigateToFile( strNewFilePath )
}

##################################################################################### #
# File 7: HelperFunctionsWeibull.R Timestamp: 2024-01-30 10:50:03 ####
##################################################################################### #


######################################################################################################################## .
# Helper function to go with the Weibull example.  ####
######################################################################################################################## .

#' @param  vTime A vector of times to compute the hazard of the Weibull distribution at
#' @param dShape The shape of the Weibull distribution, see rweibull
#' @param dScale The scale of the Weibull distribution, see rweibull
#' @description
#' Function to compute the hazard of the Weibull distribution 
#' 
ComputeHazardWeibulll <- function( vTime, dShape, dScale )
{
    vHaz <- (dShape/dScale) * (vTime/dScale )^(dShape-1)
    return ( vHaz )
}

#' @param dShape The shape of the Weibull distribution
#' @param dMedian The median of the Weibull distribution
#' @description
#' Compute the scale parameter for the Weibull distribution with median = dMedian and scale parameter = dScale
#' 
ComputeScaleGivenShapeMedian <- function( dShape, dMedian )
{
    dScale <- dMedian/exp( log( -log( 0.5) )/dShape )
    return( dScale )
}

######################################################################################################################## .
# Example - Weibull with increasing hazards with median of 12 vs 15 ####
######################################################################################################################## .
# dShapeS     <- 1.9
# dMedianS    <- 12
# 
# dScaleS     <- ComputeScaleGivenShapeMedian( dShapeS, dMedianS )
# dScaleS
# 
# nQtyPats    <- 10000
# vTime       <- seq( 0.05, 40, 0.05)
# vHazardS    <- ComputeHazardWeibulll( vTime, dShapeS, dScaleS )
# vDataS      <- rweibull( nQtyPats, dShapeS, dScaleS )
# 
# 
# dShapeE     <- 1.9
# dMedianE    <- 15
# dScaleE     <- ComputeScaleGivenShapeMedian( dShapeE, dMedianE )
# dScaleE
# 
# vHazardE    <- ComputeHazardWeibulll( vTime, dShapeE, dScaleE )
# vDataE      <- rweibull( nQtyPats, dShapeE, dScaleE )
# 
# 
# plot( vTime, vHazardS, type = 'l', xlab = "Time", ylab="Hazard", main ="Hazard: Standard of Care (Solid), Experimental (Dashed)" )
# lines( vTime, vHazardE, lty =2)
# 
# 
# print( paste( "Parameters for S: Shape = ", round( dShapeS, 3), ", Scale= ", round( dScaleS, 3 )) )
# print( paste( "Parameters for E: Shape = ", round( dShapeE, 3), ", Scale= ", round( dScaleE, 3 )) )
# print( paste( "Observed median on S: ", median( vDataS ) ) )
# print( paste( "Observed median on E: ", median( vDataE ) ) )
# print( paste( "Observed HR=", median( vDataS )/median( vDataE ) ) )
# 
# ######################################################################################################################## .
# # Example - Weibull with decreasing hazard with median of 12 vs 15 ####
# ######################################################################################################################## .
# # Standard of Care Treatment
# dShapeS     <- 0.6
# dMedianS    <- 12
# 
# dScaleS     <- ComputeScaleGivenShapeMedian( dShapeS, dMedianS )
# dScaleS
# 
# nQtyPats    <- 10000
# vTime       <- seq( 0.05, 40, 0.05)
# vHazardS    <- ComputeHazardWeibulll( vTime, dShapeS, dScaleS )
# vDataS      <- rweibull( nQtyPats, dShapeS, dScaleS )
# 
# 
# dShapeE     <- 0.6
# dMedianE    <- 15
# dScaleE     <- ComputeScaleGivenShapeMedian( dShapeE, dMedianE )
# dScaleE
# 
# vHazardE    <- ComputeHazardWeibulll( vTime, dShapeE, dScaleE )
# vDataE      <- rweibull( nQtyPats, dShapeE, dScaleE )
# 
# 
# plot( vTime, vHazardS, type = 'l', xlab = "Time", ylab="Hazard", main ="Hazard: Standard of Care (Solid), Experimental (Dashed)" )
# lines( vTime, vHazardE, lty =2)
# 
# print( paste( "Parameters for S: Shape = ", round( dShapeS, 3), ", Scale= ", round( dScaleS, 3 )) )
# print( paste( "Parameters for E: Shape = ", round( dShapeE, 3), ", Scale= ", round( dScaleE, 3 )) )
# print( paste( "Observed median on S: ", median( vDataS ) ) )
# print( paste( "Observed median on E: ", median( vDataE ) ) )
# print( paste( "Observed HR=", median( vDataS )/median( vDataE ) ) )
# 



##################################################################################### #
# File 8: InitLoadCyneRgy.R Timestamp: 2024-01-30 10:50:03 ####
##################################################################################### #


#' Initialize is used to load required libraries and create any global 
#' @export
InitLoadCyneRgy <- function( Seed , UserParam = NULL )
{
    library( CyneRgy )
    
    setwd( UserParam$Directory )

    ######################################################################################################################## .
    # Load any data sets libraries that are needed ####
    ######################################################################################################################## .
   
    Error <- 0
    return(as.integer(Error))
}

##################################################################################### #
# File 9: SimulatePatientOutcomePercentAtZeroBetaDist.R Timestamp: 2024-01-30 10:50:03 ####
##################################################################################### #


#' Simulate patient outcomes from a normal distribution with a percent of patients having an outcome of 0 where the probability of a 0 is drawn from a Beta distribution.
#' @param NumSub The number of subjects that need to be simulated, integer value
#' @param TreatmentID A vector of treatment ids, 0 = treatment 1, 1 = Treatment 2. length( TreatmentID ) = NumSub
#' @param Mean A vector of length = 2 with the means of the two treatments.
#' @param StdDev A vector of length = 2 with the standard deviations of each treatment
#' @param  UserParam A list of user defined parameters in East.   The default must be NULL resulting in ignoring the percent of patients at 0.
#' If UserParam is supplied, the list must contain the following named elements:
#' \describe{
#'      \item{UserParam$dCtrlBetaParam1}{First parameter in the Beta distribution for the control (ctrl) treatment.}
#'      \item{UserParam$dCtrlBetaParam2}{Second parameter in the Beta distribution for the control (ctrl) treatment.}
#'      \item{UserParam$dExpBetaParam1}{First parameter in the Beta distribution for the experimental (exp) treatment.}
#'      \item{UserParam$dExpBetaParam2}{Second parameter in the Beta distribution for the experimental (exp) treatment.}
#'      \item{Exmaple Input In East}{ \figure{../../articles/Example2_1Variables.png}{Example input in East} }
#'      }
#' 
#' @description
#' The function assumes that the probability a patient has a zero response is random and follows a Beta( a, b ) distribution.
#' Each distribution must provide 2 parameters for the beta distribution and the probability of 0 outcome is selected from the corresponding Beta distribution.
#' The probability of 0 outcome on the control treatment is sampled from a Beta( UserParam$dCtrlBetaParam1, UserParam$dCtrlBetaParam2 ) distribution.
#' The probability of 0 outcome on the experimental treatment is sampled from a Beta( UserParam$dExpBetaParam1, UserParam$dExpBetaParam2 ) distribution.
#' The intent of this option is to incorporate the variability in the unknown, probability of no response, quantity.  
#' @export
SimulatePatientOutcomePercentAtZeroBetaDist <- function(NumSub, TreatmentID, Mean, StdDev, UserParam = NULL)
{
    # Note: It can be helpful to save to the parameters that East sent.
    # The next two lines show how you could save the UserParam variable to an Rds file
    # setwd( "[ENTERED THE DESIRED LOCATION TO SAVE THE FILE]" )
    # saveRDS(UserParam, "UserParam.Rds")
    
    # If the user did not specify the user parameters, but still called this function then the probability
    # of a 0 outcome is 0 for both treatments
    if( is.null( UserParam ) )
    {
        vProbabilityOfZeroOutcome <- c( 0, 0 )
    }
    else
    {
        # Simulate the probability of a 0 response from the respective Beta distributions
        dProbabilityofZeroOutcomeCtrl <- rbeta( 1, UserParam$dCtrlBetaParam1, UserParam$dCtrlBetaParam2 )
        dProbabilityofZeroOutcomeExp  <- rbeta( 1, UserParam$dExpBetaParam1,  UserParam$dExpBetaParam2 )
        
        #Create the vProbabilityOfZeroOutcome that is needed below when the patient outcome is simulated 
        vProbabilityOfZeroOutcome     <- c( dProbabilityofZeroOutcomeCtrl, dProbabilityofZeroOutcomeExp )    
    }
    
    
    
    nError           <- 0 # East code for no errors occurred 
    vPatientOutcome  <- rep( 0, NumSub ) # Initialize the vector of patient outcomes as 0 so only the patients that do NOT have a zero response will be simulated
    
    
    for( nPatIndx in 1:NumSub )
    {
        nTreatmentID                <- TreatmentID[ nPatIndx ] + 1 # The TreatmentID vector sent from East has the treatments as 0, 1 so need to add 1 to get a vector index
        
        # Need to check the probability of a 0 outcome to make sure it is in the range (0, 1) and if not simulate the outcome accordingly 
        if( vProbabilityOfZeroOutcome[ nTreatmentID ] > 0 & vProbabilityOfZeroOutcome[ nTreatmentID ] < 1 ) # Probability is valid, so need to simulate if the patient is a 0 response
            nResponseIsZero <- rbinom( 1, 1, vProbabilityOfZeroOutcome[ nTreatmentID ] )
        else if( vProbabilityOfZeroOutcome[ nTreatmentID ] <= 0 )   # If Probability of a 0  <= 0
            nResponseIsZero <- 0
        else                        # if the probability of a 0 >= 1 --> Don't need to simulate from the normal distribution as all patients in the treatment are a 0
            nResponseIsZero <- 1
        
        
        if( nResponseIsZero == 0  )  # The patient responded, so we need to simulate their outcome from a normal distribution with the specified mean and standard deviation 
            vPatientOutcome[ nPatIndx ] <- rnorm( 1, Mean[ nTreatmentID ], StdDev[ nTreatmentID ])
    }
    
    if(  any( is.na( vPatientOutcome )==TRUE) )
        nError <- -100
    
    return( list( Response = as.double( vPatientOutcome ), ErrorCode = as.integer( nError ) ))
}

##################################################################################### #
# File 10: SimulatePatientSurvivalWeibull.r Timestamp: 2024-01-30 10:50:03 ####
##################################################################################### #


#' Simulate time-to-event patient data from a Weibull distribution
# Parameter Description 
# NumSub - The number of patients (subjects) in the trial.  NumSub survival times need to be generated for the trial.  
#           This is a single numeric value, eg 250.
# NumArm - The number of arms in the trial, a single numeric value.  For a two arm trial, this will be 2. 
# The SurvParam depends on input in East. In the simulation window on the Response Generation tab 
# SurvMethod - This values is pulled from the Input Method drop-down list.  
# SurvParam - Depends on the table in the Response Generation tab. 2‐D array of parameters uses to generate time of events.
# If SurvMethod is 1 (Hazard Rates):
#   SurvParam is an array that specifies arm by arm hazard rates (one rate per arm per piece). Thus SurvParam [i, j] specifies hazard rate in ith period for jth arm.
#   Arms are in columns with column 1 is control, column 2 is experimental
#   Time periods are in rows
# If SurvMethod is 2:
#   SurvParam is an array specifies arm by arm the Cum % Survivals (one value per arm per piece). Thus, SurvParam [i, j] specifies Cum % Survivals in ith period for jth arm.
# If SurvMethod is 3:
#   SurvParam will be a 1 x 2 array with median survival times on each arms. Column 1 is control, column 2 is experimental 
#  
# Description: This function simulates from exponential, just included as a simple example as a starting point 

#' @param NumSub NumSub - The number of patients (subjects) in the trial.  NumSub survival times need to be generated for the trial.  
#'           This is a single numeric value, eg 250.
#' @param NumArm The number of arms in the trial, a single numeric value.  For a two arm trial, this will be 2. 
#'           The SurvParam depends on input in East. In the simulation window on the Response Generation tab 
#' @export
SimulatePatientSurvivalWeibull<- function(NumSub, NumArm, TreatmentID, SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL ) 
{
    #TODO: Need to test that the paths that hit an error actually stop
    
    # The SurvParam depends on input in East, EAST sends the Median (see the Simulation->Response Generation tab for what is sent)
    setwd( "C:\\Kyle\\Cytel\\Software\\East-R\\EastRExamples\\Examples\\2ArmTimeToEventOutcomePatientSimulation\\ExampleOutput")
 
    # If you wanted to save the input objects you could use the following to save the files to your working directory
    
    # Example of how to save the data sent from East for each look.
    # If the DesignParam.Rds exists, then don't save it again.
    if( !file.exists(  "DesignParam.Rds" ))
    {
        saveRDS( SurvParam, paste0( "SurvParam.Rds") )
        # Use the same function as previous line if you want to save the other objects
    }

    # For this example, in East the user must set the Input Method to Hazard Rate and have the # of pieces = 2. 
    # This will cause SurvParam to be a 2x2 matrix. 
    # For this example we will assume that column 1 are the 2 Weibull parameters for Standard of Care and column 2 are the two Weibull parameters for Experimental 
    
    vSurvTime <- rep( -1, NumSub )  # The vector of patient survival times that will be returned.  
    
    vTreatmentID <- TreatmentID +1   # If this is 0 then it is control, 1 is treatment. Adding one since vectors are index by 1 
    ErrorCode    <- rep( -1, NumSub ) 
    
    
    if(SurvMethod == 1)   # Hazard Rates
    {
        
        if( NumPrd == 2 )
        {
            vShapes <- SurvParam[ 1, ]   # Row 1 is the shape parameters
            vScales <- SurvParam[ 2, ]   # Row 2 is the scale parameters
            # Simulate the patient survival times based on the treatment
            # For the Hazard Rate input with 1 piece, this is just simulating from an exponential distribution as an example and results will match
            # East if you used the build hazard option.
            for( nPatIndx in 1:NumSub)
            {
                nPatientTreatment     <- vTreatmentID[ nPatIndx ]
                vSurvTime[ nPatIndx ] <- rweibull( 1, vShapes[ nPatientTreatment ], vScales[ nPatientTreatment ] )
                
            }
        }
        else 
        {
            ErrorCode <- ERROR1   # CAUSE_AN_ERROR is not defined so should cause in error
        }
        
    }
    else if(SurvMethod == 2)   # Cumulative % Survivals
    {
        ErrorCode <- ERROR1 # ERROR1 is not defined so this will cause an error if the users selects anything besides Hazard Rate and uses this function
        
    }
    else if(SurvMethod == 3)   # Median Survival Times
    {
        ErrorCode <- ERROR3   # ERROR3 is not defined so this will cause an error if the users selects anything besides Hazard Rate and uses this function
    }
    
    return( list(SurvivalTime = as.double( vSurvTime ), ErrorCode = ErrorCode) )
}


