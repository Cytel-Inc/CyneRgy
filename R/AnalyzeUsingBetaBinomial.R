######################################################################################################################## .
#' Analyze for efficacy using a beta prior to compute the posterior probability that experimental is better than standard of care. 
#' @param SimData Data frame which consists of data generated in current simulation.
#' @param DesignParam Input Parameters which user may need to compute test statistic and perform test. 
#'                    User should access the variables using names, for example,  DesignParam$Alpha, and not order. 
#' @param LookInfo List Input Parameters related to multiple looks which user may need to compute test statistic 
#'                 and perform test. User should access the variables using names, 
#'                 for example LookInfo$NumLooks and not order. Other important variables in group sequential designs are: 
#'                   LookInfo$NumLooks An integer value with the number of looks in the study
#'                   LookInfo$CurrLookIndex An integer value with the current index look, starting from 1
#'                   LookInfo$CumEvents A vector of length LookInfo$NumLooks that contains the number of events at the look.
#' @param UserParam A list of user defined parameters in East or Solara.
#'                  UserParam must be supplied and contain the following named elements:
#'  \describe{
#'      \item{UserParam$dAlphaCtrl}{Prior alpha parameter for control treatment.  Equivalent to the prior number of treatment successes. }
#'      \item{UserParam$dBetaCtrl}{Prior beta parameter for control treatment.  Equivalent to the prior number of treatment failures.}
#'      \item{UserParam$dAlphaExp}{Prior alpha parameter for experimental treatment. Equivalent to the prior number of treatment successes.}
#'      \item{UserParam$dBetaExp}{Prior beta parameter for experimental treatment. Equivalent to the prior number of treatment failures.}
#'      \item{UserParam$dUpperCutoffEfficacy}{A value (0,1) that specifies the upper cutoff for the efficacy check. Above this value will declare efficacy }
#'      \item{UserParam$dLowerCutoffForFutility}{A value (0,1) that specified the lower cutoff for the futility check. Below this value will declare futility. }
#'  }
#'  If user variables are not specified then a Beta( 1, 1 ) prior  is utilized for both standard of care and experimental.
#'  
#' @description In this version, the analysis for efficacy is to assume a beta prior to compute the posterior probability that experimental is better than standard of care
#'              Futility is based on a low posterior probability, eg it is unlikely that experimental is better than standard of care  
#' @return The function must return a list in the return statement of the function. The information below lists 
#'             elements of the list, if the element is required or optional and a description of the return values if needed.
#'             \describe{
#'                  \item{Decision}{Required value. Integer Value with the following meaning:
#'                                  \describe{
#'                                    \item{Decision = 0}{when No boundary, futility or efficacy is  crossed}
#'                                    \item{Decision = 1}{when the Lower Efficacy Boundary Crossed}
#'                                    \item{Decision = 2}{when the Upper Efficacy Boundary Crossed}
#'                                    \item{Decision = 3}{when the Futility Boundary Crossed}
#'                                    \item{Decision = 4}{when the Equivalence Boundary Crossed}
#'                                    } 
#'                                    }
#'                  \item{ErrorCode}{Optional integer value \describe{ 
#'                                     \item{ErrorCode = 0}{No Error}
#'                                     \item{ErrorCode > 0}{Non fatal error, current simulation is aborted but the next simulations will run}
#'                                     \item{ErrorCode < 0}{Fatal error, no further simulation will be attempted}
#'                                     }
#'                                     }
#'                  \item{Delta}{Estimated different between experimental and standard of care}
#'                  }
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
#' @export
######################################################################################################################## .
AnalyzeUsingBetaBinomial <- function(SimData, DesignParam, LookInfo, UserParam = NULL)
{
    # The below lines set the values of the parameters if a user does not specify a value
    
    if( is.null( UserParam ) )
    {
        # Default values are non-informative uniform prior
        UserParam <- list(dAlphaS = 1, dBetaS = 1, 
                          dAlphaE = 1, dBetaE = 1, 
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
    
    
    return(list(TestStat = as.double(lRet$dPostProb), ErrorCode = as.integer(Error), Decision = as.integer( nDecision ), Delta = as.double( lRet$dDelta ) ))
}




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
    
    # Compute Delta: mean(Pi_E) - mean( Pi_C)
    dDelta     <- ( dAlphaE/( dAlphaE + dBetaE) ) - ( dAlphaS/( dAlphaS +  dBetaS ))
    return(list(dPostProb = dPostProb, dDelta = dDelta))
}





