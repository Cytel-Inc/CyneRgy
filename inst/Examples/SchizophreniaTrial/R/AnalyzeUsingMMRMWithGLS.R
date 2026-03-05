#' @name AnalyzeUsingMMRMWithGLS
#' @title Perform MMRM Analysis Using Generalized Least Squares (GLS)
#' @description This function fits Mixed Model for Repeated Measures (MMRM) using GLS to simulated patient data, and returns the treatment effect estimate, p-value, and decision outcome.
#' @param SimData Data frame with subject data generated in current simulation with one row per patient. It will have headers indicating the names of the 
#' columns. These names will be same as those used in Data Generation. For analysis the most relevant variables are:
#'        \describe
#'        {
#'          \item{ArrivalTime}{Numeric vector respresenting patient arrival times}
#'          \item{TreatmentID}{Integer vector (0 = control, 1 = treatment)}
#'          \item{Response[X]}{Numeric vector representing response for visit X, where X = 1, 2, 3, 4, 5}
#'         }
#' @param DesignParam List which consists of Design and Simulation Parameters which user may need to compute 
#' test statistic and perform test. For analysis the most relevant variable is:
#'        \describe
#'        {
#'          \item{Alpha}{1-sided Type I Error}
#'        }
#' @param LookInfo List containing Design and Simulation Parameters, which might be required to perform analysis.
#' @param UserParam A list of user defined parameters in East or East Horizon. The default must be NULL.
#' 
#' @return A list containing the following elements:
#'         \describe{
#'           \item{Decision}{Optional integer value indicating the decision:
#'                           \describe{
#'                             \item{0}{No boundary crossed (neither efficacy nor futility).}
#'                             \item{1}{Lower efficacy boundary crossed.}
#'                             \item{2}{Upper efficacy boundary crossed.}
#'                             \item{3}{Futility boundary crossed.}
#'                             \item{4}{Equivalence boundary crossed.}
#'                           }}
#'         \item {PrimDelta}{Estimated treatment effect from the MMRM model with GLS at the final visit.}
#'         \item {p.value} {p-value for the analysis}
#'         \item{ErrorCode}{Optional integer value \describe{ 
#'                                     \item{ErrorCode = 0}{No Error}
#'                                     \item{ErrorCode > 0}{Nonfatal error, current simulation is aborted but the next simulations will run}
#'                                     \item{ErrorCode < 0}{Fatal error, no further simulation will be attempted}
#'                                     }}
#'         }
#' @export
######################################################################################################################## .

AnalyzeUsingMMRMWithGLS <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL ) 
{
    # Load required packages
    library( nlme )   
    library( rpact )   
    library( dplyr ) 
    library( tidyr )  
    library( CyneRgy )
    
    # Initialize outputs
    nError     <- 0
    nDecision  <- 0
    
    # Step 1: Setup LooksInfo ####
    if ( !is.null( LookInfo ) ) 
    {
        nQtyOfLooks          <- LookInfo$NumLooks
        nLookIndex           <- LookInfo$CurrLookIndex
        nQtyOfPatsForInterim <- LookInfo$CumCompleters[ nLookIndex ]
        nAnalysisVisit       <- LookInfo$InterimVisit
    } 
    else 
    {
        nLookIndex           <- 1
        nQtyOfLooks          <- 1
        nQtyOfPatsForInterim <- nrow( SimData )
    }
    
    # Step 2: Create analysis dataset ####
    dfNoBaselineAnalysisData <- CreateAnalysisDataset( SimData, LookInfo )
    
    # Step 3: Fit the MMRM using nlme::gls ####
    nVisits <- sum( grepl( "^Response", names( SimData ) ) )
    dfNoBaselineAnalysisData$TreatmentID <- as.factor( dfNoBaselineAnalysisData$TreatmentID )
    dfNoBaselineAnalysisData$Visit       <- factor( dfNoBaselineAnalysisData$Visit, levels = seq( 2 : nVisits) )
    
    # Create the vectors for analysis, using the names needed for the GetLSDiffGLS
    vOut      <- dfNoBaselineAnalysisData$Response
    vBaseline <- dfNoBaselineAnalysisData$Baseline
    vTrt      <- dfNoBaselineAnalysisData$TreatmentID
    vTime     <- dfNoBaselineAnalysisData$Visit
    vIND      <- dfNoBaselineAnalysisData$Id
    
    glsFit <- gls( vOut ~ vBaseline + vTrt * vTime ,
                   weights = varIdent( form = ~ 1 | vTime ),
                   correlation = corSymm( form = ~ 1 | vIND ), 
                   na.action = na.omit ) 
    
    lRetGLS <- GetLSDiffGLS( glsFit, 1, nVisits, FALSE )
    
    # Step 4: Obtain group‐sequential alpha ####
    if ( !is.null( LookInfo ) ) 
    {
        gsDesign <- getDesignGroupSequential( kMax         = nQtyOfLooks,
                                              alpha        = DesignParam$Alpha,
                                              sided        = 1,
                                              typeOfDesign = "OF" )
        
        dAlpha   <- gsDesign$alphaSpent[ nLookIndex ]
    } 
    else 
    {
        dAlpha   <- DesignParam$Alpha
    }
    
    # Step 5: Decision rules ####
    if( lRetGLS$dPValue <= dAlpha ) 
    {
        if( nLookIndex == nQtyOfLooks )
        {
            # FA Efficacy condition 
            bIAEfficacyCondition <- FALSE
            bFAEfficacyCondition <- TRUE
        }
        else 
        {
            # IA Efficacy condition
            bIAEfficacyCondition <- TRUE
            bFAEfficacyCondition <- FALSE
        }
        # Efficacy decision
        strDecision <- CyneRgy::GetDecisionString( LookInfo   = LookInfo,
                                                   nLookIndex  = nLookIndex,
                                                   nQtyOfLooks = nQtyOfLooks,
                                                   bIAEfficacyCondition = bIAEfficacyCondition,
                                                   bFAEfficacyCondition = bFAEfficacyCondition )
        
        nDecision   <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )
    } 
    else 
    {
        strDecision <- CyneRgy::GetDecisionString( LookInfo = LookInfo,
                                                   nLookIndex = nLookIndex, 
                                                   nQtyOfLooks = nQtyOfLooks )
        
        nDecision   <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )
    }
    
    # Step 6: Return analysis results ####
    lRet <- list( Decision  = as.integer( nDecision ),
                  PrimDelta = as.double( lRetGLS$dEst ),
                  p.value   = as.double( lRetGLS$dPValue ),
                  ErrorCode = as.integer( nError ) )
    
    return( lRet )
}


######################################################################################################################## .
# Define auxiliary functions
######################################################################################################################## .

# Function to create a dataset for analysis

CreateAnalysisDataset <- function( SimData, LookInfo )
{
    # Step 1: Setup LooksInfo ####
    if ( !is.null( LookInfo ) ) 
    {
        nQtyOfLooks          <- LookInfo$NumLooks
        nLookIndex           <- LookInfo$CurrLookIndex
        nQtyOfPatsForInterim <- LookInfo$CumCompleters[ nLookIndex ]
        nAnalysisVisit       <- LookInfo$InterimVisit
    }
    else 
    {
        nLookIndex           <- 1
        nQtyOfLooks          <- 1
        nQtyOfPatsForInterim <- nrow( SimData )
    }
    
    # Step 2: Reshape wide → long in one shot ####
    dfLongData <- SimData %>%
        mutate( Id = row_number() ) %>%
        
        pivot_longer( cols          = matches( "^(Response|ArrTimeVisit)\\d+$" ),
                      names_to      = c( ".value", "Visit" ),
                      names_pattern = "(Response|ArrTimeVisit)(\\d+)" ) %>%
        
        mutate( Visit             = as.integer( Visit ),
                CalendarVisitTime = ArrivalTime + ArrTimeVisit ) %>%
        
        select( Id, TreatmentID, Visit, Response, CalendarVisitTime ) %>%
        
        arrange( Visit, CalendarVisitTime )
    
    # Step 3: Interim‐look filtering using dplyr ####
    if ( !is.null( LookInfo ) ) 
    {
        
        # 3a) compute cutoff time
        dAnalysisTime <- dfLongData %>%
            filter( Visit == nAnalysisVisit ) %>%
            slice( nQtyOfPatsForInterim ) %>%
            pull( CalendarVisitTime )
        
        # 3b) pick subjects
        if ( LookInfo$IncludePipeline == 0 )  
        {
            vSubjectsForAnalysis <- dfLongData %>%
                filter( Visit == nAnalysisVisit,
                        CalendarVisitTime <= dAnalysisTime ) %>%
                distinct( Id ) %>%
                pull( Id )
        } 
        else 
        {
            vSubjectsForAnalysis <- dfLongData %>%
                filter( CalendarVisitTime <= dAnalysisTime ) %>%
                distinct( Id ) %>%
                pull( Id )
        }
        
        dfAnalysisData <- dfLongData %>%
            filter( Id %in% vSubjectsForAnalysis )
    } 
    else 
    {
        dfAnalysisData <- dfLongData
    }
    
    # Step 4: Prepare for MMRM ####
    dfAnalysisData <- dfAnalysisData %>%
        mutate( Visit       = factor( Visit ),
                TreatmentID = factor( TreatmentID ),
                Id          = factor( Id ) )
    
    # Step 5: Create a dataset ####
    # The dataset removes the long form and adds the baseline response as a new column
    dfNoBaselineAnalysisData <- filter( dfAnalysisData, Visit != 1 )
    dfBaselineAnalysisData   <- filter( dfAnalysisData, Visit == 1 ) %>% select( Id, Baseline = Response )
    dfNoBaselineAnalysisData <- left_join( dfNoBaselineAnalysisData, dfBaselineAnalysisData, by = "Id" )
    
    return( dfNoBaselineAnalysisData )
}

# Function to compute Least Squares Difference from GLS Fit

GetLSDiffGLS <-  function( glsFit, nTrt, nTime, bPlacMinusTrt )
{
    # Step 1: Construct variable names for treatment, time and intercept ####
    strWhichTrt     <- paste( "vTrt" , nTrt, sep = "" )
    strWhichTime    <- paste( "vTime", nTime, sep = "" )
    strIntercept    <- "(Intercept)"
    
    # Step 2: Determine which variables to include in the estimate ####
    vCoeff          <- coef( glsFit )
    
    if( !any(names(vCoeff ) == strWhichTime ) )
    {
        # Time is the baseline; no need to include the interaction
        vVarNames       <- strWhichTrt
        vVarNamesPlac   <- strIntercept
        vVarNamesTrt    <- c( strIntercept, strWhichTrt )
    }
    else
    {
        # Time is not baseline; include trt * time interaction
        vVarNames       <- c( strWhichTrt, paste( strWhichTrt, strWhichTime, sep = ":" ) )
        vVarNamesPlac   <- c( strIntercept, strWhichTime )
        vVarNamesTrt    <- c( vVarNamesPlac, vVarNames )
    }
    
    # Include baseline covariate if present
    if( any( names( vCoeff ) == "vBaseline" ) )
    {
        vVarNamesPlac <- c( vVarNamesPlac, "vBaseline" )
        vVarNamesTrt  <- c( vVarNamesTrt, "vBaseline" )
    }
    
    # Step 3: Degrees of freedom ####
    nDOF <- diff( unlist( glsFit$dims ) [ 2:1 ] )
    
    # Step 4: Estimate for Treatment - Placebo ####
    dEst <- sum(vCoeff[ vVarNames ])
    if( bPlacMinusTrt ) dEst <- dEst * -1
    
    # Step 5: Compute standard error, t-statistic, and p-value ####
    dSE      <- sqrt( sum( vcov( glsFit )[ vVarNames, vVarNames ] ) )
    dTStat   <- dEst / dSE
    dPValue  <- pt( dTStat, nDOF )
    
    # Step 6: Final return ####
    lRet <- list( dPValue = dPValue, dEst = dEst, nDOF = nDOF, dSE = dSE, dTStat = dTStat )
    
    return( lRet )
}