######################################################################################################################## .
#' @name PlotEmax
#' @title Run and Plot the Emax Response Example
#' @description
#' Sources the Emax response model, simulates a demonstration data set, reshapes
#' the generated responses, and plots patient trajectories and group summaries.
#' @author Anton Sun, Jacob Wathen, Gabriel Potvin
#' @return The final `lPlotResults` list containing the summary and plot. Running
#'   the script also creates the simulated response and long-form data objects.
######################################################################################################################## .

# Source the desired files
source( "GenerateResponseEmaxModel.R" )

# Helper to turn return list into a tidy data frame ####
EmaxToLong <- function( lEmaxRetval, TreatmentID, VisitTime = NULL )
{
    # grab all "Response<j>" entries
    vRespNames <- names( lEmaxRetval )[ stringr::str_detect( names( lEmaxRetval ), "^Response\\d+$" ) ]
    if( length( vRespNames ) == 0 ) stop( "No Response<j> elements found in result." )

    # bind to wide matrix: rows = subjects, cols = visits
    mResp <- do.call( cbind, lEmaxRetval[ vRespNames ] )
    colnames( mResp ) <- vRespNames

    df <- as.data.frame( mResp ) |>
        dplyr::mutate( Subject = dplyr::row_number(),
                       Group   = ifelse( TreatmentID == 0, "Control", "Treatment" ) ) |>
        tidyr::pivot_longer( cols = tidyselect::starts_with( "Response" ),
                             names_to = "Visit",
                             values_to = "Response" ) |>
        dplyr::mutate( VisitIndex = as.integer( stringr::str_replace( Visit, "Response", "" ) ) ) |>
        dplyr::arrange( Subject, VisitIndex )

    # attach actual times if provided
    if( !is.null( VisitTime ) )
    {
        stopifnot( length( unique( df$VisitIndex ) ) == length( VisitTime ) )
        df <- df |> dplyr::mutate( VisitTime = VisitTime[ VisitIndex ] )
    }
    else
    {
        df <- df |> dplyr::mutate( VisitTime = VisitIndex )
    }
    return( df )
}

# Plot function: means with 95% CI, by group across visits ####
PlotEmaxGroups <- function( dfData, sTitle, bShowIndividuals = TRUE )
{
    dfSummary <- dfData |>
        dplyr::group_by( Group, VisitTime ) |>
        dplyr::summarise( n       = dplyr::n(),
                          mean    = mean( Response, na.rm = TRUE ),
                          sd      = stats::sd( Response, na.rm = TRUE ),
                          se      = sd / sqrt( n ),
                          ci      = 1.96 * se,
                          .groups = "drop" )

    p <- ggplot2::ggplot() +
        # optional faint individual trajectories
        { if( bShowIndividuals )
            ggplot2::geom_line( data = dfData,
                                ggplot2::aes( x = VisitTime, y = Response, group = interaction( Subject, Group ), color = Group ),
                                alpha = 0.15 )
            else NULL } +
        # CI ribbons
        ggplot2::geom_ribbon( data = dfSummary,
                      ggplot2::aes( x = VisitTime, ymin = mean - ci, ymax = mean + ci, fill = Group ),
                      alpha = 0.2 ) +
        # Means
          ggplot2::geom_line( data = dfSummary,
                        ggplot2::aes( x = VisitTime, y = mean, color = Group ),
                        linewidth = 1.2 ) +
          ggplot2::geom_point( data = dfSummary,
                         ggplot2::aes( x = VisitTime, y = mean, color = Group ),
                         size = 2 ) +
          ggplot2::labs( title = sTitle,
                     x = "Visit Time",
                     y = "Response ( Emax model output )",
                     color = "Group", fill = "Group" ) +
          ggplot2::theme_minimal( base_size = 12 )

    print( p )
    invisible( list( summary = dfSummary, plot = p ) )
}

# Example usage ####

# Define a small scenario to demonstrate plot
set.seed( 123 )

NumSub      <- 60
NumVisit    <- 5
VisitTime   <- c( 1, 2, 3, 4, 5 )
TreatmentID <- sample( c( 0, 1 ), NumSub, replace = TRUE, prob = c( 0.5, 0.5 ) )

MeanControl   <- c( 10, 10, 10, 10, 10 )
MeanTrt       <- c( 0, 0, 0, 0, 0 )
StdDevControl <- rep( 5, NumVisit )
StdDevTrt     <- rep( 5, NumVisit )
CorrMat       <- diag( NumVisit )

UserParam <- list(
    AbsorptionRate  = 1, # absorption rate constant
    EliminationRate = 0.2, # elimination rate constant
    Dose = 500, # dose administered
    E0   = 5, # baseline
    Emax = 40, # max drug effect
    EC50 = 50     # concentration where 50% Emax realized
)

# Run simulator
lEmaxOut <- GenerateResponseEmaxModel(
    NumSub        = NumSub,
    NumVisit      = NumVisit,
    ArrivalTime   = rep( 0, NumSub ),
    TreatmentID   = TreatmentID,
    Inputmethod   = 0,
    VisitTime     = VisitTime,
    MeanControl   = MeanControl,
    MeanTrt       = MeanTrt,
    StdDevControl = StdDevControl,
    StdDevTrt     = StdDevTrt,
    CorrMat       = CorrMat,
    UserParam     = UserParam
)

# Build tidy frame and plot
dfEmax <- EmaxToLong( lEmaxOut, TreatmentID, VisitTime )

lPlotResults <- PlotEmaxGroups( dfEmax,
                                sTitle = "Control vs Treatment: Emax Responses over Visits",
                                bShowIndividuals = TRUE )

lPlotResults
