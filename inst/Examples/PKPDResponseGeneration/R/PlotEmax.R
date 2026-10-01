######################################################################################################################## .
#' @name EmaxToLong
#'
#' @title Emax To Long
#'
#' @description Convert generated visit-level Emax responses to long format and plot the response
#'   trajectories and group summaries. Run this demonstration from the example R directory.
#'
#' @author Anton Sun, Jacob Wathen, Gabriel Potvin
#'
#' @param lEmaxRetval Named list of simulated visit-level responses returned by GenerateResponseEmaxModel.
#'
#' @param TreatmentID Integer vector of treatment assignments, with one element per subject: 0 = placebo/control, 1
#'   = first experimental arm, 2 = second experimental arm, and so on.
#'
#' @param VisitTime Numeric vector of visit times measured from enrollment, of length NumVisit and ordered by visit.
#'
#' @return Data frame with one row per subject and visit, including Subject, Group, Visit, VisitIndex, Response,
#'   and VisitTime.
#'
#' @details If VisitTime is NULL, use the numeric visit indices as visit times.
######################################################################################################################## .

source( "GenerateResponseEmaxModel.R" )

# Helper to turn return list into a tidy data frame ####
EmaxToLong <- function( lEmaxRetval, TreatmentID, VisitTime = NULL ) {
    # grab all "Response<j>" entries
    vRespNames <- names( lEmaxRetval )[ stringr::str_detect( names( lEmaxRetval ), "^Response\\d+$" ) ]
    if ( length( vRespNames ) == 0 ) {
        stop( "No Response<j> elements found in result." )
    }

    # bind to wide matrix: rows = subjects, cols = visits
    mResp <- do.call( cbind, lEmaxRetval[ vRespNames ] )
    colnames( mResp ) <- vRespNames

    dfData <- as.data.frame( mResp ) |>
        dplyr::mutate(
            Subject = dplyr::row_number( ),
            Group = ifelse( TreatmentID == 0, "Control", "Treatment" )
        ) |>
        tidyr::pivot_longer(
            cols = tidyselect::starts_with( "Response" ),
            names_to = "Visit",
            values_to = "Response"
        ) |>
        dplyr::mutate( VisitIndex = as.integer( stringr::str_replace( Visit, "Response", "" ) ) ) |>
        dplyr::arrange( Subject, VisitIndex )

    # attach actual times if provided
    if ( !is.null( VisitTime ) ) {
        stopifnot( length( unique( dfData$VisitIndex ) ) == length( VisitTime ) )
        dfData <- dfData |> dplyr::mutate( VisitTime = VisitTime[ VisitIndex ] )
    } else {
        dfData <- dfData |> dplyr::mutate( VisitTime = VisitIndex )
    }
    return( dfData )
}

######################################################################################################################## .
#' @name PlotEmaxGroups
#'
#' @title Plot Emax response summaries by treatment group
#'
#' @description Plot mean responses and 95\% confidence intervals by treatment group and visit time,
#'   optionally including individual subject trajectories. Confidence intervals use the number of observed
#'   responses in each group and visit.
#'
#' @author Anton Sun, Jacob Wathen, Gabriel Potvin
#'
#' @param dfData Data frame returned by EmaxToLong, with Subject, Group, VisitTime, and Response columns.
#'
#' @param strTitle Character title for the plot.
#'
#' @param bShowIndividuals Logical value indicating whether to include individual subject trajectories.
#'
#' @return An invisible named list containing summary (group and visit summaries) and plot (the ggplot object).
#'   The function also displays the plot.
######################################################################################################################## .
PlotEmaxGroups <- function( dfData, strTitle, bShowIndividuals = TRUE ) {
    dfSummary <- dfData |>
        dplyr::group_by( Group, VisitTime ) |>
        dplyr::summarise(
            n = sum( !is.na( Response ) ),
            mean = mean( Response, na.rm = TRUE ),
            sd = stats::sd( Response, na.rm = TRUE ),
            se = sd / sqrt( n ),
            ci = 1.96 * se,
            .groups = "drop"
        )

    cPlot <- ggplot2::ggplot( ) +
        {
            # optional faint individual trajectories
            if ( bShowIndividuals ) {
                ggplot2::geom_line(
                    data = dfData,
                    ggplot2::aes( x = VisitTime, y = Response, group = interaction( Subject, Group ), color = Group ),
                    alpha = 0.15
                )
            } else {
                NULL
            }
        } +
        # CI ribbons
        ggplot2::geom_ribbon(
            data = dfSummary,
            ggplot2::aes( x = VisitTime, ymin = mean - ci, ymax = mean + ci, fill = Group ),
            alpha = 0.2
        ) +
        # Means
        ggplot2::geom_line(
            data = dfSummary,
            ggplot2::aes( x = VisitTime, y = mean, color = Group ),
            linewidth = 1.2
        ) +
        ggplot2::geom_point(
            data = dfSummary,
            ggplot2::aes( x = VisitTime, y = mean, color = Group ),
            size = 2
        ) +
        ggplot2::labs(
            title = strTitle,
            x = "Visit Time",
            y = "Response ( Emax model output )",
            color = "Group", fill = "Group"
        ) +
        ggplot2::theme_minimal( base_size = 12 )

    print( cPlot )
    return( invisible( list( summary = dfSummary, plot = cPlot ) ) )
}

# Example usage ####

# Define a small scenario to demonstrate plot
set.seed( 123 )

NumSub <- 60
NumVisit <- 5
VisitTime <- c( 1, 2, 3, 4, 5 )
TreatmentID <- sample( c( 0, 1 ), NumSub, replace = TRUE, prob = c( 0.5, 0.5 ) )

MeanControl <- c( 10, 10, 10, 10, 10 )
MeanTrt <- c( 0, 0, 0, 0, 0 )
StdDevControl <- rep( 5, NumVisit )
StdDevTrt <- rep( 5, NumVisit )
CorrMat <- diag( NumVisit )

UserParam <- list(
    AbsorptionRate = 1, # absorption rate constant
    EliminationRate = 0.2, # elimination rate constant
    Dose = 500, # dose administered
    E0 = 5, # baseline
    Emax = 40, # max drug effect
    EC50 = 50 # concentration where 50% Emax realized
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
    strTitle = "Control vs Treatment: Emax Responses over Visits",
    bShowIndividuals = TRUE
)

lPlotResults
