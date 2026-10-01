######################################################################################################################## .
#' @name PlotTreatmentControlCI
#'
#' @title Plot Treatment vs Control Mean Responses with 95\% Confidence Interval
#'
#' @description This function generates a ggplot comparing mean responses between treatment and control groups
#'   across visits, including 95\% confidence intervals.
#'
#' @author Jacob Wathen
#'
#' @param SimData Data frame of subject-level data for the current simulation, with one row per subject. Access
#'   columns by name, for example `SimData$ArrivalTime`. Columns include the native fields below when applicable,
#'   plus any custom outputs from enrollment, randomization, response, or dropout generation.
#' \describe{
#'   \item{ArrivalTime}{Numeric vector of subject arrival times on the calendar scale, with one element per
#'     subject, in the same order as TreatmentID.}
#'   \item{TreatmentID}{Integer vector of treatment assignments, with one element per subject: 0 = placebo/control,
#'     1 = first experimental arm, 2 = second experimental arm, and so on.}
#'   \item{Response1, ..., ResponseNumVisit}{Numeric response vectors, one per visit, with one element per subject.
#'     Replace NumVisit by the actual number of visits.}
#'   \item{CensorInd1, ..., CensorIndNumVisit}{Integer censor-indicator vectors, one per visit: 0 =
#'     dropout/non-completer; 1 = completer.}
#'   \item{DropOutTime}{Numeric vector of generated dropout times measured from each subject's enrollment, with one
#'     element per subject. Inf indicates no dropout.}
#'   \item{DropoutVisitID}{Integer vector of 1-based visit IDs after which subjects drop out, with one element per
#'     subject.}
#'   \item{ArrTimeVisit[VisitID]}{Optional custom numeric vector of visit times measured from each subject's
#'     enrollment, with one element per subject. Replace VisitID by the actual visit number. Add ArrivalTime to
#'     obtain calendar visit times.}
#' }
#'
#' @return A ggplot object showing arm-specific mean responses and 95\% confidence intervals by visit.
######################################################################################################################## .

PlotTreatmentControlCI <- function( SimData ) {
    dfSummary <- SimData |>
        dplyr::mutate( id = dplyr::row_number( ) ) |>
        tidyr::pivot_longer(
            cols = tidyselect::matches( "^(Response|ArrTimeVisit)\\d+$" ),
            names_to = c( ".value", "Visit" ),
            names_pattern = "(Response|ArrTimeVisit)(\\d+)"
        ) |>
        dplyr::mutate(
            Visit = as.integer( Visit ),
            Treatment = factor( TreatmentID,
                levels = c( 0, 1 ),
                labels = c( "Control", "Treatment" )
            )
        ) |>
        dplyr::group_by( Visit, Treatment ) |>
        dplyr::summarise(
            Mean = mean( Response, na.rm = TRUE ),
            SE = stats::sd( Response, na.rm = TRUE ) / sqrt( sum( !is.na( Response ) ) ),
            .groups = "drop"
        ) |>
        dplyr::mutate(
            Lower = Mean - 1.96 * SE,
            Upper = Mean + 1.96 * SE
        )

    cPlot <- ggplot2::ggplot( dfSummary, ggplot2::aes( x = Visit, y = Mean, color = Treatment, fill = Treatment ) ) +
        ggplot2::geom_ribbon( ggplot2::aes( ymin = Lower, ymax = Upper ), alpha = 0.2, color = NA ) +
        ggplot2::geom_line( linewidth = 1.2 ) +
        ggplot2::geom_point( size = 3, shape = 21, color = "white", stroke = 1 ) +
        ggplot2::scale_color_manual( values = c( Control = "dodgerblue", Treatment = "hotpink" ) ) +
        ggplot2::scale_fill_manual( values = c( Control = "dodgerblue", Treatment = "hotpink" ) ) +
        ggplot2::scale_x_continuous( breaks = unique( dfSummary$Visit ) ) +
        ggplot2::labs(
            x = "Visit Number",
            y = "Mean Response",
            title = "Control vs Treatment: Mean Response ±95% CI"
        ) +
        ggplot2::theme_minimal( base_size = 14 ) +
        ggplot2::theme(
            legend.position = "bottom",
            legend.direction = "horizontal",
            panel.grid.minor = ggplot2::element_blank( ),
            panel.grid.major = ggplot2::element_line( color = "gray90" ),
            axis.ticks = ggplot2::element_line( color = "gray70" ),
            plot.title = ggplot2::element_text( face = "bold", size = 16 ),
            legend.key = ggplot2::element_blank( )
        )

    return( cPlot )
}
