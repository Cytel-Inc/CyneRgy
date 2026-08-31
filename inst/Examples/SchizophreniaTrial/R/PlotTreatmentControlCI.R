######################################################################################################################## .
#' @name PlotTreatmentControlCI
#' @title Plot Treatment vs Control Mean Responses with 95% Confidence Interval
#' @description
#' Compares mean responses between treatment and control groups across visits,
#' including 95 percent confidence intervals.
#' @author Jacob Wathen
#' @param SimData Data frame with subject data generated in current simulation with one row per patient. This data frame contains
#' response variables (`Response1`, `Response2`, ..., `ResponseN`) and arrival times (`ArrTimeVisit1`, ..., `ArrTimeVisitN`),
#' where N is the number of visits, as well as a `TreatmentID` column.
#' @return A `ggplot` object showing group means and 95 percent confidence intervals.
######################################################################################################################## .

PlotTreatmentControlCI <- function( SimData )
{

    dfSummary <- SimData |>
        dplyr::mutate( id = dplyr::row_number() ) |>

        tidyr::pivot_longer( cols          = tidyselect::matches( "^(Response|ArrTimeVisit)\\d+$" ),
                             names_to      = c( ".value", "Visit" ),
                             names_pattern = "(Response|ArrTimeVisit)(\\d+)" ) |>

        dplyr::mutate( Visit     = as.integer( Visit ),
                       Treatment = factor( TreatmentID,
                                           levels = c( 0, 1 ),
                                           labels = c( "Control", "Treatment" ) ) ) |>

        dplyr::group_by( Visit, Treatment ) |>

        dplyr::summarise( Mean    = mean( Response, na.rm = TRUE ),
                          SE      = stats::sd( Response, na.rm = TRUE ) / sqrt( dplyr::n() ),
                          .groups = "drop" ) |>

        dplyr::mutate( Lower = Mean - 1.96 * SE,
                       Upper = Mean + 1.96 * SE )

    Plot <- ggplot2::ggplot( dfSummary, ggplot2::aes( x = Visit, y = Mean, color = Treatment, fill = Treatment ) ) +
        ggplot2::geom_ribbon( ggplot2::aes( ymin = Lower, ymax = Upper ), alpha = 0.2, color = NA ) +
        ggplot2::geom_line( size = 1.2 ) +
        ggplot2::geom_point( size = 3, shape = 21, color = "white", stroke = 1 ) +
        ggplot2::scale_color_manual( values = c( Control = "dodgerblue", Treatment = "hotpink" ) ) +
        ggplot2::scale_fill_manual( values = c( Control = "dodgerblue", Treatment = "hotpink" ) ) +
        ggplot2::scale_x_continuous( breaks = unique( dfSummary$Visit ) ) +

        ggplot2::labs( x     = "Visit Number",
                       y     = "Mean Response",
                       title = "Control vs Treatment: Mean Response ±95% CI" ) +

        ggplot2::theme_minimal( base_size = 14 ) +

        ggplot2::theme( legend.position  = "bottom",
                legend.direction = "horizontal",
                panel.grid.minor = ggplot2::element_blank(),
                panel.grid.major = ggplot2::element_line( color = "gray90" ),
                axis.ticks       = ggplot2::element_line( color = "gray70" ),
                plot.title       = ggplot2::element_text( face = "bold", size = 16 ),
                legend.key       = ggplot2::element_blank() )

      return( Plot )
}
