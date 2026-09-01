######################################################################################################################## .
#' @name PlotSelectedPatients
#' @title Plot Individual Patient Trajectories Across Visits
#' @description
#' Generates a plot of response trajectories for selected patients across visits,
#' with color-coded points indicating treatment assignment.
#' @author Jacob Wathen
#' @param SimData Data frame containing subject data generated in the current simulation, with one row per subject. Access variables by column name; optional outputs from response generation and dropout are also available as columns.
#'   \describe{
#'     \item{TreatmentID}{Treatment assignment, where 0 represents control and 1 represents experimental treatment.}
#'     \item{Response1, ..., ResponseNumVisit}{Subject response at each visit.}
#'     \item{ArrTimeVisit1, ..., ArrTimeVisitNumVisit}{Visit times relative to subject arrival.}
#'   }
#' @param vPatientIDs Integer vector. IDs of patients to include in the plot.
#' @return A `ggplot` object displaying individual patient response trajectories across visits.
######################################################################################################################## .

PlotSelectedPatients <- function( SimData, vPatientIDs )
{

    # Reshape data from wide to long
    dfLong <- SimData |>
        dplyr::mutate( id = dplyr::row_number() ) |>

        tidyr::pivot_longer( cols          = tidyselect::matches( "^(Response|ArrTimeVisit)\\d+$" ),
                             names_to      = c( ".value", "Visit" ),
                             names_pattern = "(Response|ArrTimeVisit)(\\d+)" ) |>

        dplyr::mutate( Visit     = as.integer( Visit ),
                       Treatment = factor( TreatmentID,
                                           levels = c( 0, 1 ),
                                           labels = c( "Control", "Treatment" ) ) )

    # Filter to selected patients & order
    vPatientIDs <- sort( unique( vPatientIDs ) )

    dfPlot <- dfLong |>
        dplyr::filter( id %in% vPatientIDs ) |>
        dplyr::mutate( idFactor = factor( id, levels = vPatientIDs ) )

    # Build per-patient legend labels and attach to data
    dfKey <- dfPlot |>
        dplyr::distinct( idFactor, Treatment ) |>
        dplyr::arrange( idFactor ) |>
        dplyr::mutate( PatientLabel = paste0( as.character( idFactor ), " (", as.character( Treatment ), ")" ) )

    dfPlot <- dfPlot |>
        dplyr::left_join( dplyr::select( dfKey, idFactor, PatientLabel ), by = "idFactor" ) |>
        dplyr::mutate( PatientLabel = factor( PatientLabel, levels = dfKey$PatientLabel ) )

    # Patient colors (unique per patient) + fill colors (by treatment)
    nPats <- length( vPatientIDs )

    vPatCols <- if( nPats <= 8 )
    {
        RColorBrewer::brewer.pal( max( 3, nPats ), "Set1" )[ seq_len( nPats ) ]
    }
    else
    {
        rainbow( nPats )
    }

    # Color scale keyed by patient label (unique per patient)
    names( vPatCols ) <- dfKey$PatientLabel

    # Fill scale keyed by patient label, but value chosen by that patient's treatment
    vFillVals <- setNames( ifelse( dfKey$Treatment == "Control", "dodgerblue", "hotpink" ),
                           dfKey$PatientLabel )

    # Plot (both color and fill map to PatientLabel → merged legend)
    Plot <- ggplot2::ggplot( dfPlot, ggplot2::aes( x = Visit, y = Response, group = idFactor, color = PatientLabel ) ) +
        ggplot2::geom_line( size = 1.1 ) +
        ggplot2::geom_point( ggplot2::aes( fill = PatientLabel ), shape = 21, size = 3, stroke = 0.8, color = "white" ) +

        ggplot2::scale_color_manual( name   = "Patient (Treatment)",
                         values = vPatCols,
                         breaks = dfKey$PatientLabel,
                         labels = dfKey$PatientLabel ) +

        ggplot2::scale_fill_manual( name   = "Patient (Treatment)",
                        values = vFillVals,
                        breaks = dfKey$PatientLabel,
                        labels = dfKey$PatientLabel ) +

        ggplot2::scale_x_continuous( breaks = unique( dfPlot$Visit ) ) +

          ggplot2::labs( x     = "Visit Number",
                     y     = "Response",
                     title = "Individual Patient Trajectories" ) +

        ggplot2::theme_minimal( base_size = 14 ) +

        ggplot2::theme( legend.position     = "bottom",
                legend.direction    = "horizontal",
                panel.grid.minor    = ggplot2::element_blank(),
                panel.grid.major    = ggplot2::element_line( color = "gray90" ),
                axis.ticks          = ggplot2::element_line( color = "gray70" ),
                plot.title          = ggplot2::element_text( face = "bold", size = 16 ),
                legend.key          = ggplot2::element_blank() ) +

        ggplot2::guides( color = ggplot2::guide_legend( order = 1 ),
                 fill  = ggplot2::guide_legend( order = 1, override.aes = list( shape = 21, stroke = 0.8 ) ) )

    return( Plo )
}
