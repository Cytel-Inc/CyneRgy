#' @name PlotTreatmentControlCI
#' @title Plot Treatment vs Control Mean Responses with 95% Confidence Interval
#' @description This function generates a ggplot comparing mean responses between treatment and control groups across visits,
#' including 95% confidence intervals.
#' @param SimData Data frame with subject data generated in current simulation with one row per patient. This dataframe contains
#' response variables (`Response1`, `Response2`, ..., `ResponseN`) and arrival times (`ArrTimeVisit1`, ..., `ArrTimeVisitN`), 
#' where N is the number of visits, as well as a `TreatmentID` column.
#' @return A `ggplot` object showing mean responses ±95% CI for control and treatment groups across visits.
#' @export
######################################################################################################################## .

PlotTreatmentControlCI <- function( SimData ) {
    
    library( dplyr )
    library( tidyr )
    library( ggplot2 )
    
    dfSummary <- SimData %>%
        mutate( id = row_number( ) ) %>%
        
        pivot_longer( cols          = matches( "^(Response|ArrTimeVisit)\\d+$" ),
                      names_to      = c( ".value", "Visit" ),
                      names_pattern = "(Response|ArrTimeVisit)(\\d+)" ) %>%
        
        mutate( Visit     = as.integer( Visit ),
                Treatment = factor(TreatmentID,
                                   levels = c( 0, 1 ),
                                   labels = c( "Control", "Treatment" ) ) ) %>%
        
        group_by( Visit, Treatment ) %>%
        
        summarise( Mean  = mean( Response, na.rm = TRUE ),
                   SE    = sd( Response,   na.rm = TRUE ) / sqrt( n( ) ),
                   .groups = "drop" ) %>%
        
        mutate( Lower = Mean - 1.96 * SE,
                Upper = Mean + 1.96 * SE )
    
    
    Plot <- ggplot( dfSummary, aes( x = Visit, y = Mean, color = Treatment, fill = Treatment ) ) +
        geom_ribbon( aes( ymin = Lower, ymax = Upper ), alpha = 0.2, color = NA ) +
        geom_line( size = 1.2 ) +
        geom_point( size = 3, shape = 21, color = "white", stroke = 1 ) +
        scale_color_manual( values = c( Control = "dodgerblue", Treatment = "hotpink" ) ) +
        scale_fill_manual(  values = c( Control = "dodgerblue", Treatment = "hotpink" ) ) +
        scale_x_continuous( breaks = unique( dfSummary$Visit ) ) +
        
        labs( x     = "Visit Number",
              y     = "Mean Response",
              title = "Control vs Treatment: Mean Response ±95% CI" ) +
        
        theme_minimal( base_size = 14 ) +
        
        theme( legend.position   = "bottom",
               legend.direction  = "horizontal",
               panel.grid.minor  = element_blank( ),
               panel.grid.major  = element_line( color = "gray90" ),
               axis.ticks        = element_line( color = "gray70" ),
               plot.title        = element_text( face = "bold", size = 16 ),
               legend.key        = element_blank( ) )
    
    return( Plot )
}
