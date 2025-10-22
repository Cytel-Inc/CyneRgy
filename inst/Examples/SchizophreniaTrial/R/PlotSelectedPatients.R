#' @name PlotSelectedPatients
#' @title Plot Individual Patient Trajectories Across Visits
#' @description This function generates a ggplot showing response trajectories for selected patients across visits, with 
#' color-coded points indicating treatment assignment.
#' @param SimData Data frame with subject data generated in current simulation with one row per patient. This dataframe contains
#' response variables (`Response1`, `Response2`, ..., `ResponseN`) and arrival times (`ArrTimeVisit1`, ..., `ArrTimeVisitN`), 
#' where N is the number of visits, as well as a `TreatmentID` column.
#' @param vPatientIDs Integer vector. IDs of patients to include in the plot.
#' @return A `ggplot` object displaying individual patient response trajectories across visits.
#' @export
######################################################################################################################## .

PlotSelectedPatients <- function(SimData, vPatientIDs ) {
    
    library( dplyr )
    library( tidyr )
    library( ggplot2 )
    library( RColorBrewer )
    
    # Reshape data from wide to long
    dfLong <- SimData %>%
        mutate( id = row_number( ) ) %>%
        
        pivot_longer( cols          = matches( "^(Response|ArrTimeVisit)\\d+$" ),
                      names_to      = c( ".value", "Visit" ),
                      names_pattern = "(Response|ArrTimeVisit)(\\d+)" ) %>%
        
        mutate( Visit     = as.integer( Visit ),
                Treatment = factor( TreatmentID,
                                    levels = c( 0, 1 ),
                                    labels = c( "Control", "Treatment" ) ) )
    
    # Filter to selected patients & order
    vPatientIDs <- sort( unique( vPatientIDs ) )
    
    dfPlot <- dfLong %>%
        filter( id %in% vPatientIDs ) %>%
        mutate( idFactor = factor( id, levels = vPatientIDs ) )
    
    # Build per-patient legend labels and attach to data
    dfKey <- dfPlot %>%
        distinct( idFactor, Treatment ) %>%
        arrange( idFactor ) %>%
        mutate( PatientLabel = paste0( as.character( idFactor ), " (", as.character( Treatment ), ")" ) )
    
    dfPlot <- dfPlot %>%
        left_join( select( dfKey, idFactor, PatientLabel ), by = "idFactor" ) %>%
        mutate( PatientLabel = factor( PatientLabel, levels = dfKey$PatientLabel ) )
    
    # Patient colors (unique per patient) + fill colors (by treatment)
    nPats <- length( vPatientIDs )
    
    vPatCols <- if ( nPats <= 8 ) 
    {
        brewer.pal( max( 3, nPats ), "Set1" )[seq_len( nPats )]
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
    Plot <- ggplot( dfPlot, aes( x = Visit, y = Response, group = idFactor, color = PatientLabel ) ) +
        geom_line( size = 1.1 ) +
        geom_point( aes( fill = PatientLabel ), shape = 21, size = 3, stroke = 0.8, color = "white" ) +
        
        scale_color_manual( name   = "Patient (Treatment)",
                            values = vPatCols,
                            breaks = dfKey$PatientLabel,
                            labels = dfKey$PatientLabel ) +
        
        scale_fill_manual( name   = "Patient (Treatment)",
                           values = vFillVals,
                           breaks = dfKey$PatientLabel,
                           labels = dfKey$PatientLabel ) +
        
        scale_x_continuous( breaks = unique( dfPlot$Visit ) ) +
        
        labs( x     = "Visit Number",
              y     = "Response",
              title = "Individual Patient Trajectories" ) +
        
        theme_minimal( base_size = 14 ) +
        
        theme( legend.position     = "bottom",
               legend.direction    = "horizontal",
               panel.grid.minor    = element_blank( ),
               panel.grid.major    = element_line( color = "gray90" ),
               axis.ticks          = element_line( color = "gray70" ),
               plot.title          = element_text( face = "bold", size = 16 ),
               legend.key          = element_blank( ) ) +
        
        guides( color = guide_legend( order = 1 ),
                fill  = guide_legend( order = 1, override.aes = list( shape = 21, stroke = 0.8 ) ) )
    
    return( Plot )
}

