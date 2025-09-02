# ---- Packages ----
suppressPackageStartupMessages({
    library(ggplot2)
    library(dplyr)
    library(tidyr)
    library(stringr)
    library(purrr)
})

# Source the desired files
source("PKPDResponseGeneration.R")

# ---- Helper to turn return list into a tidy data frame ----
emax_to_long <- function( emax_retval, TreatmentID, VisitTime = NULL ) {
    # grab all "Response<j>" entries
    resp_names <- names( emax_retval )[ str_detect( names( emax_retval ), "^Response\\d+$" )]
    if ( length( resp_names ) == 0 ) stop( "No Response<j> elements found in result." )
    
    # bind to wide matrix: rows = subjects, cols = visits
    resp_mat <- do.call( cbind, emax_retval[ resp_names ])
    colnames( resp_mat ) <- resp_names
    
    df <- as.data.frame( resp_mat ) |>
        mutate( Subject = row_number(),
               Group   = ifelse( TreatmentID == 0, "Control", "Treatment" )) |>
        pivot_longer( cols = starts_with( "Response" ),
                     names_to = "Visit",
                     values_to = "Response" ) |>
        mutate( VisitIndex = as.integer( str_replace( Visit, "Response", "" ))) |>
        arrange( Subject, VisitIndex )
    
    # attach actual times if provided
    if ( !is.null( VisitTime )) {
        stopifnot( length( unique( df$VisitIndex )) == length( VisitTime ))
        df <- df |> mutate( VisitTime = VisitTime[ VisitIndex ])
    } else {
        df <- df |> mutate( VisitTime = VisitIndex )
    }
    df
}

# ---- Plot function: means with 95% CI, by group across visits ----
plot_emax_groups <- function( df, title = "Emax Response by Group over Visits",
                             show_individuals = TRUE ) {
    summary_df <- df |>
        group_by( Group, VisitTime ) |>
        summarise( n = n(),
                  mean = mean( Response, na.rm = TRUE ),
                  sd   = sd( Response,   na.rm = TRUE ),
                  se   = sd / sqrt( n ),
                  ci   = 1.96 * se,
                  .groups = "drop" )
    
    p <- ggplot() +
        # optional faint individual trajectories
        { if ( show_individuals )
            geom_line( data = df,
                      aes( x = VisitTime, y = Response, group = interaction(Subject, Group), color = Group ),
                      alpha = 0.15 )
            else NULL } +
        # CI ribbons
        geom_ribbon( data = summary_df,
                    aes( x = VisitTime, ymin = mean - ci, ymax = mean + ci, fill = Group ),
                    alpha = 0.2 ) +
        # Means
        geom_line( data = summary_df,
                  aes( x = VisitTime, y = mean, color = Group ),
                  linewidth = 1.2 ) +
        geom_point( data = summary_df,
                   aes( x = VisitTime, y = mean, color = Group ),
                   size = 2 ) +
        labs( title = title,
             x = "Visit Time",
             y = "Response ( Emax model output )",
             color = "Group", fill = "Group" ) +
        theme_minimal( base_size = 12 )
    
    print( p ) 
    invisible( list( summary = summary_df, plot = p ))
}

# ---- Example usage ----
# Define a small scenario to demonstrate plot
set.seed( 123 )
NumSub      <- 60
NumVisit    <- 5
VisitTime   <- c( 1, 2, 3, 4, 5 )
TreatmentID <- sample( c( 0,1 ), NumSub, replace = TRUE, prob = c( 0.5, 0.5 ))

MeanControl   <- c( 10, 10, 10, 10, 10 )      # placeholders for control mean by visit
MeanTrt       <- c( 0,  0,  0,  0,  0 )      # not used in your Emax part, but required
StdDevControl <- rep( 5, NumVisit )
StdDevTrt     <- rep( 5, NumVisit )
CorrMat       <- diag( NumVisit )             # placeholder (not used in your current code)

UserParam <- list(
    E0   = 5,     # baseline
    Emax = 40,    # max drug effect
    EC50 = 50     # conc where 50% Emax realized
)

# Run simulator
emax_out <- GenerateResponseEmaxModel(
    NumSub      = NumSub,
    NumVisit    = NumVisit,
    TreatmentID = TreatmentID,
    Inputmethod = 0,
    VisitTime   = VisitTime,
    MeanControl = MeanControl,
    MeanTrt     = MeanTrt,
    StdDevControl = StdDevControl,
    StdDevTrt     = StdDevTrt,
    CorrMat       = CorrMat,
    UserParam     = UserParam
)

# Build tidy frame and plot
df_emax <- emax_to_long( emax_out, TreatmentID, VisitTime )
plot_results <- plot_emax_groups( df_emax,
                                 title = "Control vs Treatment: Emax Responses over Visits",
                                 show_individuals = TRUE )

plot_results

