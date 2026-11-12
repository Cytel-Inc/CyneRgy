######################################################################################################################## .
# Regression checks for confidence intervals in the plotting examples.
######################################################################################################################## .

test_that( "example confidence intervals count observed responses", {
    skip_if_not_installed( "dplyr" )
    skip_if_not_installed( "tidyr" )
    skip_if_not_installed( "tidyselect" )

    envPlots <- new.env()
    strExamples <- system.file( "Examples", package = "CyneRgy" )
    sys.source( file.path( strExamples, "SchizophreniaTrial", "R", "PlotTreatmentControlCI.R" ), envPlots )
    dfResponses <- data.frame( TreatmentID = rep( 0:1, each = 3 ),
        Response1 = c( 1, 3, NA, 2, 4, NA ) )
    cPlot <- envPlots$PlotTreatmentControlCI( dfResponses )
    expect_equal( cPlot$data$SE, c( 1, 1 ) )
    expect_equal( cPlot$data$Upper - cPlot$data$Mean, c( 1.96, 1.96 ) )

    # Evaluate only the plotting helper; leave the demonstration scenario for interactive use.
    vExpressions <- parse( file.path( strExamples, "PKPDResponseGeneration", "R", "PlotEmax.R" ) )
    for ( expr in vExpressions ) {
        if ( is.call( expr ) && identical( expr[[ 1 ]], as.name( "<-" ) ) &&
            identical( expr[[ 2 ]], as.name( "PlotEmaxGroups" ) ) ) {
            eval( expr, envir = envPlots )
        }
    }
    dfLong <- data.frame( Subject = seq_len( 6 ), Group = rep( c( "Control", "Treatment" ), each = 3 ),
        VisitTime = 1, Response = dfResponses$Response1 )
    strPdf <- tempfile( fileext = ".pdf" )
    grDevices::pdf( strPdf )
    on.exit( {
        grDevices::dev.off()
        unlink( strPdf )
    }, add = TRUE )
    lPlot <- envPlots$PlotEmaxGroups( dfLong, strTitle = "Observed responses", bShowIndividuals = FALSE )
    expect_equal( lPlot$summary$n, c( 2L, 2L ) )
    expect_equal( lPlot$summary$se, c( 1, 1 ) )
} )
