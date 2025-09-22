####################################################################################################
#   Program/Function Name: CreateExampleFlowchart
#   Author: Gabriel Potvin
#   Last Modified Date: 2025/09/22
####################################################################################################

#' @name CreateExampleFlowchart
#' @title Generate a Flowchart for a CyneRgy Example
#'
#' @description This function creates a flowchart of the used integration points 
#' and main steps of a CyneRgy R Integration example.
#' Used integration points are highlighted with wider columns and custom step boxes, 
#' while unused points remain gray placeholders. Steps are automatically wrapped to fit.
#'
#' @param lUsedPoints A named list where each name is an integration point (e.g., "Response") 
#'        and each element is a character vector of step labels for that integration point.
#'        Options: "Initialization", "Enrollment", "Randomization", "Dropout", "Treatment\verb{\\n}Selection", "Response", "Analysis"
#' @param nBoxHeight Numeric. Base height of each step box. Default = 0.7.
#' @param nBoxSpacing Numeric. Vertical spacing between boxes. Default = 0.3.
#' @param nColumnWidth Numeric. Width of unused integration point columns. Default = 0.5.
#' @param nBigColWidth Numeric. Width of used integration point columns. Default = 3.
#'
#' @return A ggplot object containing the flowchart visualization.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example with one used integration point
#' p1 <- CreateExampleFlowchart(
#'   lUsedPoints = list(
#'     "Response" = c(
#'       "Load MAV, TV, and confidence level",
#'       "Run proportions test (treatment > control)",
#'       "Calculate CI lower and upper limits",
#'       "Return decision using CI thresholds"
#'     )
#'   )
#' )
#' p1
#'
#' # Example with two integration points
#' p2 <- CreateExampleFlowchart(
#'   lUsedPoints = list(
#'     "Response" = c(
#'       "Load MAV, TV, and confidence level",
#'       "Run proportions test (treatment > control)"
#'     ),
#'     "Analysis" = c(
#'       "Analyze PFS using Cox regression",
#'       "Analyze OS using Cox regression"
#'     )
#'   )
#' )
#' p2
#' }
####################################################################################################

CreateExampleFlowchart <- function(lUsedPoints = list(),
                                   nBoxHeight = 0.7,
                                   nBoxSpacing = 0.3,
                                   nColumnWidth = 0.5,
                                   nBigColWidth = 3) {
    
    # Set max characters per line depending on number of used points
    nUsed <- length(lUsedPoints)
    nMaxCharsPerLine <- ifelse(nUsed == 1, 40, 30)
    nTitleSize <- ifelse(nUsed == 1, 3, 2)
    
    # Integration points order
    vIntegrationPoints <- c("Initialization", "Enrollment", "Randomization",
                            "Dropout", "Treatment\nSelection", "Response", "Analysis")
    
    # Step 1: Setup column positions
    nXStart <- 0
    dfColumns <- data.frame(
        xmin = numeric(),
        xmax = numeric(),
        ymin = numeric(),
        ymax = numeric(),
        label = character(),
        fill = character(),
        border = character(),
        textSize = numeric(),
        stringsAsFactors = FALSE
    )
    
    for (strPoint in vIntegrationPoints) {
        if (strPoint %in% names(lUsedPoints)) {
            nW <- nBigColWidth
            strFill <- "#cfe2ff"
        } else {
            nW <- nColumnWidth
            strFill <- "lightgray"
        }
        dfColumns <- rbind(dfColumns,
                           data.frame(
                               xmin = nXStart,
                               xmax = nXStart + nW,
                               ymin = NA,
                               ymax = NA,
                               label = strPoint,
                               fill = strFill,
                               border = strFill,
                               textSize = nTitleSize,
                               stringsAsFactors = FALSE
                           ))
        nXStart <- nXStart + nW + 0.5
    }
    
    # Step 2: Compute flowchart boxes
    lFlowcharts <- list()
    
    for (strPoint in names(lUsedPoints)) {
        vIdx <- which(dfColumns$label == strPoint)
        if (length(vIdx) == 0) next
        
        # Enlarge column
        dfColumns$xmax[vIdx] <- dfColumns$xmin[vIdx] + nBigColWidth
        dfColumns$fill[vIdx] <- "#cfe2ff"
        dfColumns$border[vIdx] <- "#cfe2ff"
        dfColumns$textSize[vIdx] <- dfColumns$textSize[vIdx] + 1
        
        # Wrap text for boxes
        vLabelsRaw <- lUsedPoints[[strPoint]]
        vLabelsWrapped <- sapply(vLabelsRaw, function(x) stringr::str_wrap(x, width = nMaxCharsPerLine))
        
        n <- length(vLabelsWrapped)
        vLinesPerBox <- sapply(strsplit(vLabelsWrapped, "\n"), length)
        vHeights <- nBoxHeight * vLinesPerBox
        
        # vertical stacking (top-down)
        nYTop <- 8.5
        vFlowYMax <- numeric(n)
        vFlowYMin <- numeric(n)
        
        for (i in seq_len(n)) {
            vFlowYMax[i] <- nYTop
            vFlowYMin[i] <- nYTop - vHeights[i]
            nYTop <- vFlowYMin[i] - nBoxSpacing
        }
        
        dfFlowchart <- data.frame(
            xmin = dfColumns$xmin[vIdx] + 0.2,
            xmax = dfColumns$xmax[vIdx] - 0.2,
            ymin = vFlowYMin,
            ymax = vFlowYMax,
            label = vLabelsWrapped,
            fill = "#cfe2ff",
            stringsAsFactors = FALSE
        )
        
        dfArrows <- data.frame(
            x = (dfColumns$xmin[vIdx] + dfColumns$xmax[vIdx]) / 2,
            xend = (dfColumns$xmin[vIdx] + dfColumns$xmax[vIdx]) / 2,
            y = head(dfFlowchart$ymin, -1),
            yend = tail(dfFlowchart$ymax, -1)
        )
        
        lFlowcharts[[strPoint]] <- list(boxes = dfFlowchart, arrows = dfArrows)
    }
    
    # Step 3: Adjust column vertical range
    if (length(lFlowcharts) > 0) {
        dfAllBoxes <- do.call(rbind, lapply(lFlowcharts, `[[`, "boxes"))
        dfColumns$ymin <- min(dfAllBoxes$ymin) - 0.5
        dfColumns$ymax <- max(dfAllBoxes$ymax) + 0.5
    } else {
        dfColumns$ymin <- 2
        dfColumns$ymax <- 8.5
    }
    
    # Step 4: Legend (bottom-right)
    nMaxX <- max(dfColumns$xmax)
    nMinY <- min(dfColumns$ymin)
    nLegendWidth <- 1
    nLegendHeight <- 0.3
    nLegendGap <- 0.2
    
    dfLegend <- data.frame(
        xmin = c(nMaxX - 2*nLegendWidth - nLegendGap, nMaxX - nLegendWidth),
        xmax = c(nMaxX - nLegendWidth - nLegendGap, nMaxX),
        ymin = rep(nMinY - nLegendHeight - 0.7, 2),
        ymax = rep(nMinY - 0.7, 2),
        fill = c("lightgray", "#cfe2ff"),
        label = c("Not Used", "Used")
    )
    
    # Step 5: Build ggplot
    p <- ggplot2::ggplot() +
        geom_rect(data = dfColumns,
                  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
                  color = dfColumns$border) +
        scale_fill_identity() +
        geom_text(data = dfColumns,
                  aes(x = (xmin + xmax)/2, y = ymax + 0.7, label = label, size = textSize),
                  vjust = 1) +
        scale_size_identity()
    
    # Flowchart boxes and arrows
    for (fc in lFlowcharts) {
        p <- p +
            geom_rect(data = fc$boxes,
                      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
                      color = "black") +
            geom_text(data = fc$boxes,
                      aes(x = (xmin + xmax)/2, y = (ymin + ymax)/2, label = label),
                      size = 2.5) +
            geom_curve(data = fc$arrows,
                       aes(x = x, y = y, xend = xend, yend = yend),
                       curvature = 0, arrow = arrow(length = unit(0.15, "cm")),
                       color = "black")
    }
    
    # Legend
    p <- p +
        geom_rect(data = dfLegend,
                  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
                  color = "black") +
        geom_text(data = dfLegend,
                  aes(x = (xmin + xmax)/2, y = ymin - 0.1, label = label),
                  size = 2.5, vjust = 1) +
        theme_void() +
        theme(panel.background = element_rect(fill = 'white', colour = 'white'))
    
    return(p)
}
