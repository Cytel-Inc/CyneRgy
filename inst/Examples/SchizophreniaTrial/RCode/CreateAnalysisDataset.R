CreateAnalysisDataset <- function( SimData, LookInfo )
{
    if (!is.null(LookInfo)) {
        nQtyOfLooks          <- LookInfo$NumLooks
        nLookIndex           <- LookInfo$CurrLookIndex
        nQtyOfPatsForInterim <- LookInfo$CumCompleters[nLookIndex]
        nAnalysisVisit       <- LookInfo$InterimVisit
    } else {
        nLookIndex           <- 1
        nQtyOfLooks          <- 1
        nQtyOfPatsForInterim <- nrow(SimData)
    }
    # —————————————————————————————————————————————————————————————
    # Step 2: Reshape wide → long in one shot
    # —————————————————————————————————————————————————————————————
    dfLongData <- SimData %>%
        mutate(id = row_number()) %>%
        pivot_longer(
            cols          = matches("^(Response|ArrTimeVisit)\\d+$"),
            names_to      = c(".value", "Visit"),
            names_pattern = "(Response|ArrTimeVisit)(\\d+)"
        ) %>%
        mutate(
            Visit             = as.integer(Visit),
            CalendarVisitTime = ArrivalTime + ArrTimeVisit
        ) %>%
        dplyr::select(id, TreatmentID, Visit, Response, CalendarVisitTime) %>%
        arrange(Visit, CalendarVisitTime)
    
    # —————————————————————————————————————————————————————————————
    # Step 3: Interim‐look filtering using dplyr
    # —————————————————————————————————————————————————————————————
    if (!is.null(LookInfo)) {
        
        # —————————————————————————————————————————————————————————————
        # 3a) compute cutoff time
        # —————————————————————————————————————————————————————————————
        
        dAnalysisTime <- dfLongData %>%
            filter(Visit == nAnalysisVisit) %>%
            slice(nQtyOfPatsForInterim) %>%
            pull(CalendarVisitTime)
        
        # —————————————————————————————————————————————————————————————
        # 3b) pick subjects
        # —————————————————————————————————————————————————————————————
        
        
        if (LookInfo$IncludePipeline == 0) {
            vSubjectsForAnalysis <- dfLongData %>%
                filter(
                    Visit == nAnalysisVisit,
                    CalendarVisitTime <= dAnalysisTime
                ) %>%
                distinct(id) %>%
                pull(id)
        } else {
            vSubjectsForAnalysis <- dfLongData %>%
                filter(CalendarVisitTime <= dAnalysisTime) %>%
                distinct(id) %>%
                pull(id)
        }
        
        dfAnalysisData <- dfLongData %>%
            filter(id %in% vSubjectsForAnalysis)
    } else {
        dfAnalysisData <- dfLongData
    }
    
    # —————————————————————————————————————————————————————————————
    # Step 4: Prepare for MMRM
    # —————————————————————————————————————————————————————————————
    dfAnalysisData <- dfAnalysisData %>%
        mutate(
            Visit       = factor(Visit),
            TreatmentID = factor(TreatmentID),
            id          = factor(id)
        )
    
    # Create a dataset that removes the baseline visit (Visit == 1) from the long form and adds the baseline response as a new column
    dfNoBaselineAnalysisData <- dplyr::filter( dfAnalysisData, Visit != 1)
    dfBaselineAnalysisData   <- dplyr::filter( dfAnalysisData, Visit == 1) %>% select( id, Baseline = Response)
    dfNoBaselineAnalysisData <- dplyr::left_join(dfNoBaselineAnalysisData,dfBaselineAnalysisData, by = "id" )
    
    return( dfNoBaselineAnalysisData )
}