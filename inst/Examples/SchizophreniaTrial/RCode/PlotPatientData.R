PlotSelectedPatients <- function(SimData, DesignParam, vPatientIDs) {
    
    # ————————————————————————————————————————————————————————————— 
    # Load Libraries
    # ————————————————————————————————————————————————————————————— 
    library(dplyr)
    library(tidyr)
    library(ggplot2)
    library(RColorBrewer)
    
    # ————————————————————————————————————————————————————————————— 
    # 1) Reshape wide → long
    # ————————————————————————————————————————————————————————————— 
    dfLong <- SimData %>%
        mutate(id = row_number()) %>%
        pivot_longer(
            cols          = matches("^(Response|ArrTimeVisit)\\d+$"),
            names_to      = c(".value", "Visit"),
            names_pattern = "(Response|ArrTimeVisit)(\\d+)"
        ) %>%
        mutate(
            Visit     = as.integer(Visit),
            Treatment = factor(TreatmentID, levels = c(0,1), labels = c("Control","Treatment"))
        )
    
    # ————————————————————————————————————————————————————————————— 
    # 2) Filter to selected patients & order
    # ————————————————————————————————————————————————————————————— 
    vPatientIDs <- sort(unique(vPatientIDs))
    dfPlot <- dfLong %>%
        filter(id %in% vPatientIDs) %>%
        mutate(idFactor = factor(id, levels = vPatientIDs))
    
    # ————————————————————————————————————————————————————————————— 
    # 3) Build per-patient legend labels and attach to data
    #     (e.g., "12 (Control)")
    # ————————————————————————————————————————————————————————————— 
    dfKey <- dfPlot %>%
        distinct(idFactor, Treatment) %>%
        arrange(idFactor) %>%
        mutate(PatientLabel = paste0(as.character(idFactor), " (", as.character(Treatment), ")"))
    
    dfPlot <- dfPlot %>%
        left_join(dplyr::select(dfKey, idFactor, PatientLabel), by = "idFactor") %>%
        mutate(PatientLabel = factor(PatientLabel, levels = dfKey$PatientLabel))
    
    # ————————————————————————————————————————————————————————————— 
    # 4) Patient colors (unique per patient) + fill colors (by treatment)
    #    Note: brewer.pal requires at least 3; guard for n < 3
    # ————————————————————————————————————————————————————————————— 
    nPats <- length(vPatientIDs)
    vPatCols <- if (nPats <= 8) {
        brewer.pal(max(3, nPats), "Set1")[seq_len(nPats)]
    } else {
        rainbow(nPats)
    }
    # color scale keyed by patient label (unique per patient)
    names(vPatCols) <- dfKey$PatientLabel
    
    # fill scale keyed by patient label, but value chosen by that patient's treatment
    vFillVals <- setNames(
        ifelse(dfKey$Treatment == "Control", "dodgerblue", "hotpink"),
        dfKey$PatientLabel
    )
    
    # —————————————————————————————————————————————————————————————
    # 5) Plot (both color and fill map to PatientLabel → merged legend)
    # —————————————————————————————————————————————————————————————
    p <- ggplot(dfPlot, aes(x = Visit, y = Response, group = idFactor, color = PatientLabel)) +
        geom_line(size = 1.1) +
        geom_point(aes(fill = PatientLabel), shape = 21, size = 3, stroke = 0.8, color = "white") +
        scale_color_manual(
            name   = "Patient (Treatment)",
            values = vPatCols,
            breaks = dfKey$PatientLabel,
            labels = dfKey$PatientLabel
        ) +
        scale_fill_manual(
            name   = "Patient (Treatment)",  # same name + same breaks/labels → single merged legend
            values = vFillVals,
            breaks = dfKey$PatientLabel,
            labels = dfKey$PatientLabel
        ) +
        scale_x_continuous(breaks = unique(dfPlot$Visit)) +
        labs(
            x     = "Visit Number",
            y     = "Response",
            title = "Individual Patient Trajectories"
        ) +
        theme_minimal(base_size = 14) +
        theme(
            legend.position     = "bottom",
            legend.direction    = "horizontal",
            panel.grid.minor    = element_blank(),
            panel.grid.major    = element_line(color = "gray90"),
            axis.ticks          = element_line(color = "gray70"),
            plot.title          = element_text(face = "bold", size = 16),
            legend.key          = element_blank()
        ) +
        guides(
            color = guide_legend(order = 1),
            fill  = guide_legend(order = 1, override.aes = list(shape = 21, stroke = 0.8))
        )
    
    
    return(p)
}

