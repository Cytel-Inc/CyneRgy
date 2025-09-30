#------------------------------
# SortTrialsByRCode
# - Groups trials by R script hash
# - Produces one .R + one .rds per group
# - Creates NoRScript bucket if trials have no R code
#------------------------------
SortTrialsByRCode <- function(
        input_root,                 
        output_root = file.path(input_root, "grouped_trials"),
        recursive_trials = FALSE,   
        choose_name = c("script", "hash"),
        dry_run = FALSE,
        purge_output_root = TRUE
) {
    choose_name <- match.arg(choose_name)
    
    if (!dir.exists(input_root)) stop("input_root does not exist: ", input_root)
    
    #------------------------------
    # Prepare output root
    #------------------------------
    if (!dry_run && purge_output_root && dir.exists(output_root)) {
        unlink(output_root, recursive = TRUE, force = TRUE)
    }
    EnsureDir(output_root)
    
    #------------------------------
    # Step 1: Collect trial directories
    #------------------------------
    if (recursive_trials) {
        all_dirs <- list.dirs(input_root, recursive = TRUE, full.names = TRUE)
        trial_dirs <- Filter(function(d) {
            if (identical(normalizePath(d, winslash = "/", mustWork = FALSE),
                          normalizePath(output_root, winslash = "/", mustWork = FALSE))) return(FALSE)
            files_here <- list.files(d, recursive = FALSE)
            any(grepl("\\.[Rr]$", files_here)) || any(grepl("\\.rds$", files_here, ignore.case = TRUE))
        }, all_dirs)
    } else {
        trial_dirs <- list.dirs(input_root, full.names = TRUE, recursive = FALSE)
        trial_dirs <- setdiff(trial_dirs, input_root)
    }
    
    if (!length(trial_dirs)) {
        message("No trial folders found under: ", input_root)
        return(invisible(data.frame()))
    }
    
    #------------------------------
    # Step 2: Compute per-trial primary script + hash
    #------------------------------
    recs <- lapply(trial_dirs, function(d) {
        rfile <- FindPrimaryRScript(d)
        if (is.na(rfile)) {
            list(trial_dir = d, primary_script = NA_character_, script_name = NA_character_, code_hash = NA_character_)
        } else {
            h <- CodeHash(rfile)
            list(trial_dir = d, primary_script = rfile,
                 script_name = tools::file_path_sans_ext(basename(rfile)),
                 code_hash = h)
        }
    })
    df <- do.call(rbind.data.frame, lapply(recs, as.data.frame, stringsAsFactors = FALSE))
    names(df) <- c("trial_dir", "primary_script", "script_name", "code_hash")
    
    #------------------------------
    # Step 3: Group by hash
    #------------------------------
    df$group_key <- df$code_hash
    df_code  <- df[!is.na(df$group_key) & nzchar(df$group_key), , drop = FALSE]
    df_empty <- df[is.na(df$group_key) | !nzchar(df$group_key), , drop = FALSE]
    
    groups <- split(df_code, df_code$group_key)
    out_map <- list()
    for (k in names(groups)) {
        g <- groups[[k]]
        script_names <- g$script_name[!is.na(g$script_name) & nzchar(g$script_name)]
        if (!length(script_names)) script_names <- paste0("code_", substr(k, 1, 6))
        best_script <- names(sort(table(script_names), decreasing = TRUE))[1]
        base_name <- if (choose_name == "script") best_script else paste0("code_", substr(k, 1, 6))
        folder_name <- MakeSafeName(base_name)
        if (!is.null(out_map[[folder_name]])) {
            folder_name <- MakeSafeName(paste0(base_name, "__", substr(k, 1, 6)))
        }
        out_map[[folder_name]] <- list(
            hash = k,
            trials = g$trial_dir,
            rep_script = best_script
        )
    }
    
    #------------------------------
    # Step 4: Process groups
    #------------------------------
    manifest <- data.frame(
        group_folder     = character(0),
        code_hash_md5    = character(0),
        script_name      = character(0),
        combined_rds     = character(0),
        kept_script_path = character(0),
        n_source_rds     = integer(0),
        stringsAsFactors = FALSE
    )
    
    for (folder_name in names(out_map)) {
        g <- out_map[[folder_name]]
        group_dir <- file.path(output_root, folder_name)
        
        if (!dry_run) {
            if (dir.exists(group_dir)) PurgeDirContents(group_dir) else EnsureDir(group_dir)
        }
        
        # Representative script
        primary_scripts <- df$primary_script[df$trial_dir %in% g$trials]
        primary_scripts <- primary_scripts[!is.na(primary_scripts)]
        if (length(primary_scripts)) {
            sizes <- suppressWarnings(file.info(primary_scripts)$size)
            sizes[is.na(sizes)] <- -Inf
            rep_script_src <- primary_scripts[which.max(sizes)]
            target_rep <- file.path(group_dir, paste0(g$rep_script, ".R"))
            if (!dry_run) file.copy(rep_script_src, target_rep, overwrite = TRUE)
        } else {
            target_rep <- NA_character_
        }
        
        # Combine RDS from all trials
        rds_files <- unlist(lapply(
            g$trials,
            function(td) list.files(td, pattern = "\\.rds$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
        ), use.names = FALSE)
        
        if (length(rds_files)) {
            reads <- lapply(rds_files, SafeReadRDS)
            ok_idx <- which(vapply(reads, `[[`, logical(1), "ok"))
            if (length(ok_idx)) {
                vals <- lapply(reads[ok_idx], `[[`, "value")
                srcs <- rds_files[ok_idx]
                combined <- CombineRDSObjects(vals, srcs)
                out_rds <- file.path(group_dir, paste0(g$rep_script, ".rds"))
                if (!dry_run) saveRDS(combined, out_rds)
                n_src <- length(srcs)
            } else {
                out_rds <- NA_character_
                n_src <- 0L
                warning("No readable RDS files in group: ", group_dir)
            }
        } else {
            out_rds <- NA_character_
            n_src <- 0L
        }
        
        manifest[nrow(manifest) + 1L, ] <- list(folder_name, out_map[[folder_name]]$hash,
                                                g$rep_script, out_rds, target_rep, n_src)
        
        # Minimal output: only keep the two files
        if (!dry_run) {
            PurgeDirContents(group_dir)
            EnsureDir(group_dir)
            if (!is.na(target_rep) && exists("rep_script_src") && file.exists(rep_script_src)) {
                file.copy(rep_script_src, file.path(group_dir, paste0(g$rep_script, ".R")), overwrite = TRUE)
            }
            if (!is.na(out_rds) && exists("combined")) {
                saveRDS(combined, file.path(group_dir, paste0(g$rep_script, ".rds")))
            }
        }
    }
    
    #------------------------------
    # Step 5: NoRScript bucket
    #------------------------------
    if (nrow(df_empty)) {
        no_code_dir <- file.path(output_root, "NoRScript")
        if (!dry_run) {
            if (dir.exists(no_code_dir)) PurgeDirContents(no_code_dir) else EnsureDir(no_code_dir)
        }
        rds_nc <- unlist(lapply(
            df_empty$trial_dir,
            function(td) list.files(td, pattern = "\\.rds$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
        ), use.names = FALSE)
        
        if (length(rds_nc)) {
            reads <- lapply(rds_nc, SafeReadRDS)
            ok_idx <- which(vapply(reads, `[[`, logical(1), "ok"))
            if (length(ok_idx)) {
                vals <- lapply(reads[ok_idx], `[[`, "value")
                srcs <- rds_nc[ok_idx]
                combined_nc <- CombineRDSObjects(vals, srcs)
                out_nc <- file.path(no_code_dir, "NoRScript.rds")
                if (!dry_run) saveRDS(combined_nc, out_nc)
                
                manifest[nrow(manifest) + 1L, ] <- list("NoRScript", NA_character_, "NoRScript",
                                                        out_nc, NA_character_, length(srcs))
                
                if (!dry_run) {
                    PurgeDirContents(no_code_dir)
                    EnsureDir(no_code_dir)
                    saveRDS(combined_nc, out_nc)
                }
            } else if (!dry_run) {
                unlink(no_code_dir, recursive = TRUE, force = TRUE)
            }
        } else if (!dry_run) {
            unlink(no_code_dir, recursive = TRUE, force = TRUE)
        }
    }
    
    #------------------------------
    # Step 6: Write manifest
    #------------------------------
    utils::write.csv(manifest, file.path(output_root, "group_manifest.csv"), row.names = FALSE)
    message("Grouped ", length(out_map), " code bucket(s). NoRScript bucket ",
            if (nrow(df_empty)) "processed." else "not needed.",
            " Manifest: ",
            normalizePath(file.path(output_root, "group_manifest.csv"), mustWork = FALSE))
    
    return(invisible(manifest))
}

