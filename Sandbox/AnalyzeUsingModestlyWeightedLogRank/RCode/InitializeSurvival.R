# Template for Initialization function
InitializeSurvival <- function(Seed)
{
    # TO DO : Modify this function appropriately
    
    Error <- 0
    set.seed(Seed)
    
    library( survival )
    library( gsdelayed )
    
    #TODO Define gDesign
    
    recruitment_mwlrt <- list(n_0 = 220, n_1 = 220, r_period = 12, k = 1)
    model_ph <- list(change_points = c(6),
                     lambdas_0 = c(log(2) / 9, log(2) / 9),
                     lambdas_1 = c(log(2) / 16, log(2) / 16))
    model = list(change_points = c(6),
                 lambdas_0 = c(log(2) / 9, log(2) / 9),
                 lambdas_1 = c(log(2) / 9, log(2) / 16))
    
    two_stage_mwlrt <- gsdelayed::two_stage_design(t_star = 12,
                                                   model = model,
                                                   recruitment = recruitment_mwlrt,
                                                   dco_int = 18,
                                                   dco_final = 30,
                                                   alpha_spend_f = ldobf,
                                                   alpha_one_sided = 0.025)
    gDesign <<- two_stage_mwlrt
    # User may use other options in set.seed like setting 
    # the Random Number Generator
    # User may also initialize Global Variables or set up 
    # the working directory etc. 
    # Do the error handling Modify Error appropriately 
    
    return(as.integer(Error))
}


InitializeSurvival3Stage <- function(Seed)
{
    # TO DO : Modify this function appropriately
    
    Error <- 0
    set.seed(Seed)
    
    library( survival )
    library(gsDesign)
    library(gsDesign2)
    
    #TODO Define gDesign
    
    
    
    enroll_rate <- define_enroll_rate(duration = 12, rate = 450 / 12)
    
    fail_rate <- define_fail_rate(
        duration = c(6, Inf),
        fail_rate = log(2) / 9,
        hr = c(1, 0.5625),
        dropout_rate = 0
    )
    
    
    dm <- gs_design_wlr(
        enroll_rate = enroll_rate,
        fail_rate = fail_rate,
        ratio = 1,
        #alpha = 0.025,  
        weight = function(x, arm0, arm1) {
            wlr_weight_mb(x, arm0, arm1, tau = 12, wmax = Inf )
        },
        
        info_scale = "h0_h1_info",
        upper = gs_spending_bound,
        upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
        lower = gs_b,
        lpar = rep(-Inf, 3),
        beta = 0.0866606,
        analysis_time = c(18, 24, 30)
    )
    
    gvTimeOfAnalysis <<- c( 18, 24, 30)
    gvBounds <<- dm$bounds$z # Note, the <<- makes this a global variable. 
    

    
    return(as.integer(Error))
}
