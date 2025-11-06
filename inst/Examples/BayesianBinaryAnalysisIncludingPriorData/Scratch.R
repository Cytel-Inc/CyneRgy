# Requires: rjags, coda
# install.packages( c( "rjags", "coda" ) )

RunHierBinomJAGS <- function( vY, vN, nIter = 8000, nBurn = 3000, nChains = 3, nThin = 2, nAdapt = 1000, nSeed = 123 ) {
    # vY: integer vector of responders per study
    # vN: integer vector of totals per study (same length as vY)
    # Returns: list with MCMC samples and a summary data frame
    
    if ( length( vY ) != length( vN ) ) {
        stop( "vY and vN must have equal length" )
    }
    if ( any( vY < 0 ) || any( vN <= 0 ) || any( vY > vN ) ) {
        stop( "Counts must satisfy 0 <= vY[i] <= vN[i], with vN[i] > 0" )
    }
    
    set.seed( nSeed )
    suppressPackageStartupMessages( library( rjags ) )
    suppressPackageStartupMessages( library( coda ) )
    
    nStud <- length( vY )
    
    strModel <- "
  model {
    for (i in 1:Nstud) {
      y[i] ~ dbin(p[i], n[i])
      logit(p[i]) <- mu + u[i]
      u[i] ~ dnorm(0, prec)
    }
    mu  ~ dnorm(0, 1.0E-4)
    tau ~ dunif(0, 5)
    tau2 <- tau * tau
    prec <- 1 / tau2
    pOverallLogitMean <- ilogit(mu)
  }"
    
    lData <- list(
        Nstud = nStud,
        y = as.integer( vY ),
        n = as.integer( vN )
    )
    
    MakeInits <- function() {
        # Initialize around empirical logit with small jitter
        dMuStart <- qlogis( ( sum( vY ) + 0.5 ) / ( sum( vN ) + 1.0 ) )
        lInit <- list(
            mu = rnorm( 1, dMuStart, 0.5 ),
            tau = runif( 1, 0.1, 1.0 ),
            u = rnorm( nStud, 0, 0.2 )
        )
        return( lInit )
    }
    lInits <- replicate( nChains, MakeInits(), simplify = FALSE )
    
    mJags <- jags.model(
        file = textConnection( strModel ),
        data = lData,
        inits = lInits,
        n.chains = nChains,
        n.adapt = nAdapt
    )
    
    update( mJags, n.iter = nBurn )
    
    vParams <- c( "mu", "tau", "pOverallLogitMean", "p" )
    mcmc <- coda.samples(
        model = mJags,
        variable.names = vParams,
        n.iter = nIter,
        thin = nThin
    )
    smry <- summary( mcmc )
    dfQuant <- as.data.frame( smry$quantiles )
    dfStats <- as.data.frame( smry$statistics )
    dfOut <- cbind( Parameter = rownames( dfStats ), dfStats, dfQuant )
    rownames( dfOut ) <- NULL
    
    strCurrentStudy <- paste0( "p[", 1, "]" )
    vCurrentStudy <- as.numeric(
        do.call(
            rbind,
            lapply( mcmc, function( x ) x[ , strCurrentStudy, drop = TRUE ] )
        )
    )
    
    lRet <- list(
        samples = mcmc,
        summary = dfOut,
        vCurrentStudy = vCurrentStudy
    )
    return( lRet )
}


set.seed( 2025 )

# True hyperparameters
dMuTrue <- qlogis( 0.2 )   # overall ~30% on average (logit scale)
dTauTrue <- 0.1             # heterogeneity SD on logit scale

nStud <- 10
vN <- sample( 60:220, nStud, replace = TRUE )
vU <- rnorm( nStud, 0, dTauTrue )
vP <- plogis( dMuTrue + vU )
vY <- rbinom( nStud, size = vN, prob = vP )

vY/vN
dfPrior <- data.frame( Y = vY, N = vN)

#saveRDS(dfPrior, "PriorStudies.Rds")

# Fit
vY[1] <- rbinom( 1, vN[1], 0.25)

vY <- dfPriorStudies$Y
vN <- dfPriorStudies$N
lFit <- RunHierBinomJAGS(
    vY = vY,
    vN = vN,
    nIter = 8000,
    nBurn = 3000,
    nChains = 3,
    nThin = 2,
    nAdapt = 1000
)
vY[1]/vN[1]
mean( lFit$vCurrentStudy)
vStudy1Post <- rbeta( 12000, 0.2 + vY[1], 0.8 + vN[1] - vY[1])
mean( vStudy1Post)
plot( density( lFit$vCurrentStudy ), type = 'l')
lines( density( vStudy1Post), lty = 2)

# View pooled summaries and study-level rates
lFit$summary[ lFit$summary$Parameter %in% c( "mu", "tau", "pOverallLogitMean" ), ]

# Extract posterior means of each study's response rate p[i]
vRow <- grep( "^p\\[", lFit$summary$Parameter )
lFit$summary[ vRow, c( "Parameter", "Mean", "2.5%", "97.5%" ) ]

