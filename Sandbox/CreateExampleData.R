# Example objects for trial with binary data trying to do an analysis request 

nQtyOfPatientsPerArm <- 125
nQtyOfPatients       <- 2*nQtyOfPatientsPerArm

vTreatmentID        <- c( rep(0,nQtyOfPatientsPerArm ), rep( 1, nQtyOfPatientsPerArm) )
vPatientResponseStd <- rbinom( nQtyOfPatientsPerArm,1, 0.25 )
vPatientResponseExp <- rbinom( nQtyOfPatientsPerArm,1, 0.5 )
vPatientResponse    <- c( vPatientResponseStd, vPatientResponseExp )

vRandomIndex        <- sample( 1:nQtyOfPatients,  size = nQtyOfPatients )

vPatientResponse    <- vPatientResponse[ vRandomIndex ]
vTreatmentID        <- vTreatmentID[ vRandomIndex ]

SimData     <- list( TreatmentID = vTreatmentID, Response = vPatientResponse)
LookInfo    <- list( NumLooks = 3, CurrLooKIndex = 1)
DesignParam <- list( SampleSize = nQtyOfPatients, MaxCompleters = nQtyOfPatients)

# Now call the analysis function you created
{{FUNCTION_NAME}}( SimData, DesignParam, LookInfo, UserParam )