library(deSolve)

# Use this script to generate concentration vectors using ODE 

# Define ODE function for one-compartment model with first-order absorption
OneCompartmentModelPK <- function(time, state, parameters) {
    with(as.list(c(state, parameters)), {
        dA1 <- -ka * A1  # Change in drug amount in absorption compartment
        dA2 <- (ka * A1 - ke * A2)  # Change in drug concentration in central compartment
        list(c(dA1, dA2))
    })
}


# Initial state: A1 = Dose (amount in absorption compartment), A2 = 0 (concentration in central compartment)
state <- c( A1 = 100, A2 = 0) 
parameters <- c( ka = 1, ke = 0.2)

VisitTime <- c( 1,2,3,4,5 )
NumVisit <- 5


# Solve ODE for each visit time
j <- 1
concentration <- numeric(NumVisit) #prepare a vector (NumVisit length) to store concentrations at each visit
for (j in 1:NumVisit) {
    time <- c(0, VisitTime[j])  # Time points for ODE solver
    result <- deSolve::ode(y = state, times = time, func = OneCompartmentModelPK, parms = parameters)
    state <- result[nrow(result), -1]  # Update state for next visit
    concentration[j] <- state["A2"]  # Extract concentration at current visit
}
