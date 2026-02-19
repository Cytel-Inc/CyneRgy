# Compute Bayesian Predictive Probability of Success

Function to compute the Bayesian predictive probability of success for a
clinical trial using Bayesian analysis. The function simulates future
patient outcomes based on posterior distributions derived from observed
interim data and evaluates the probability of trial success at the end.

## Usage

``` r
ComputeBayesianPredictiveProbabilityWithBayesianAnalysis(
  dataS,
  dataE,
  priorAlphaS,
  priorBetaS,
  priorAlphaE,
  priorBetaE,
  nQtyOfPatsS,
  nQtyOfPatsE,
  nSimulations,
  finalBoundary,
  lAnalysisParams
)
```

## Arguments

- dataS:

  A vector of binary outcomes (0 or 1) for the control treatment
  observed at the interim analysis.

- dataE:

  A vector of binary outcomes (0 or 1) for the experimental treatment
  observed at the interim analysis.

- priorAlphaS:

  The alpha parameter of the Beta prior for the control treatment.

- priorBetaS:

  The beta parameter of the Beta prior for the control treatment.

- priorAlphaE:

  The alpha parameter of the Beta prior for the experimental treatment.

- priorBetaE:

  The beta parameter of the Beta prior for the experimental treatment.

- nQtyOfPatsS:

  The total number of patients for the control treatment expected by the
  end of the trial.

- nQtyOfPatsE:

  The total number of patients for the experimental treatment expected
  by the end of the trial.

- nSimulations:

  The number of virtual trials to simulate for predictive probability
  computation.

- finalBoundary:

  The cutoff threshold for posterior probability to determine trial
  success.

- lAnalysisParams:

  A list of analysis parameters for posterior computation, including
  priors for the control and experimental treatments.

## Value

A list containing:

- predictiveProbabilityS:

  The Bayesian predictive probability of trial success.

## Details

This function computes the Bayesian predictive probability of success
for a clinical trial. It uses observed interim data to update the Beta
priors into posterior distributions for success probabilities of both
control and experimental treatments. Future patient outcomes are
simulated based on these posteriors, and the trial success is evaluated
based on the probability that the experimental treatment has a higher
success rate than the control treatment. The predictive probability is
calculated as the proportion of simulated trials meeting the success
criteria.
