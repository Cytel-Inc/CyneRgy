# Compute Posterior Probability of Experimental Treatment Being Greater than Control

Function to perform statistical analysis using a Beta-Binomial Bayesian
model. It computes the posterior probability that the success rate of an
experimental treatment exceeds that of a control treatment, based on
observed outcomes.

## Usage

``` r
ProbExpGreaterCtrlBeta(
  vOutcomesS,
  vOutcomesE,
  dAlphaS,
  dBetaS,
  dAlphaE,
  dBetaE
)
```

## Arguments

- vOutcomesS:

  A vector of binary outcomes (0 or 1) for the control treatment.

- vOutcomesE:

  A vector of binary outcomes (0 or 1) for the experimental treatment.

- dAlphaS:

  The alpha parameter of the Beta prior for the control treatment.

- dBetaS:

  The beta parameter of the Beta prior for the control treatment.

- dAlphaE:

  The alpha parameter of the Beta prior for the experimental treatment.

- dBetaE:

  The beta parameter of the Beta prior for the experimental treatment.

## Value

A list containing:

- dPostProb:

  The posterior probability that the success rate of the experimental
  treatment is greater than that of the control treatment.

## Details

In the Beta-Binomial model, it is assumed that the probability of
success (\\\pi\\) follows a Beta distribution: \\\pi \sim Beta(\alpha,
\beta)\\. Given observed binary outcomes, the posterior distribution of
\\\pi\\ is: \\\pi \| \text{data} \sim \text{Beta}(\alpha + \text{\\
successes}, \beta + \text{\\ non-successes})\\. This function samples
from the posterior distributions of the success probabilities for both
control and experimental treatments, and calculates the posterior
probability that the experimental treatment has a higher success rate
than the control treatment.
