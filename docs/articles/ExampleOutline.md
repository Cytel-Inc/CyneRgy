# Examples Outline

## Introduction

This document provides an overview of the R examples provided in [this
directory](https://github.com/Cytel-Inc/CyneRgy/tree/main/inst/Examples).
Each example is included in a directory that provides:

- an RStudio project file,
- a Description file that describes the example,
- an R folder which contains the example R scripts,
- optional: a FillInTheBlankR folder which contains the worked examples
  with various code deleted so you can practice and fill in the blanks.

The following examples are included:

## Enrollment

**Arrival Times with Poisson Process** Any \# of ArmsSingle/Dual/Multi
EndpointsAny Outcome

This example demonstrates how to add the ability to generate patient
arrival times according to a Poisson process with a ramp-up by
customizing the Enrollment integration point of East Horizon.  
[Click here to view the full
example.](https://Cytel-Inc.github.io/CyneRgy/articles/GeneratePoissonArrival.md)

------------------------------------------------------------------------

## Randomization

**Randomization of Subjects** 2-ArmMultiple ArmSingle/Dual/Multi
EndpointsAny Outcome

This example illustrates four ways to customize how subjects are
assigned to treatment arms in East Horizon: using a uniform
distribution, using the [`sample()`](https://rdrr.io/r/base/sample.html)
function, using the `randomizeR` package, and in the context of a
multi-arm trial. [Click here to view the full
example.](https://Cytel-Inc.github.io/CyneRgy/articles/RandomizeSubjects.md)

------------------------------------------------------------------------

## Dropout

**2-Arm, Patient Dropout** 2-ArmSingle
EndpointContinuousTTEBinaryRepeated Measures

This example illustrates how to customize the dropout distribution in
East Horizon for continuous and binary outcomes, time-to-event outcome,
and continuous outcome with repeated measures. [Click here to view the
full
example.](https://Cytel-Inc.github.io/CyneRgy/articles/2ArmPatientDropout.md)

**Multiple Arm, Patient Dropout** Multiple ArmSingle
EndpointContinuousBinary

This example illustrates how to customize the dropout distribution in
East Horizon for multi-arm trials, covering continuous, binary, and
time-to-event outcomes. [Click here to view the full
example.](https://Cytel-Inc.github.io/CyneRgy/articles/MultiArmPatientDropout.md)

------------------------------------------------------------------------

## Response (Patient Simulation)

**Continuous Outcome – Patient Simulation** 2-ArmSingle
EndpointContinuous

This example demonstrates two ways to customize the patient outcome
simulation in East Horizon for a two-arm trial with a continuous
outcome: using a mixture distribution, with or without the mixture
proportion sampled from a Beta distribution.  
[Click here to view the full
example.](https://Cytel-Inc.github.io/CyneRgy/articles/2ArmNormalOutcomePatientSimulation.md)

**Time-To-Event Outcome – Patient Simulation** 2-ArmSingle
EndpointTTEStratification

This example demonstrates two ways to customize the patient outcome
simulation in East Horizon for a two-arm trial with a time-to-event
outcome: using a Weibull distribution, and using a mixture of
exponential distributions. It also presents additional examples using
Stratification. [Click here to view the full
example.](https://Cytel-Inc.github.io/CyneRgy/articles/2ArmTimeToEventOutcomePatientSimulation.md)

**Binary Outcome – Patient Simulation**2-ArmSingle EndpointBinary

This example demonstrates two ways to customize the patient outcome
simulation in East Horizon for a two-arm trial with a binary outcome:
using a mixture distribution, with or without the mixture proportion
sampled from a Beta distribution. [Click here to view the full
example.](https://Cytel-Inc.github.io/CyneRgy/articles/2ArmBinaryOutcomePatientSimulation.md)

**Repeated Measures – Patient Simulation**2-ArmSingle EndpointRepeated
Measures

This example demonstrates how to customize the patient outcome
simulation in East Horizon for a two-arm trial with a continuous outcome
with repeated measures using the
[`MASS::mvrnorm()`](https://rdrr.io/pkg/MASS/man/mvrnorm.html) function.
[Click here to view the full
example.](https://Cytel-Inc.github.io/CyneRgy/articles/2ArmNormalRepeatedMeasuresResponseGeneration.md)

**Multiple Arm – Patient Simulation**Multiple ArmSingle
EndpointContinuousBinary

This example demonstrates how to customize the patient outcome
simulation in East Horizon for multi-arm trials, covering continuous,
binary, and time-to-event outcomes. [Click here to view the full
example.](https://Cytel-Inc.github.io/CyneRgy/articles/MultiArmPatientSimulation.md)

**Dual Endpoints – Patient Simulation**2-ArmDual EndpointsTTE-TTE

This example demonstrates how to customize the patient outcome
simulation in East Horizon for two-arm trials with dual endpoints
(TTE-TTE). [Click here to view the full
example.](https://Cytel-Inc.github.io/CyneRgy/articles/DEPPatientSimulation.md)

**Multiple Endpoints – Patient Simulation**2-ArmMultiple
EndpointsContinuousBinaryTTE

This example demonstrates how to customize the patient outcome
simulation in East Horizon for two-arm trials with multiple endpoints
(Continuous, Binary and/or TTE). [Click here to view the full
example.](https://Cytel-Inc.github.io/CyneRgy/articles/MEPPatientSimulation.md)

**Childhood Anxiety Trial** 2-ArmSingle EndpointContinuous

This example covers a specific situation where the patient data
simulation needs to be customized to match what is expected in a
clinical trial in childhood anxiety.  
[Click here to view the full
example.](https://Cytel-Inc.github.io/CyneRgy/articles/ChildhoodAnxiety.md)

------------------------------------------------------------------------

## Analysis (Test Statistic)

**Continuous Outcome – Analysis** 2-ArmSingle EndpointContinuousSSR

This example demonstrates three ways to customize the statistical test
in East Horizon for a two-arm trial with a continuous outcome: using a
formula from the East manual, using the
[`t.test()`](https://rdrr.io/r/stats/t.test.html) function, and using
confidence interval limits for Go/No-Go decision-making. It also
presents an additional example using Sample Size Re-estimation. [Click
here to view the full
example.](https://Cytel-Inc.github.io/CyneRgy/articles/2ArmNormalOutcomeAnalysis.md)

**Time-To-Event Outcome – Analysis** 2-ArmSingle
EndpointTTESSRStratification & Subpopulation

This example demonstrates three ways to customize the statistical test
in East Horizon for a two-arm trial with a time-to-event outcome: using
formulas from the East manual, using the
[`survival::survdiff()`](https://rdrr.io/pkg/survival/man/survdiff.html)
function, and using confidence interval limits for Go/No-Go
decision-making. It also presents additional examples using Sample Size
Re-estimation, Stratification, and Subpopulation options. [Click here to
view the full
example.](https://Cytel-Inc.github.io/CyneRgy/articles/2ArmTimeToEventOutcomeAnalysis.md)

**Binary Outcome – Analysis** 2-ArmSingle EndpointBinarySSR

This example demonstrates four ways to customize the statistical test in
East Horizon for a two-arm trial with a binary outcome: using a formula
from the East manual, using the
[`prop.test()`](https://rdrr.io/r/stats/prop.test.html) function, using
confidence interval limits for Go/No-Go decision-making, and using a
Bayesian Beta-Binomial model. It also presents an additional example
using Sample Size Re-estimation. [Click here to view the full
example.](https://Cytel-Inc.github.io/CyneRgy/articles/2ArmBinaryOutcomeAnalysis.md)

**Repeated Measures – Analysis**2-ArmSingle EndpointRepeated Measures

This example demonstrates how to customize the statistical test in East
Horizon for a two-arm trial with a continuous outcome with repeated
measures using the
[`nlme::gls()`](https://rdrr.io/pkg/nlme/man/gls.html) function. [Click
here to view the full
example.](https://Cytel-Inc.github.io/CyneRgy/articles/2ArmNormalRepeatedMeasuresAnalysis.md)

**Multiple Arm – Analysis**Multiple ArmSingle EndpointContinuousBinary

This example demonstrates how to customize the statistical test in East
Horizon for multi-arm trials, covering continuous, binary, and
time-to-event outcomes. [Click here to view the full
example.](https://Cytel-Inc.github.io/CyneRgy/articles/MultiArmAnalysis.md)

**Dual Endpoints – Analysis**2-ArmDual EndpointsTTE-TTETTE-Binary

This example demonstrates how to customize the statistical test in East
Horizon for two-arm trials with dual endpoints. [Click here to view the
full
example.](https://Cytel-Inc.github.io/CyneRgy/articles/DEPAnalysis.md)

**Weighted Conditional Power Analysis** 2-ArmSingle EndpointTTE

This example demonstrates how to customize the statistical test in East
Horizon for a two-arm trial with a time-to-event outcome using
conditional power and futility boundaries based on the Logrank test.
[Click here to view the full
example.](https://Cytel-Inc.github.io/CyneRgy/articles/TimeToEventConditionalPowerFutilityAnalysis.md)

------------------------------------------------------------------------

## Treatment Selection

**Binary Outcome - Treatment Selection** Multiple ArmSingle
EndpointBinary

This example demonstrates four ways to customize the treatment selection
in East Horizon for a multiple arm trial: based on response rates, based
on p-value, based on number of responses, and based on Bayesian
posterior probabilities. [Click here to view the full
example.](https://Cytel-Inc.github.io/CyneRgy/articles/TreatmentSelection.md)

------------------------------------------------------------------------

## Multiplicity Adjustment (Dual Endpoints)

**Dual Endpoints - Multiplicity Adjustment**2-ArmDual
EndpointsTTE-TTETTE-Binary

This example demonstrates how to compute decisions for a dual-endpoint
fixed sample clinical trial using the Bonferroni adjustment for multiple
testing. [Click here to view the full
example.](https://Cytel-Inc.github.io/CyneRgy/articles/DEPDecisionsUsingMCP.md)

------------------------------------------------------------------------

## Design (Multiple Endpoints)

**Multiple Endpoints - Design**2-ArmMultiple
EndpointsContinuousBinaryTTE

This example demonstrates how to implement custom decision-making logic
for multiple endpoints designs. [Click here to view the full
example.](https://Cytel-Inc.github.io/CyneRgy/articles/MEPDesign.md)

------------------------------------------------------------------------

## Advanced Examples

These examples use more than one integration point in the same project
to achieve a more complex design option.

**Bayesian Assurance, Continuous** 2-ArmSingle EndpointContinuous

This example demonstrates the computation of Bayesian assurance, or
probability of success, using a mixture of normal distribution priors
featuring a two-arm trial with continuous outcome.  
[Click here to view the full
example.](https://Cytel-Inc.github.io/CyneRgy/articles/BayesianAssuranceContinuous.md)

**Bayesian Assurance, Time-to-Event** 2-ArmSingle EndpointTTE

This example demonstrates the computation of Bayesian assurance, or
probability of success, using a bi-modal distribution prior featuring a
two-arm trial with time-to-event outcome.  
[Click here to view the full
example.](https://Cytel-Inc.github.io/CyneRgy/articles/BayesianAssuranceTimeToEvent.md)

**Consecutive Studies, Binary** 2-ArmSingle EndpointBinaryFMS

This example demonstrates the computation of conditional probability of
success in consecutive studies. It features a sequential design
involving a Phase 2 trial followed by a Phase 3 trial, both with a
binary outcome Phase 2 results are saved and then used as the prior for
Phase 3, allowing Phase 3 patient outcomes to be generated conditional
on Phase 2 success.  
[Click here to view the full
example.](https://Cytel-Inc.github.io/CyneRgy/articles/ConsecutiveStudiesBinary.md)

**Consecutive Studies, Continuous** 2-ArmSingle EndpointContinuous

This example demonstrates the computation of Bayesian assurance, or
probability of success, in consecutive studies: a Phase 2 trial followed
by a Phase 3 trial, both with continuous outcome. The objective is to
understand how conducting a Phase 2 study can reduce the risk associated
with the Phase 3 trial.  
[Click here to view the full
example.](https://Cytel-Inc.github.io/CyneRgy/articles/ConsecutiveStudiesContinuous.md)

**Consecutive Studies, Continuous & TTE** 2-ArmSingle
EndpointContinuousTTEFMS

This example demonstrates the computation of Bayesian assurance, or
probability of success, in consecutive studies: a Phase 2 trial with
continuous outcome followed by a Phase 3 trial with time-to-event
outcome. The objective is to understand how conducting a Phase 2 study
can reduce the risk associated with the Phase 3 trial. This example also
includes a step to load the Phase 2 output and extract the true
treatment differences.  
[Click here to view the full
example.](https://Cytel-Inc.github.io/CyneRgy/articles/ConsecutiveStudiesContinuousTimeToEvent.md)

**Probability of Success, PFS & OS** 2-ArmSingle EndpointTTE

This example demonstrates how to compute the probability of success of a
trial and extend East Horizon’s single-endpoint framework to handle dual
endpoints (Progression-Free Survival and Overall Survival) using custom
R scripts for the Analysis and Response integration points.  
[Click here to view the full
example.](https://Cytel-Inc.github.io/CyneRgy/articles/ProbabilitySuccessDualEndpoints.md)

**Multiple Endpoints w/ Covariates & Stratified Randomization**
2-ArmSingle EndpointContinuous

This example demonstrates how to integrate patient outcome simulation
and analysis functionality for multiple independent continuous outcomes
into East Horizon, across three cases: multiple endpoints using a
t-test, multiple endpoints with covariates using ANCOVA, and multiple
endpoints with covariates and stratified randomization. [Click here to
view the full
example.](https://Cytel-Inc.github.io/CyneRgy/articles/MultipleEndpointsWithCovariates.md)

**Schizophrenia Trial (MMRM Analysis)** 2-ArmSingle EndpointRepeated
Measures

This example demonstrates how to simulate and analyze data using a Mixed
Model for Repeated Measures (MMRM) approach in the context of a
schizophrenia trial. [Click here to view the full
example.](https://Cytel-Inc.github.io/CyneRgy/articles/SchizophreniaTrial.md)
