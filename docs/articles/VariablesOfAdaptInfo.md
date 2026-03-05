# Variables of AdaptInfo

The variables in AdaptInfo define the sample size re-estimation design
parameters. They can be used in custom scripts for the [Analysis
Integration
Point](https://Cytel-Inc.github.io/CyneRgy/articles/IntegrationPointAnalysis.md)
and the [Treatment Selection Integration
Point](https://Cytel-Inc.github.io/CyneRgy/articles/IntegrationPointTreatmentSelection.md).
To access these parameters in your R code, use the syntax:
`AdaptInfo$NameOfTheVariable`, replacing `NameOfTheVariable` with the
appropriate parameter name.

The tables below describe each variable and notes any conditions under
which it can or cannot be used in a project on East Horizon Explore and
East Horizon Design. Availability notes are only specified when they
differ from the default availability of the integration points (see the
pages linked above for details).

AdaptInfo is only available for
`Statistical Design = Group Sequential with Sample Size Re-Estimation`
(for Design,
`Adaptation Method = Cui, Hung, and Wang (CHW) or Chen, DeMets, and Lan (CDL)`.
Not available for
`Endpoint Type = Continuous with Repeated Measures or Dual Endpoint (TTE-TTE or TTE-Binary)`
or `Study Objective = Multiple Arm COnfirmatory`.

## For `Study Objective = Two Arm Confirmatory`

### For `Endpoint Type = Continuous, Binary, Time-to-Event`

[TABLE]
