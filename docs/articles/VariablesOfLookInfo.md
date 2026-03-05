# Variables of LookInfo

The variables in LookInfo define the multiple looks design parameters.
They can be used in custom scripts for the [Analysis Integration
Point](https://Cytel-Inc.github.io/CyneRgy/articles/IntegrationPointAnalysis.md)
and the [Treatment Selection Integration
Point](https://Cytel-Inc.github.io/CyneRgy/articles/IntegrationPointTreatmentSelection.md).
To access these parameters in your R code, use the syntax:
`LookInfo$NameOfTheVariable`, replacing `NameOfTheVariable` with the
appropriate parameter name.

The tables below describe each variable and notes any conditions under
which it can or cannot be used in a project on East Horizon Explore and
East Horizon Design. Availability notes are only specified when they
differ from the default availability of the integration points (see the
pages linked above for details).

LookInfo is only available for `Statistical Design = Group Sequential`.

## For `Study Objective = Two Arm Confirmatory`

### For `Endpoint Type = Continuous, Binary, Time-to-Event, Continuous with Repeated Measures`

[TABLE]

### For `Endpoint Type = Dual (TTE-TTE or TTE-Binary)` (including for Multiplicity Adjustment)

Named List of length equal to the number of endpoints, indicating the
trial type for each endpoint. For example, `TrialType[“Endpoint 1”]` is
the type for Endpoint 1.

[TABLE]

**Notes:**

- Dual endpoints are not available for East Horizon: Design.
- “Endpoint 1” is used as a sample endpoint name. It will be the actual
  endpoint name as specified by the user.

## For `Study objective = Two Arm Confirmatory - Multiple Endpoints`

[TABLE]

## For `Study Objective = Multiple Arm Confirmatory or Dose Finding`

[TABLE]
