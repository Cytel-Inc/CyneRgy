# Generate a Flowchart for a CyneRgy Example

This function creates a flowchart of the used integration points and
main steps of a CyneRgy R Integration example. Used integration points
are highlighted with wider columns and custom step boxes, while unused
points remain gray placeholders. Steps are automatically wrapped to fit.

## Usage

``` r
PlotExampleFlowchart(
  lIntPoints = list(),
  nBoxHeight = 0.7,
  nBoxSpacing = 0.3,
  nColumnWidth = 0.5,
  nBigColWidth = 3,
  bShowTreatmentSelection = FALSE,
  bShowMultiplicityAdjustment = FALSE
)
```

## Arguments

- lIntPoints:

  A named list where each name is an integration point (e.g.,
  "Response") and each element is a character vector of step labels for
  that integration point. Options: "Initialization", "Enrollment",
  "Randomization", "Dropout", "Treatment Selection", "Response",
  "Analysis", "Multiplicity Adjustment"

- nBoxHeight:

  Numeric. Base height of each step box. Default = 0.7.

- nBoxSpacing:

  Numeric. Vertical spacing between boxes. Default = 0.3.

- nColumnWidth:

  Numeric. Width of unused integration point columns. Default = 0.5.

- nBigColWidth:

  Numeric. Width of used integration point columns. Default = 3.

- bShowTreatmentSelection:

  Logical. Whether to include the "Treatment Selection" column. Default
  = FALSE.

- bShowMultiplicityAdjustment:

  Logical. Whether to include the "Multiplicity Adjustment" column.
  Default = FALSE.

## Value

A ggplot object containing the flowchart visualization.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example with one used integration point
p1 <- PlotExampleFlowchart(
  lIntPoints = list(
    "Response" = c(
      "Load MAV, TV, and confidence level",
      "Run proportions test (treatment > control)",
      "Calculate CI lower and upper limits",
      "Return decision using CI thresholds"
    )
  )
)
p1

# Example with two integration points
p2 <- PlotExampleFlowchart(
  lIntPoints = list(
    "Response" = c(
      "Load MAV, TV, and confidence level",
      "Run proportions test (treatment > control)"
    ),
    "Analysis" = c(
      "Analyze PFS using Cox regression",
      "Analyze OS using Cox regression"
    )
  )
)
p2
} # }
```
