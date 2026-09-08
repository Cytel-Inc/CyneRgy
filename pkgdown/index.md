[//]: # (Comment: When editing this file, do not forget to edit README.md too.)

# CyneRgy <a href=""><img src="man/figures/logo.png" align="right" height="120" /></a>

# Introduction

R integration with Cytel's products provides a highly efficient way to create custom adaptive clinical trial designs and enhance your simulation capabilities, without requiring you to develop an entire R code base.

CyneRgy provides documentation, templates, examples, and R functions for integrating R with Cytel products such as East Horizon.

<div class="alert alert-danger" role="alert"><p style="margin-bottom:0">
Important (January 2026): **ArrivalTime** is a new required parameter for the Response integration point. Existing R scripts must be updated to include this parameter in the function definition, even if it is not used. See <a href="articles/IntegrationPointResponse.html" class="alert-link">**Integration Point: Response**</a> for more information.
</p></div>

# Getting Started

For a quick start, visit the [Getting Started](articles/Overview.html) section. This guide covers the basics of integrating your R scripts with Cytel's products, including detailed steps for accessing integration points, what input variables are available, what output variables are expected, and links to related templates and examples.

# Examples and Templates

A variety of examples highlighting how R scripts integrate with Cytel's simulation tools can be found in [`inst/Examples`](https://github.com/Cytel-Inc/CyneRgy/tree/main/inst/Examples). Please see the [Examples Outline](articles/ExampleOutline.html) for the complete list and descriptions. Each example includes a description and R scripts; some also include supporting files, practice scripts, and an optional RStudio project.

After installing CyneRgy, you can list or open examples. For example:

```r
CyneRgy::RunExample()                                # Lists all available examples
CyneRgy::RunExample("TreatmentSelection")            # Opens the example project/folder in your IDE
CyneRgy::RunExample("GeneratePoissonArrival")
CyneRgy::RunExample("2ArmPatientDropout")
CyneRgy::RunExample("TreatmentSelection", strDirectory = getwd())   # Copy to a specific folder
```

`RunExample()` creates a writable copy when needed, then opens the description and R scripts in the active supported IDE. Use `strDirectory` to choose the copy location.

Templates are available in the [Templates directory](https://github.com/Cytel-Inc/CyneRgy/tree/main/inst/Templates), and exploratory, in-progress examples can be found in the [Sandbox directory](https://github.com/Cytel-Inc/CyneRgy/tree/main/Sandbox). Note that Sandbox examples are incomplete and untested.

# Functions

The package exports selected common functions for trial operations and binary, continuous, repeated-measures, time-to-event, DEP, and MEP endpoints. It also provides `RunExample()`, `CreateCyneRgyFunction()`, `CreateCyneRgyExample()`, and `CombineAllRFiles()` for working with examples and integration scripts. See the [function reference](reference/index.html) for details.

Common functions can be called directly with `CyneRgy::FunctionName()`. A few examples:

```r
# Arrival
CyneRgy::GeneratePoissonArrival( NumSub = 20, NumPrd = 1, PrdStart = 0, AccrRate = 10 )

# Randomization
CyneRgy::RandomizationSubjectsUsingUniformDistribution( NumSub = 20, NumArms = 2, AllocRatio = 1 )

# Dropout
CyneRgy::GenerateCensoringUsingBinomialProportion( NumSub = 20, ProbDrop = 0.1 )

# Binary endpoint analysis
CyneRgy::AnalyzeUsingPropTest(
    SimData     = data.frame( Response = c( 1, 0, 1, 1, 0, 0, 1, 0 ), TreatmentID = c( 0, 0, 0, 0, 1, 1, 1, 1 ) ),
    DesignParam = list( TailType = 1, CriticalPoint = 1.96 )
)
```

# Installation

Currently, this package is not officially released and is not available on CRAN. However, it may be installed directly from GitHub using the [remotes package](https://remotes.r-lib.org/) with the following code:

```r
remotes::install_github( "Cytel-Inc/CyneRgy@main" )
```

You must have the `remotes` package to use the command above. The optional `rstudioapi` package provides the best opening experience in RStudio and Positron; it is not required to list, locate, or copy examples.
