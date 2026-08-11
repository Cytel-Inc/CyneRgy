[//]: # (Comment: When editing this file, do not forget to edit README.md too.)

# CyneRgy <a href=""><img src="man/figures/logo.png" align="right" height="120" /></a>

# Introduction

R integration with Cytel's products provides a highly efficient way to create custom adaptive clinical trial designs and enhance your simulation capabilities, without requiring you to develop an entire R code base.

The CyneRgy repository provides documentation, templates, and complete examples for Cytel products such as East Horizon. The accompanying R package provides a small set of tools for finding those examples, creating custom scripts from templates, and preparing R code for use in Cytel products.

<div class="alert alert-danger" role="alert"><p style="margin-bottom:0">
Important (January 2026): **ArrivalTime** is a new required parameter for the Response integration point. Existing R scripts must be updated to include this parameter in the function definition, even if it is not used. See <a href="articles/IntegrationPointResponse.html" class="alert-link">**Integration Point: Response**</a> for more information.
</p></div>

# Getting Started

For a quick start, visit the [Getting Started](articles/Overview.html) section. This guide covers the basics of integrating your R scripts with Cytel's products, including detailed steps for accessing integration points, what input variables are available, what output variables are expected, and links to related templates and examples.

# Examples and Templates

A variety of examples showing how R scripts integrate with Cytel's simulation tools are stored in the repository's [`inst/Examples`](https://github.com/Cytel-Inc/CyneRgy/tree/main/inst/Examples) directory. Please see the [Examples Outline](articles/ExampleOutline.html) for the complete list and descriptions.

The R functions used by these examples remain in their example directories; they are not exported as `CyneRgy::` package functions. Each example is self-contained and generally includes:

- A `Description.Rmd` file explaining the example.
- An `R` folder containing its scripts.
- Any inputs or supporting files required by that example.
- A matching `<ExampleName>.Rproj` file as an optional RStudio entry point.

The R scripts do not depend on the RStudio project. It is provided for users who clone or copy an example and prefer to open the
whole folder directly in RStudio. Some examples also include hands-on practice files.

After installing CyneRgy, list and open the included examples with:

```r
CyneRgy::RunExample()
CyneRgy::RunExample( "TreatmentSelection" )
```

For an installed package, the second call creates or reuses a writable copy under `~/CyneRgyExamples`; files inside the R package
library are never opened. When CyneRgy is loaded from a development checkout with `pkgload::load_all()`, it opens the repository
example directly. To choose another copy location, provide an existing destination directory:

```r
CyneRgy::RunExample( "TreatmentSelection", strDirectory = getwd() )
```

The default user location can also be changed with `options( CyneRgy.examples.path = "path" )`.

`RunExample()` opens the matching project in RStudio. In VS Code it opens the example folder, `Description.Rmd`, and every R script
under `R/`. Positron and browser-based environments use their available IDE hooks. In other environments the function displays and
returns the example path so it can be opened manually.

Templates are available in the [Templates directory](https://github.com/Cytel-Inc/CyneRgy/tree/main/inst/Templates), and exploratory, in-progress examples can be found in the [Sandbox directory](https://github.com/Cytel-Inc/CyneRgy/tree/main/Sandbox). Note that Sandbox examples are incomplete and untested.

# Functions

The package intentionally keeps a small public API. Its main functions are:

- `RunExample()` lists, opens, or copies an included example.
- `CreateCyneRgyFunction()` creates one R script from an integration-point template.
- `CreateCyneRgyExample()` creates an example folder with a matching RStudio project by default; the project can be omitted.
- `CombineAllRFiles()` combines a folder of R scripts for upload to a Cytel product.
- `GetDecisionString()` and `GetDecision()` help create valid analysis decisions.

`PlotExampleFlowchart()` supports the diagrams used in example documentation. For details, see the [function reference](reference/index.html).

# Installation

Currently, this package is not officially released and is not available on CRAN. However, it may be installed directly from GitHub using the [remotes package](https://remotes.r-lib.org/) with the following code:

```r
remotes::install_github( "Cytel-Inc/CyneRgy@main" )
```

You must have the `remotes` package to use the command above. The optional `rstudioapi` package provides the best opening experience in RStudio and Positron; it is not required to list, locate, or copy examples.
