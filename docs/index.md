# CyneRgy

# Introduction

R integration with Cytel’s products provides a highly efficient way to
create custom adaptive clinical trial designs and enhance your
simulation capabilities, without requiring you to develop an entire R
code base.

CyneRgy provides documentation, templates, examples, and R functions for
integrating R with Cytel products such as East Horizon.

Important (January 2026): **ArrivalTime** is a new required parameter
for the Response integration point. Existing R scripts must be updated
to include this parameter in the function definition, even if it is not
used. See [**Integration Point:
Response**](https://Cytel-Inc.github.io/CyneRgy/articles/IntegrationPointResponse.md)
for more information.

# Getting Started

For a quick start, visit the [Getting
Started](https://Cytel-Inc.github.io/CyneRgy/articles/Overview.md)
section. This guide covers the basics of integrating your R scripts with
Cytel’s products, including detailed steps for accessing integration
points, what input variables are available, what output variables are
expected, and links to related templates and examples.

# Examples and Templates

A variety of examples highlighting how R scripts integrate with Cytel’s
simulation tools can be found in
[`inst/Examples`](https://github.com/Cytel-Inc/CyneRgy/tree/main/inst/Examples).
Please see the [Examples
Outline](https://Cytel-Inc.github.io/CyneRgy/articles/ExampleOutline.md)
for the complete list and descriptions. Each example includes a
description and R scripts; some also include supporting files, practice
scripts, and an optional RStudio project.

After installing CyneRgy, list or open an example with:

``` r
CyneRgy::RunExample()
CyneRgy::RunExample( "TreatmentSelection" )
```

[`RunExample()`](https://Cytel-Inc.github.io/CyneRgy/reference/RunExample.md)
creates a writable copy when needed, then opens the description and R
scripts in the active supported IDE. Use `strDirectory` to choose the
copy location.

Templates are available in the [Templates
directory](https://github.com/Cytel-Inc/CyneRgy/tree/main/inst/Templates),
and exploratory, in-progress examples can be found in the [Sandbox
directory](https://github.com/Cytel-Inc/CyneRgy/tree/main/Sandbox). Note
that Sandbox examples are incomplete and untested.

# Functions

The package exports selected common functions for trial operations and
binary, continuous, repeated-measures, time-to-event, DEP, and MEP
endpoints. It also provides
[`RunExample()`](https://Cytel-Inc.github.io/CyneRgy/reference/RunExample.md),
[`CreateCyneRgyFunction()`](https://Cytel-Inc.github.io/CyneRgy/reference/CreateCyneRgyFunction.md),
[`CreateCyneRgyExample()`](https://Cytel-Inc.github.io/CyneRgy/reference/CreateCyneRgyExample.md),
and
[`CombineAllRFiles()`](https://Cytel-Inc.github.io/CyneRgy/reference/CombineAllRFiles.md)
for working with examples and integration scripts. See the [function
reference](https://Cytel-Inc.github.io/CyneRgy/reference/index.md) for
details.

# Installation

Currently, this package is not officially released and is not available
on CRAN. However, it may be installed directly from GitHub using the
[remotes package](https://remotes.r-lib.org/) with the following code:

``` r
remotes::install_github( "Cytel-Inc/CyneRgy@main" )
```

You must have the `remotes` package to use the command above. The
optional `rstudioapi` package provides the best opening experience in
RStudio and Positron; it is not required to list, locate, or copy
examples.
