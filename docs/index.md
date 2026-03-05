# CyneRgy

# Introduction

R integration with Cytel’s products provides a highly efficient way to
create custom adaptive clinical trial designs and enhance your
simulation capabilities, without requiring you to develop an entire R
code base.

The CyneRgy R package demonstrates the synergy between R and Cytel’s
products such as East Horizon, providing tools, documentation,
templates, and examples. It also offers a streamlined R experience in
RStudio, simplifying the creation of new custom scripts.

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

A variety of examples highlighting how R scripts can seamlessly
integrate with Cytel’s simulation tools can be found in this package.
Please see [Examples
Outline](https://Cytel-Inc.github.io/CyneRgy/articles/ExampleOutline.md)
for a complete list of examples and their descriptions.

[Each example
directory](https://github.com/Cytel-Inc/CyneRgy/tree/main/inst/Examples)
provides:

- An **RStudio project file** for setup.  
- A **Description file** detailing the example.  
- An **R folder** which contains the example R scripts.  
- A **FillInTheBlankR folder** which contains practice scripts with
  sections removed for hands-on learning.

Templates are available in the [Templates
directory](https://github.com/Cytel-Inc/CyneRgy/tree/main/inst/Templates),
and exploratory, in-progress examples can be found in the [Sandbox
directory](https://github.com/Cytel-Inc/CyneRgy/tree/main/Sandbox). Note
that Sandbox examples are incomplete and untested.

# Functions

The CyneRgy package also provides many built-in functions to facilitate
the creation of your custom R scripts. For a complete list of available
functions, see
[References](https://Cytel-Inc.github.io/CyneRgy/reference/index.md).

# Installation

Currently, this package is not officially released and is not available
on CRAN. However, it may be installed directly from GitHub using the
[remotes package](https://remotes.r-lib.org/) with the following code:

    remotes::install_github( "Cytel-Inc/CyneRgy@main" )

You must have the remotes package to use the above command. To launch
the examples, you will also need to have the rstudioapi package.
