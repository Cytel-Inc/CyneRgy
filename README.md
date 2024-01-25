# CyneRgy <a href="https://Cytel-Inc.github.io/CyneRgy/"><img src="man/figures/logo.png" align="right" height="120" /></a>

R integration with Cytel products provides a highly efficient approach to achieving custom adaptive clinical trial designs, without requiring you to develop an entire R code base. The CyneRgy R package illustrates the synergy of using R and Cytel products for clinical trial simulation and provide tools to help develop new functions.  


# Introduction 

This repository and corresponding website provide an overview of the R examples provided in this repository. Each example is intended to be used with one of Cytel's products (East or Solara).  Each example is included in a directory that provides an R Studio project file, a Description file that describes the example, RCode folder which contains the example R scripts, and FillInTheBlankRCode - examples with various code deleted so you can practice and fill in the blanks. 

In addition, the CyneRgy package provide helpful functions for you to create your own R functions based on templates and a seamless R experience for viewing examples in R Studio.   

Lastly, the Sandbox directory contains incomplete, in-progress examples. The examples in this directory have not been completed or tested. 

# Examples 
A variety of examples can be found in this package. Please see [Examples](articles/ExampleOutline.html) for a complete list of examples. The CyneRgy package provides many built-in functions that may be used; for a complete list see [References](reference/index.html) .  

# Installation 
Currently this package is not officially released and is not available on CRAN. However, it may be installed directly from 1) GitHub using the [remotes package](https://remotes.r-lib.org/) package with the following code:

```
remotes::install_github( "Cytel-Inc/CyneRgy"" )
```

The following examples are included:

# Package Development

This document is intended to help with development of this package.  In this document you can find packages that are helpful, example code snippets and function calls that were made to create new functions. 

## Style Guide 

Please follow the [Biopharm Soft style guide](https://biopharmsoftgrp.github.io/BioPharmSoftRStyleGuide/) when developing code so that the style is consistent across different developers. 

## Helpful packages
1. [pkgdown](https://pkgdown.r-lib.org/reference/build_home.html) - Package used to create the package website. 
1. [testthat](https://testthat.r-lib.org/) Useful for creating and testing R packages
1. [covr](https://covr.r-lib.org/) - Use to create a test coverage report with the following commands
 ```
usethis::use_coverage()  # Likely you do not need to run this again as it was set up already
covr::package_coverage() # Computes the coverage
covr::report()           # Create a report
```

## Helpful Links
1. [Git for Windows](https://git-for-windows.github.io/)  - Git for Windows; I believe you need to install this to use source control from Windows
3. [Tortoise Git](https://tortoisegit.org/) - Free Windows shell program (runs in Windows explorer by right-clicking on a folder/file) – See image to the right.  This allows you to commit changes to GitHub without having to remember all the commands. 
2. [GitHub Flow](https://guides.github.com/introduction/flow/) - Helps understanding GitHub flow.

4. [Getting Started with Source Control](https://git-scm.com/book/en/v1/Getting-Started-About-Version-Control) - An introduction to source control and getting started with Git.
5. [Git Tutorial](https://backlog.com/git-tutorial/) - Useful Git tutorial 




## Helpful Code Snippets
If you need help please ask Kyle Wathen or another team member

Add code snippet by Clicking Tools-> Global Options -> Code-> Edit Snippets.  In the Snippets menu choose R.  When creating new snippets the key work snippet should start in column 1 and the snippets should be indented.  In order to insert a snippet in R Studio you type the snippet name, or partial name, and click tab. 


Snippet to insert a new comment (newcom) will insert a commented code block.

 ```
snippet newcom
    `r paste( paste( rep( "#", 100), collapse="" ), "." )`
    # ${0} ####
     `r paste( paste( rep( "#", 100), collapse="" ), "." )`
```

Snippet to insert a header (header).

```
snippet header
   `r paste( paste( rep( "#", 100), collapse="" ), "." )`
   #   Program/Function Name: ${1}
   #   Author: J. Kyle Wathen
   #   Description:
   #   Change History:
   #   Last Modified Date:
   `r paste( paste( rep( "#", 100), collapse="" ), "." )`
```

