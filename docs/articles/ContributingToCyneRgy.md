# Contributing to CyneRgy

## Contributing to CyneRgy

If you would like to contribute to CyneRgy there are multiple avenues to
get involved.

You can submit an issue, report a bug, or add your own functionality and
examples for others to learn from. To add examples, please follow the
instructions below.

## Style Guide

Please follow the [Biopharm Soft style
guide](https://biopharmsoftgrp.github.io/BioPharmSoftRStyleGuide/) when
developing code so that the style is consistent across different
developers.

## Adding an Example

To make sure the examples are clear and included in the package website
please use the following steps. These steps are changing over time so if
you find something that is not clear or incorrect, please add an issue.

1.  Use Git to clone the CyneRgy repository.
2.  If an issue does not exist in the CyneRgy repository, then please
    add one and use it to create a new branch for development. If an
    issue already exists, then create a branch for development.
3.  You can start a new example with the function
    CyneRgy::CreateCyneRgyExample.
4.  Once the example is complete, copy the example folder to the
    inst/Examples directory of CyneRgy.  
5.  Use Git to add the files to the repository.  
6.  Update the vignettes/ExampleOutline.Rmd file to include a brief
    description and link to the new example.
7.  Use Git to commit to the branch created above.
8.  Add a Pull Request in GitHub to merge your development branch to the
    Dev branch.

Please note that all branches are merged into Dev. Dev is used to
combine all branches, then a branch is created for building the package
website and reviewed for documentation and completeness. Once this is
done, the Dev branch is merged into Main via a Pull Request and the
example is then included on the website as well as in the package when
it is installed from GitHub.

## Package Development

This document is intended to help with development of this package. In
this document you can find packages that are helpful, example code
snippets and function calls that were made to create new functions.

### Helpful packages

1.  [pkgdown](https://pkgdown.r-lib.org/reference/build_home.html) -
    Package used to create the package website.

&nbsp;

    usethis::use_pkgdown_github_pages()  # Likely you do not need to run this again as it was set up already.
    devtools::document() # Build the Rd scripts in /man for the reference section using the R scripts in /R.
    devtools::install() # Re-install CyneRgy after adding new functions to /R.
    pkgdown::build_site() # Build the site pages using Rmd scripts in /vignettes. Preview the site locally before publishing.

2.  [testthat](https://testthat.r-lib.org/) - Useful for creating and
    testing R packages.
3.  [covr](https://covr.r-lib.org/) - Use to create a test coverage
    report with the following commands

&nbsp;

    usethis::use_coverage()  # Likely you do not need to run this again as it was set up already.
    covr::package_coverage() # Computes the coverage
    covr::report()           # Creates a report

### Helpful Links

1.  [Git for Windows](https://git-for-windows.github.io/) - Git for
    Windows; I believe you need to install this to use source control
    from Windows.
2.  [Tortoise Git](https://tortoisegit.org/) - Free Windows shell
    program (runs in Windows explorer by right-clicking on a
    folder/file). This allows you to commit changes to GitHub without
    having to remember all the commands.
3.  [GitHub Flow](https://guides.github.com/introduction/flow/) - Helps
    understanding GitHub flow.
4.  [Getting Started with Source
    Control](https://git-scm.com/book/en/v1/Getting-Started-About-Version-Control) -
    An introduction to source control and getting started with Git.
5.  [Git Tutorial](https://backlog.com/git-tutorial/) - Useful Git
    tutorial.

### Helpful Code Snippets

If you need help please ask Kyle Wathen or another team member.

Add code snippet by Clicking Tools → Global Options → Code → Edit
Snippets. In the Snippets menu choose R. When creating new snippets the
keyword “snippet” should start in column 1 and the snippets should be
indented. To insert a snippet in RStudio, type the snippet name, or
partial name, and click tab.

Snippet to insert a new comment (newcom) will insert a commented code
block.

    snippet newcom
       #################################################################################################### .
       # ${0} ####
       #################################################################################################### .

Snippet to insert a header (header).

    snippet header
       #################################################################################################### .
       #   Program/Function Name: ${1}
       #   Author: J. Kyle Wathen
       #   Description:
       #   Change History:
       #   Last Modified Date:
       #################################################################################################### .
