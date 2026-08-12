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
3.  Start a new example with
    [`CyneRgy::CreateCyneRgyExample()`](https://Cytel-Inc.github.io/CyneRgy/reference/CreateCyneRgyExample.md).
4.  Place the completed example in `inst/Examples`; keep its
    example-specific functions there rather than in the package-level
    `R` directory.
5.  Use Git to add the files to the repository.  
6.  Update the vignettes/ExampleOutline.Rmd file to include a brief
    description and link to the new example.
7.  Use Git to commit to the branch created above.
8.  Open a pull request against the integration branch used by the
    repository.

## Package Development

This document is intended to help with development of this package.

After changing package code or documentation, run
[`devtools::document()`](https://devtools.r-lib.org/reference/document.html),
[`devtools::test()`](https://devtools.r-lib.org/reference/test.html),
[`devtools::check()`](https://devtools.r-lib.org/reference/check.html),
and
[`pkgdown::build_site()`](https://pkgdown.r-lib.org/reference/build_site.html).
Include the relevant generated files under `man/` and `docs/` in the
pull request.

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

Add a code snippet by selecting Tools -\> Global Options -\> Code -\>
Edit Snippets. In the Snippets menu, choose R. When creating new
snippets, the keyword “snippet” should start in column 1 and the
snippets should be indented. To insert a snippet in RStudio, type the
snippet name, or partial name, and press Tab.

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
