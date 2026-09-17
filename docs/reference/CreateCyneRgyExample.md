# Create a CyneRgy Example Folder

Creates an example folder containing `Description.Rmd`, an `R` directory
with a selected integration-point template, and a matching RStudio
project by default. The R scripts do not depend on the project file.

## Usage

``` r
CreateCyneRgyExample(
  strFunctionType = "",
  strNewExampleName = "",
  strDirectory = NA,
  bCreateProject = TRUE,
  bOpen = interactive()
)
```

## Arguments

- strFunctionType:

  Character string naming the integration-point template to use.

- strNewExampleName:

  Character string naming the new example folder and starter function.

- strDirectory:

  Existing parent directory where the example should be created.
  Defaults to the current working directory.

- bCreateProject:

  Logical value indicating whether to include an RStudio project file.
  Defaults to `TRUE`.

- bOpen:

  Logical value indicating whether to open the new example in the active
  IDE. Defaults to
  [`interactive()`](https://rdrr.io/r/base/interactive.html).

## Value

Invisibly returns the created example path. When called without
`strFunctionType`, invisibly returns the available template names.

## Examples

``` r
if (FALSE) { # interactive()
CreateCyneRgyExample()
CreateCyneRgyExample( "Analyze.Binary", "MyBinaryAnalysis" )
CreateCyneRgyExample( "Analyze.Binary", "MyBinaryFolder", bCreateProject = FALSE )
}
```
