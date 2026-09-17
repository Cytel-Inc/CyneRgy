# Create a CyneRgy Function from a Template

Creates an R script from one of the integration-point templates included
with CyneRgy. Call the function without a template name to list the
available templates.

## Usage

``` r
CreateCyneRgyFunction(
  strFunctionType = "",
  strNewFunctionName = NA,
  strDirectory = NA,
  bOpen = interactive()
)
```

## Arguments

- strFunctionType:

  Character string naming the template to use.

- strNewFunctionName:

  Character string used for the function and file name. Defaults to
  `strFunctionType`.

- strDirectory:

  Directory where the file should be created. Defaults to the current
  working directory.

- bOpen:

  Logical value indicating whether to open the new file in the active
  IDE. Defaults to
  [`interactive()`](https://rdrr.io/r/base/interactive.html).

## Value

Invisibly returns the created file path. When called without
`strFunctionType`, invisibly returns the available template names.

## Examples

``` r
if (FALSE) { # interactive()
CreateCyneRgyFunction()
CreateCyneRgyFunction( "Analyze.Binary", "NewBinaryAnalysis" )
}
```
