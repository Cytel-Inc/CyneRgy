# Create a CyneRgy Function Template

This function generates a new R script file containing a template for a
CyneRgy function, which can be integrated with Cytel products. The
template is selected based on the provided function type.

## Usage

``` r
CreateCyneRgyFunction(
  strFunctionType = "",
  strNewFunctionName = NA,
  strDirectory = NA,
  bOpen = TRUE
)
```

## Arguments

- strFunctionType:

  A string specifying the type of function to create. This determines
  the integration template to use. Valid values for `strFunctionType`
  can be obtained from the available templates.

- strNewFunctionName:

  The name of the new function to be created. This also determines the
  name of the resulting file, which will be named
  `[strNewFunctionName].R`. If no value is provided, the default name
  will be derived from `strFunctionType`.

- strDirectory:

  The directory where the new file will be created. If not provided, the
  file will be created in the current working directory. A sub-directory
  with this name will also be created if it does not exist.

- bOpen:

  Logical value (TRUE/FALSE). When TRUE, the newly created file will be
  opened in RStudio using the RStudio API (works only in RStudio). When
  FALSE, the file will be created but not opened.

## Examples

``` r
if (FALSE) { # \dontrun{
CreateCyneRgyFunction()  # Run without arguments to see valid options for `strFunctionType`.

# Example: Create a new file named `NewBinaryAnalysis.R` using the `Analyze.Binary` template.
CreateCyneRgyFunction("Analyze.Binary", "NewBinaryAnalysis")
} # }
```
