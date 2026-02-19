# Create a New CyneRgy Example Using Templates

This function creates a new directory containing the necessary files for
the desired CyneRgy template. The directory can be used in connection
with Cytel-R integration.

## Usage

``` r
CreateCyneRgyExample(
  strFunctionType,
  strNewExampleName = "",
  strDirectory = NA
)
```

## Arguments

- strFunctionType:

  The type of CyneRgy template to use. Must be a valid template name.

- strNewExampleName:

  A string representing the name of the new example directory. Defaults
  to an empty string.

- strDirectory:

  The directory path where the example will be created. If not provided,
  the current working directory is used.

## Value

Creates the specified example directory and files within the provided or
default directory path.
