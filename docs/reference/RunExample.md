# Launch Example from the CyneRgy Package

This function opens an example included in the CyneRgy package. A new
instance of RStudio will launch and open the requested example. To
obtain a current list of available examples, call
`CyneRgy::RunExample()`, and it will display the list.

## Usage

``` r
RunExample(strExample)
```

## Note

The function requires RStudio to open the example projects
automatically. If the RStudio API is unavailable, a manual process will
be needed.

## Examples

``` r
if (FALSE) { # \dontrun{
CyneRgy::RunExample("TreatmentSelection")
} # }
```
