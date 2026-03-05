# Replace Tags in a File

This function replaces tags in template files with corresponding values.

## Usage

``` r
ReplaceTagsInFile(strFileName, vTags, vReplace)
```

## Arguments

- strFileName:

  The name of the file to use as input. Tags, defined by tags, will be
  replaced with the corresponding values.

- vTags:

  A vector of tag names, e.g., FUNCTION_NAME, VARIABLE_NAME, that will
  be replaced with the values in vReplace.

- vReplace:

  A vector of values to replace the tags with.

## Value

A logical value (TRUE/FALSE) indicating whether the function was
successful.

## Examples

``` r
if (FALSE) { # \dontrun{
vTags    <- c("FUNCTION_NAME", "CREATION_DATE")
vReplace <- c(strNewFunctionName, strToday)
strFileName <- "MyTemplate.R" # A file that contains {{FUNCTION_NAME}} and {{CREATION_DATE}}
ReplaceTagsInFile(strFileName, vTags, vReplace)
} # }
```
