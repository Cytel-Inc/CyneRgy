# Combine All R Files

This function combines the contents of all R files in a specified
directory into one file. It also replaces any sequence of one or more
`#` characters with a single `#`.

## Usage

``` r
CombineAllRFiles(
  strOutFileName = NA,
  strDirectory = "",
  strFileNameToIgnore = NA
)
```

## Arguments

- strOutFileName:

  The name of the output file. If not provided, the function will return
  the combined content.

- strDirectory:

  The directory where the R files are located. Defaults to the current
  working directory.

- strFileNameToIgnore:

  The name of any file to be ignored during the combination process.
  Defaults to NA.

## Value

A list containing the following elements:

- nQtyCombinedFiles:

  The number of files combined.

- strCombinedContents:

  The combined content of all the R files (only if strOutFileName is
  NA).

- strReturn:

  A string summarizing the operation, including the names of the
  combined files.

## See also

[`list.files`](https://rdrr.io/r/base/list.files.html),
[`file`](https://rdrr.io/r/base/connections.html),
[`readLines`](https://rdrr.io/r/base/readLines.html),
[`writeLines`](https://rdrr.io/r/base/writeLines.html)

## Examples

``` r
if (FALSE) { # \dontrun{
  result <- CombineAllRFiles(strOutFileName = "combined.R", strDirectory = "/path/to/your/directory")
  print(result$strReturn)
} # }
```
