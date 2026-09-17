# Open a CyneRgy Example

Lists or opens an example stored in the CyneRgy package. Examples are
self-contained folders in the repository; their functions are not added
to the CyneRgy package namespace. RStudio opens the example's project;
VS Code opens its folder, R scripts, and `Description.Rmd`. The R
scripts do not require an RStudio project.

When CyneRgy is installed normally, the example is copied to
`~/CyneRgyExamples` and opened there; an existing copy is reused so user
changes are preserved. When CyneRgy is loaded from a development
checkout with `pkgload`, the repository example is opened directly.
Supply `strDirectory` to choose another copy location. Calling
`RunExample()` without an example name lists all available examples. An
existing example folder or file path can also be supplied.

## Usage

``` r
RunExample(strExample = "", strDirectory = NA, bOpen = interactive())
```

## Arguments

- strExample:

  Character string naming an included example, or an existing example
  folder or file path.

- strDirectory:

  Optional existing directory where an included example should be
  copied. The default, `NA`, uses the development checkout directly or
  copies an installed example to the default user examples directory.

- bOpen:

  Logical value indicating whether to open the example in the active
  IDE. Defaults to
  [`interactive()`](https://rdrr.io/r/base/interactive.html).

## Value

Invisibly returns the example path. When called without `strExample`,
invisibly returns the available example names.

## Details

Set `options( CyneRgy.examples.path = "path" )` to change the default
user examples directory from `~/CyneRgyExamples`. RStudio Desktop and
supported browser-based RStudio sessions use `rstudioapi` to open a
matching `.Rproj` file. Positron can use its supported `rstudioapi`
hooks, and VS Code uses the `code` command when it is available on
`PATH`. Other IDEs can register an opener with
`options( CyneRgy.path.opener = function( strPath ) ... )`. If no
integration is available, the function returns and displays the path so
it can be opened manually.

## Examples

``` r
if (FALSE) { # interactive()
RunExample()
RunExample( "TreatmentSelection" )
RunExample( "TreatmentSelection", strDirectory = getwd() )
}
```
