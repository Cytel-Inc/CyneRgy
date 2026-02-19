# Initialize CyneRgy Library for Simulations

The Initialize function initializes the R environment for all
simulations. It is optional and is executed before any user-defined
functions. Key functionalities include:

- Setting a seed for random number generation

- Loading required packages

Here, the CyneRgy library is initialized.

## Usage

``` r
InitLoadCyneRgy(Seed, UserParam = NULL)
```

## Arguments

- Seed:

  An integer value to set the seed used for generating random numbers
  in R. Default is NULL.

- UserParam:

  A named list to pass custom scalar variables defined by users.
  Variables can be accessed using names, e.g., `UserParam$Var1`. Default
  is NULL.

## Value

A list containing:

- ErrorCode:

  An integer indicating success or error status:

  ErrorCode = 0

  :   No error.

  ErrorCode \> 0

  :   Nonfatal error, current simulation aborted but subsequent
      simulations will run.

  ErrorCode \< 0

  :   Fatal error, no further simulations attempted.

## Note

Do not use `install.packages` or attempt to install new R packages in
East Horizon, as this will fail. Please contact support to install
libraries.
