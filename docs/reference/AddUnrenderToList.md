# Add Unrendered Tags to List

This function is used internally. It scans a template string for tags in
the format `{{tag}}` and adds any tags not already present in the
provided list to the list, assigning their unrendered form (`{{tag}}`)
as the value.

## Usage

``` r
AddUnrenderToList(sTemplate, lList)
```

## Arguments

- sTemplate:

  A string template that may contain tags in the form `{{tag}}`.

- lList:

  A named list of existing tag-value pairs. The function will not
  overwrite any existing keys.

## Value

A named list with the original elements of `lList`, plus any new tags
found in the template that were not already included.

## See also

[`str_extract_all`](https://stringr.tidyverse.org/reference/str_extract.html),
[`substring`](https://rdrr.io/r/base/substr.html),
[`names`](https://rdrr.io/r/base/names.html)
