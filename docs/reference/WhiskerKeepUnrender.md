# Render Template While Preserving Unmatched Tags

This function is used internally. It renders a template string using the
`whisker` package, but preserves any tags (`{{tag}}`) that do not have a
matching key in the provided list. It does so by first augmenting the
list with any missing tags using their unrendered form, then calling
`whisker.render()`.

## Usage

``` r
WhiskerKeepUnrender(sTemplate, lList)
```

## Arguments

- sTemplate:

  A string template containing `{{tag}}` placeholders.

- lList:

  A named list of tag-value pairs to use in rendering. Tags not found in
  this list will be preserved in unrendered form.

## Value

A rendered string with all known tags replaced, and unknown tags kept as
`{{tag}}`.

## See also

[`AddUnrenderToList`](https://Cytel-Inc.github.io/CyneRgy/reference/AddUnrenderToList.md),
[`whisker.render`](https://rdrr.io/pkg/whisker/man/whisker.render.html)
