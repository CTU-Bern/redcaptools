# Analagous to \`janitor::remove_empty(..., "rows")\`, but allows ignoring specific variables

Analagous to \`janitor::remove_empty(..., "rows")\`, but allows ignoring
specific variables

## Usage

``` r
remove_empty_rows(data, ignore = "^(record_id|redcap)|_complete$")
```

## Arguments

- data:

  a dataframe

- ignore:

  regex identifying variables to ignore

## Value

dataframe

## Examples

``` r
x <- data.frame(a = c(1:9, NA), b = rep(c("b", NA), 5))
remove_empty_rows(x, "a")
#>   a b
#> 1 1 b
#> 3 3 b
#> 5 5 b
#> 7 7 b
#> 9 9 b
remove_empty_rows(x, FALSE)
#>   a    b
#> 1 1    b
#> 2 2 <NA>
#> 3 3    b
#> 4 4 <NA>
#> 5 5    b
#> 6 6 <NA>
#> 7 7    b
#> 8 8 <NA>
#> 9 9    b
```
