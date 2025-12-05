# Convert variables to logical

This is particularly useful for binary variables that have been encoded
with e.g. Yes and No as options. Variable labels are retained, which may
or may not make sense, depending on the variable

## Usage

``` r
convert_to_logical(
  data,
  vars,
  true = "Yes",
  replace = TRUE,
  append = "_logical"
)
```

## Arguments

- data:

  dataframe

- vars:

  character string of variables to convert

- true:

  value which should become `TRUE`

- replace:

  Replace the indicated variables

- append:

  text to append to new variables (when `replace = TRUE`)

## Value

`data` with modified variables, potentially with additional variables
(if `replace = TRUE`)

## Examples

``` r
data(mtcars)
convert_to_logical(mtcars, "am", 1)
#>                      mpg cyl  disp  hp drat    wt  qsec vs    am gear carb
#> Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  TRUE    4    4
#> Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  TRUE    4    4
#> Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  TRUE    4    1
#> Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1 FALSE    3    1
#> Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0 FALSE    3    2
#> Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1 FALSE    3    1
#> Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0 FALSE    3    4
#> Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1 FALSE    4    2
#> Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1 FALSE    4    2
#> Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1 FALSE    4    4
#> Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1 FALSE    4    4
#> Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0 FALSE    3    3
#> Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0 FALSE    3    3
#> Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0 FALSE    3    3
#> Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0 FALSE    3    4
#> Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0 FALSE    3    4
#> Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0 FALSE    3    4
#> Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  TRUE    4    1
#> Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  TRUE    4    2
#> Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  TRUE    4    1
#> Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1 FALSE    3    1
#> Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0 FALSE    3    2
#> AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0 FALSE    3    2
#> Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0 FALSE    3    4
#> Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0 FALSE    3    2
#> Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  TRUE    4    1
#> Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  TRUE    5    2
#> Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  TRUE    5    2
#> Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  TRUE    5    4
#> Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  TRUE    5    6
#> Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  TRUE    5    8
#> Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  TRUE    4    2
convert_to_logical(mtcars, c("am", "vs"), 1)
#>                      mpg cyl  disp  hp drat    wt  qsec    vs    am gear carb
#> Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46 FALSE  TRUE    4    4
#> Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02 FALSE  TRUE    4    4
#> Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  TRUE  TRUE    4    1
#> Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  TRUE FALSE    3    1
#> Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02 FALSE FALSE    3    2
#> Valiant             18.1   6 225.0 105 2.76 3.460 20.22  TRUE FALSE    3    1
#> Duster 360          14.3   8 360.0 245 3.21 3.570 15.84 FALSE FALSE    3    4
#> Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  TRUE FALSE    4    2
#> Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  TRUE FALSE    4    2
#> Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  TRUE FALSE    4    4
#> Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  TRUE FALSE    4    4
#> Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40 FALSE FALSE    3    3
#> Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60 FALSE FALSE    3    3
#> Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00 FALSE FALSE    3    3
#> Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98 FALSE FALSE    3    4
#> Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82 FALSE FALSE    3    4
#> Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42 FALSE FALSE    3    4
#> Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  TRUE  TRUE    4    1
#> Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  TRUE  TRUE    4    2
#> Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  TRUE  TRUE    4    1
#> Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  TRUE FALSE    3    1
#> Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87 FALSE FALSE    3    2
#> AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30 FALSE FALSE    3    2
#> Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41 FALSE FALSE    3    4
#> Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05 FALSE FALSE    3    2
#> Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  TRUE  TRUE    4    1
#> Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70 FALSE  TRUE    5    2
#> Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  TRUE  TRUE    5    2
#> Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50 FALSE  TRUE    5    4
#> Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50 FALSE  TRUE    5    6
#> Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60 FALSE  TRUE    5    8
#> Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  TRUE  TRUE    4    2
convert_to_logical(mtcars, c("am", "vs"), 1, FALSE)
#>                      mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
#> Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
#> Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
#> Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
#> Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
#> Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
#> Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
#> Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
#> Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
#> Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
#> Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
#> Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
#> Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
#> Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
#> Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
#> Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
#> Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
#> Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
#> AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2
#> Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
#> Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2
#> Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
#> Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
#> Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
#> Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
#> Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
#> Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
#> Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2
#>                     am_logical vs_logical
#> Mazda RX4                 TRUE      FALSE
#> Mazda RX4 Wag             TRUE      FALSE
#> Datsun 710                TRUE       TRUE
#> Hornet 4 Drive           FALSE       TRUE
#> Hornet Sportabout        FALSE      FALSE
#> Valiant                  FALSE       TRUE
#> Duster 360               FALSE      FALSE
#> Merc 240D                FALSE       TRUE
#> Merc 230                 FALSE       TRUE
#> Merc 280                 FALSE       TRUE
#> Merc 280C                FALSE       TRUE
#> Merc 450SE               FALSE      FALSE
#> Merc 450SL               FALSE      FALSE
#> Merc 450SLC              FALSE      FALSE
#> Cadillac Fleetwood       FALSE      FALSE
#> Lincoln Continental      FALSE      FALSE
#> Chrysler Imperial        FALSE      FALSE
#> Fiat 128                  TRUE       TRUE
#> Honda Civic               TRUE       TRUE
#> Toyota Corolla            TRUE       TRUE
#> Toyota Corona            FALSE       TRUE
#> Dodge Challenger         FALSE      FALSE
#> AMC Javelin              FALSE      FALSE
#> Camaro Z28               FALSE      FALSE
#> Pontiac Firebird         FALSE      FALSE
#> Fiat X1-9                 TRUE       TRUE
#> Porsche 914-2             TRUE      FALSE
#> Lotus Europa              TRUE       TRUE
#> Ford Pantera L            TRUE      FALSE
#> Ferrari Dino              TRUE      FALSE
#> Maserati Bora             TRUE      FALSE
#> Volvo 142E                TRUE       TRUE
convert_to_logical(mtcars, c("am", "vs"), 1, FALSE, "_lgl")
#>                      mpg cyl  disp  hp drat    wt  qsec vs am gear carb am_lgl
#> Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4   TRUE
#> Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4   TRUE
#> Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1   TRUE
#> Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1  FALSE
#> Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2  FALSE
#> Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1  FALSE
#> Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4  FALSE
#> Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2  FALSE
#> Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2  FALSE
#> Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4  FALSE
#> Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4  FALSE
#> Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3  FALSE
#> Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3  FALSE
#> Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3  FALSE
#> Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4  FALSE
#> Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4  FALSE
#> Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4  FALSE
#> Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1   TRUE
#> Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2   TRUE
#> Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1   TRUE
#> Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1  FALSE
#> Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2  FALSE
#> AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2  FALSE
#> Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4  FALSE
#> Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2  FALSE
#> Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1   TRUE
#> Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2   TRUE
#> Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2   TRUE
#> Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4   TRUE
#> Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6   TRUE
#> Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8   TRUE
#> Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2   TRUE
#>                     vs_lgl
#> Mazda RX4            FALSE
#> Mazda RX4 Wag        FALSE
#> Datsun 710            TRUE
#> Hornet 4 Drive        TRUE
#> Hornet Sportabout    FALSE
#> Valiant               TRUE
#> Duster 360           FALSE
#> Merc 240D             TRUE
#> Merc 230              TRUE
#> Merc 280              TRUE
#> Merc 280C             TRUE
#> Merc 450SE           FALSE
#> Merc 450SL           FALSE
#> Merc 450SLC          FALSE
#> Cadillac Fleetwood   FALSE
#> Lincoln Continental  FALSE
#> Chrysler Imperial    FALSE
#> Fiat 128              TRUE
#> Honda Civic           TRUE
#> Toyota Corolla        TRUE
#> Toyota Corona         TRUE
#> Dodge Challenger     FALSE
#> AMC Javelin          FALSE
#> Camaro Z28           FALSE
#> Pontiac Firebird     FALSE
#> Fiat X1-9             TRUE
#> Porsche 914-2        FALSE
#> Lotus Europa          TRUE
#> Ford Pantera L       FALSE
#> Ferrari Dino         FALSE
#> Maserati Bora        FALSE
#> Volvo 142E            TRUE
```
