# Extract the standard errors of the regression coefficients from a linear regression model.

The standard errors of the regression coefficients demonstrates the
variability of the relationship between the predictor variables and the
outcome of interest.

## Usage

``` r
se_b(object, index = NULL, data = NULL)
```

## Arguments

- object:

  An R object that is a formula or contains the results of the \`lm\`
  function.

- index:

  Index indicating which coefficients to obtain.

- data:

  A data frame when the object is a formula.
