# Extract regression coefficients from a linear regression model.

The regression coefficients demonstrates how a set of predictor
variables will affect the outcome of interest.

## Usage

``` r
b(object, index = NULL, data = NULL)
```

## Arguments

- object:

  An R object that is a formula or contains the results of the \`lm\`
  function.

- index:

  Index indicating which coefficients to obtain.

- data:

  A data frame when the object is a formula.
