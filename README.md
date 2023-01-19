# inquisitive

<!-- badges: start -->
<!-- badges: end -->

A collection of exploratory data analysis serving two goals:

1) Clearly characterize source data issues to drive operational improvements
2) Gain insights into the structure and limits of the data to improve feature engineering and modeling

## Installation

You can install the development version of inquisitive from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("johnaclouse/inquisitive")
```

## Work flow

* Capture the starting point
  + The original tibble is used to calculate the impact of the suspected data
  + Do not change the original tibble until finished identifying suspect rows and columns
* Add suspect rows using [add_suspected_rows()](man/add_suspected_columns.Rd)
* Add suspect columns using add_suspected_columns()
* Plot the results using plot_suspect_data()
* Adjusticate the original tibble
  + Apply the function to the original tibble to remove all of the suspect data using 
  



