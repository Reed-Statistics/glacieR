# glacieR
<!-- badges: start -->
<!-- badges: end -->
The goal of glacieR is to provide a cleaned and interesting dataset for public use. `glacieR` uses data from the World Glacier Inventory, which has collaborated with various non-profit and government organizations to collect data on the world's glaciers since 1989. This package contains data from the most recent inventory, last updated in 2012, on over 130,000 unique glaciers.

Install with `remotes::install_github("Reed-Statistics/glacieR")

## Example
This is a basic example which shows you how to solve a common problem:
``` r
library(glacieR)
## display the 10 largest glaciers by area
glacier %>%
  arrange(desc(total_area)) %>%
  slice(1:10) %>%
  as.data.frame()
```
