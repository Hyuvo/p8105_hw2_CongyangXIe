p8105_hw2_CongyangXie
================
Congyang Xie
10/3/2021

# P8105 homework2

``` r
library(tidyverse)
library(readxl)
```

## Problem 1

1.  Read and clean the Mr. Trash Wheel sheet

``` r
trashWheel_df <-
  read_excel("Trash-Wheel-Collection-Totals-8-6-19.xlsx") %>%
  janitor::clean_names() %>%
  select(-x15, -x16, -x17) %>%
  drop_na() %>%
  mutate(sports_balls = round(sports_balls))
```
