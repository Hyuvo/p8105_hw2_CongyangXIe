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
MrTrashWheel_df <-
  read_excel("Trash-Wheel-Collection-Totals-8-6-19.xlsx", sheet = "Mr. Trash Wheel") %>%
  janitor::clean_names() %>%
  select(-x15, -x16, -x17) %>%
  drop_na() %>%
  mutate(sports_balls = round(sports_balls))
```

2.  Read and clean precipitation data for 2018 and 2019.

-   For each, omit rows without precipitation data and add a variable
    for year..

``` r
precip18_df <- read_excel("Trash-Wheel-Collection-Totals-8-6-19.xlsx", sheet = "2018 Precipitation", skip = 1) %>%
  janitor::clean_names() %>%
  drop_na() %>%
  mutate(year = 2018) 
  

precip19_df <- read_excel("Trash-Wheel-Collection-Totals-8-6-19.xlsx", sheet = "2019 Precipitation", skip = 1) %>%
  janitor::clean_names() %>%
  drop_na() %>%
  mutate(year = 2019)
```

-   Combine precipitation datasets and convert month to a character
    variable.

``` r
precip_df <- full_join(precip18_df, precip19_df) %>%
  mutate(month = month.name[month])
```

    ## Joining, by = c("month", "total", "year")
