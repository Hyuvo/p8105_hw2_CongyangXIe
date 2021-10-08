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

``` r
file_path1 = "Trash-Wheel-Collection-Totals-7-2020-2.xlsx"
```

1.  Read and clean the Mr. Trash Wheel sheet

``` r
MrTrashWheel_df <-
  read_excel(file_path1, sheet = "Mr. Trash Wheel") %>%
  janitor::clean_names() %>%
  select(-x15, -x16, -x17) %>%
  drop_na() %>%
  mutate(sports_balls = round(sports_balls))
```

2.  Read and clean precipitation data for 2018 and 2019.

-   For each, omit rows without precipitation data and add a variable
    for year..

``` r
precip18_df <- 
  read_excel(file_path1, sheet = "2018 Precipitation", skip = 1) %>%
  janitor::clean_names() %>%
  drop_na() %>%
  mutate(year = 2018) 
  

precip19_df <- 
  read_excel(file_path1, sheet = "2019 Precipitation", skip = 1) %>%
  janitor::clean_names() %>%
  drop_na() %>%
  mutate(year = 2019)
```

-   Combine precipitation datasets and convert month to a character
    variable.

``` r
precip_df <- 
  full_join(precip18_df, precip19_df) %>%
  arrange(year, month) %>%
  mutate(month = month.name[month]) 
```

    ## Joining, by = c("month", "total", "year")

Be sure to note the number of observations in both resulting datasets,
and give examples of key variables. For available data, what was the
total precipitation in 2018? What was the median number of sports balls
in a dumpster in 2019?

3.  The summary of MrTrashWheel dataset:  

-   The MrTrashWheel dataset has 14 variables and 453 observations. The
    dataset stores trash collect data from each dumpster in the year of
    2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021. The trash type
    includes plastic_bottles, polystyrene, cigarette_butts,
    glass_bottles, grocery_bags, chip_bags, sports_balls. The top3 trash
    types are stated here:

``` r
MrTrashWheel_df %>% select(-year) %>% skimr::skim() %>% select(skim_variable, numeric.mean) %>% top_n(3) %>% arrange(desc(numeric.mean))
```

    ## Selecting by numeric.mean

    ## # A tibble: 3 × 2
    ##   skim_variable   numeric.mean
    ##   <chr>                  <dbl>
    ## 1 cigarette_butts       24522.
    ## 2 polystyrene            1921.
    ## 3 plastic_bottles        1899.

-   The median number of sports balls in a dumpster in 2019 is 9.

4.  The summary of precipitation data:

-   The precipitation dataset has 3 variables and 24 observations. The
    dataset stores trash precipitation data on a monthly basis collected
    from 2018 to 2019. The mean monthly precipitation for 2018 is stated
    below:

``` r
precip_df %>% group_by(year) %>% summarise(mean_month_precip = mean(total))
```

    ## # A tibble: 2 × 2
    ##    year mean_month_precip
    ##   <dbl>             <dbl>
    ## 1  2018              5.86
    ## 2  2019              2.83

-   The total precipitation in 2018 is 70.33
