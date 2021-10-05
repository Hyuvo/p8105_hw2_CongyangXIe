p8105_hw2_CongyangXie
================
Congyang Xie
10/3/2021

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.4     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(readxl)
```

Read the the Mr. Trash Wheel sheet

``` r
trashWheel_df <-
  read_excel("Trash-Wheel-Collection-Totals-8-6-19.xlsx") %>%
  janitor::clean_names() %>%
  select(-x15, -x16, -17) %>%
  drop_na()
```

    ## New names:
    ## * `` -> ...15
    ## * `` -> ...16
    ## * `` -> ...17
