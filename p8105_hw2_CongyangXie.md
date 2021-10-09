p8105_hw2_CongyangXie
================
Congyang Xie
10/3/2021

# P8105 homework2

``` r
library(tidyverse)
library(readxl)
library(lubridate)
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

## Problem 2 Merge FiveThirtyEight datasets into 1 dataframe.

1.  clean the data in pols-month.csv.

``` r
# Use separate() to break up the variable mon into integer variables year, month, and day; replace month number with month name; create a president variable taking values gop and dem, and remove prez_dem and prez_gop; and remove the day variable.

pols_df <-
  read_csv("fivethirtyeight_datasets/pols-month.csv") %>%
  janitor::clean_names() %>%
  # Use separate() to break up the variable mon into integer variables year, month, and day
  separate(mon, c("year", "month", "day"), sep = "-") %>%
  mutate(year = as.integer(year),
         # replace month number with month name
         month = month.name[as.integer(month)],
         day = as.integer(day)) %>%
  pivot_longer(
    cols = starts_with("prez"),
    names_to = "president",
    values_to = "value",
    names_prefix = "prez_"
  ) %>%
  select(-value, -day)
```

2.  clean the data in snp.csv

``` r
snp_df <-
  read_csv("fivethirtyeight_datasets/snp.csv") %>%
  janitor::clean_names() %>%
  mutate(date = mdy(date)) %>%
  # Use separate() to break up the variable date into integer variables year, month, and day
  separate(date, c("year", "month", "day"), sep = "-") %>%
  mutate(# replace month number with month name
    month = month.name[as.integer(month)],
    day = as.integer(day),
    year = as.integer(year)) %>%
  arrange(year, month) %>%
  relocate(year, month)
```

3.  tidy the unemployment data so that it can be merged with the
    previous datasets

``` r
unemployment_df <-
  read_csv("fivethirtyeight_datasets/unemployment.csv") %>%
  pivot_longer(cols = 2:13,
               names_to = "month",
               values_to = "unemployment rate") %>%
  janitor::clean_names() %>%
  # convert month from abbreviation to full
  
  mutate(month = month.name[match(month, month.abb)])
```

4.  Join the datasets by merging snp into pols, and merging unemployment
    into the result

``` r
final_df <- 
  left_join(pols_df, snp_df, by = c("year", "month")) %>%
  left_join(unemployment_df, by = c("year", "month"))
```

5.  Write a short paragraph about these datasets. Explain briefly what
    each dataset contained, and describe the resulting dataset
    (e.g. give the dimension, range of years, and names of key
    variables).

-   The pols dataset has 9 variables and 1644 observations, containing
    the number of national politicians who are democratic or republican
    at time ranging from 1947 to 2015. The variables in the dataset are:
    year, month, gov_gop, sen_gop, rep_gop, gov_dem, sen_dem, rep_dem,
    president.

-   The snp dataset has 4 variables and 787 observations, containing
    Standard & Poor’s stock market index (S&P) at time ranging from 1969
    to 2068.The variables in the dataset are: year, month, day, close.

-   The unemployment dataset has 3 variables and 816 observations,
    containing unemployment rate at time ranging from 1948 to 2015. The
    variables in the dataset are: year, month, unemployment_rate.

-   The final dataset is merged by the 3 datasets above, which has 3
    variables and 816 observations, containing the number of national
    politicians who are democratic or republican, S&P, and unemployment
    rate at time ranging from 1948 to 2015.The variables in the dataset
    are: year, month, unemployment_rate.

## Problem 3

1.  Load and tidy dataset popular baby names.

``` r
popular_baby_names_df <- read_csv("Popular_Baby_Names.csv") %>%
  janitor::clean_names() %>%
  mutate(
    ethnicity = recode(
      ethnicity,
      "BLACK NON HISP" = "BLACK NON HISPANIC",
      "WHITE NON HISP" = "WHITE NON HISPANIC",
      "ASIAN AND PACI" = "ASIAN AND PACIFIC ISLANDER"
    )
  ) %>%
  distinct()
```

2.  create tabel showing the rank in popularity of the name “Olivia” as
    a female baby name over time.

``` r
# the rank in popularity of the name “Olivia” as a female baby name over time
popular_baby_names_df %>%
  filter(childs_first_name == "Olivia") %>%
  filter(gender =="FEMALE") %>%
  relocate(year_of_birth, rank) %>%
  group_by(year_of_birth) %>%
  arrange(year_of_birth, rank) %>%
  select(-childs_first_name)
```

    ## # A tibble: 16 × 5
    ## # Groups:   year_of_birth [4]
    ##    year_of_birth  rank gender ethnicity                  count
    ##            <dbl> <dbl> <chr>  <chr>                      <dbl>
    ##  1          2013     1 FEMALE WHITE NON HISPANIC           233
    ##  2          2013     3 FEMALE ASIAN AND PACIFIC ISLANDER   109
    ##  3          2013     6 FEMALE BLACK NON HISPANIC            64
    ##  4          2013    22 FEMALE HISPANIC                      87
    ##  5          2014     1 FEMALE ASIAN AND PACIFIC ISLANDER   141
    ##  6          2014     1 FEMALE WHITE NON HISPANIC           248
    ##  7          2014     8 FEMALE BLACK NON HISPANIC            52
    ##  8          2014    16 FEMALE HISPANIC                      96
    ##  9          2015     1 FEMALE ASIAN AND PACIFIC ISLANDER   188
    ## 10          2015     1 FEMALE WHITE NON HISPANIC           225
    ## 11          2015     4 FEMALE BLACK NON HISPANIC            82
    ## 12          2015    16 FEMALE HISPANIC                      94
    ## 13          2016     1 FEMALE ASIAN AND PACIFIC ISLANDER   172
    ## 14          2016     1 FEMALE WHITE NON HISPANIC           230
    ## 15          2016     8 FEMALE BLACK NON HISPANIC            49
    ## 16          2016    13 FEMALE HISPANIC                     108

3.  create table showing the most popular name among male children over
    time.

``` r
# the most popular name among male children over time
popular_baby_names_df %>%
  filter(gender =="MALE") %>%
  relocate(year_of_birth, rank, childs_first_name) %>%
  group_by(year_of_birth) %>%
  arrange(rank, year_of_birth) %>%
  filter(rank == 1)
```

    ## # A tibble: 24 × 6
    ## # Groups:   year_of_birth [6]
    ##    year_of_birth  rank childs_first_name gender ethnicity                  count
    ##            <dbl> <dbl> <chr>             <chr>  <chr>                      <dbl>
    ##  1          2011     1 ETHAN             MALE   ASIAN AND PACIFIC ISLANDER   177
    ##  2          2011     1 JAYDEN            MALE   BLACK NON HISPANIC           184
    ##  3          2011     1 JAYDEN            MALE   HISPANIC                     426
    ##  4          2011     1 MICHAEL           MALE   WHITE NON HISPANIC           292
    ##  5          2012     1 RYAN              MALE   ASIAN AND PACIFIC ISLANDER   197
    ##  6          2012     1 JAYDEN            MALE   BLACK NON HISPANIC           171
    ##  7          2012     1 JAYDEN            MALE   HISPANIC                     364
    ##  8          2012     1 JOSEPH            MALE   WHITE NON HISPANIC           300
    ##  9          2013     1 Jayden            MALE   ASIAN AND PACIFIC ISLANDER   220
    ## 10          2013     1 Ethan             MALE   BLACK NON HISPANIC           146
    ## # … with 14 more rows

4.  For male, white non-hispanic children born in 2016, produce a
    scatter plot showing the number of children with a name (y axis)
    against the rank in popularity of that name (x axis).

``` r
plt_df <- popular_baby_names_df %>%
  filter(gender == "MALE" &
           ethnicity == "WHITE NON HISPANIC" &
           year_of_birth == 2016) 
# scatter plot
ggplot(plt_df, aes(x = rank, y = count)) +
  geom_point(alpha = .5)+
  geom_text(aes(label=childs_first_name), size=3)
```

![](p8105_hw2_CongyangXie_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->
