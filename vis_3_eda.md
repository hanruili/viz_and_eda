vis_3_eda
================
Hanrui Li
2024-10-03

``` r
library(tidyverse)
```

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USW00022534", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2021-01-01",
    date_max = "2022-12-31") |>
  mutate(
    name = case_match(
      id, 
      "USW00094728" ~ "CentralPark_NY", 
      "USW00022534" ~ "Molokai_HI",
      "USS0023B17S" ~ "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10,
    month = lubridate::floor_date(date, unit = "month")) |>
  select(name, id, everything())
```

Make some plots:

Initial numeric explorations

``` r
weather_df |> 
  ggplot(aes(x = prcp)) + 
  geom_histogram()
```

![](vis_3_eda_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
weather_df |> 
  filter(prcp > 1000) |>
  knitr::kable()
```

| name           | id          | date       | prcp | tmax | tmin | month      |
|:---------------|:------------|:-----------|-----:|-----:|-----:|:-----------|
| CentralPark_NY | USW00094728 | 2021-08-21 | 1130 | 27.8 | 22.8 | 2021-08-01 |
| CentralPark_NY | USW00094728 | 2021-09-01 | 1811 | 25.6 | 17.2 | 2021-09-01 |
| Molokai_HI     | USW00022534 | 2022-12-18 | 1120 | 23.3 | 18.9 | 2022-12-01 |

``` r
weather_df |> 
  filter(tmax > 20, tmax < 30) |> 
  ggplot(aes(x = tmin, y = tmax, color = name, shape = name)) + 
  geom_point(alpha = .75)
```

![](vis_3_eda_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## `group_by()`

``` r
weather_df |>
  group_by(name)
```

    ## # A tibble: 2,190 × 7
    ## # Groups:   name [3]
    ##    name           id          date        prcp  tmax  tmin month     
    ##    <chr>          <chr>       <date>     <dbl> <dbl> <dbl> <date>    
    ##  1 CentralPark_NY USW00094728 2021-01-01   157   4.4   0.6 2021-01-01
    ##  2 CentralPark_NY USW00094728 2021-01-02    13  10.6   2.2 2021-01-01
    ##  3 CentralPark_NY USW00094728 2021-01-03    56   3.3   1.1 2021-01-01
    ##  4 CentralPark_NY USW00094728 2021-01-04     5   6.1   1.7 2021-01-01
    ##  5 CentralPark_NY USW00094728 2021-01-05     0   5.6   2.2 2021-01-01
    ##  6 CentralPark_NY USW00094728 2021-01-06     0   5     1.1 2021-01-01
    ##  7 CentralPark_NY USW00094728 2021-01-07     0   5    -1   2021-01-01
    ##  8 CentralPark_NY USW00094728 2021-01-08     0   2.8  -2.7 2021-01-01
    ##  9 CentralPark_NY USW00094728 2021-01-09     0   2.8  -4.3 2021-01-01
    ## 10 CentralPark_NY USW00094728 2021-01-10     0   5    -1.6 2021-01-01
    ## # ℹ 2,180 more rows

``` r
weather_df |>
  group_by(name, month)
```

    ## # A tibble: 2,190 × 7
    ## # Groups:   name, month [72]
    ##    name           id          date        prcp  tmax  tmin month     
    ##    <chr>          <chr>       <date>     <dbl> <dbl> <dbl> <date>    
    ##  1 CentralPark_NY USW00094728 2021-01-01   157   4.4   0.6 2021-01-01
    ##  2 CentralPark_NY USW00094728 2021-01-02    13  10.6   2.2 2021-01-01
    ##  3 CentralPark_NY USW00094728 2021-01-03    56   3.3   1.1 2021-01-01
    ##  4 CentralPark_NY USW00094728 2021-01-04     5   6.1   1.7 2021-01-01
    ##  5 CentralPark_NY USW00094728 2021-01-05     0   5.6   2.2 2021-01-01
    ##  6 CentralPark_NY USW00094728 2021-01-06     0   5     1.1 2021-01-01
    ##  7 CentralPark_NY USW00094728 2021-01-07     0   5    -1   2021-01-01
    ##  8 CentralPark_NY USW00094728 2021-01-08     0   2.8  -2.7 2021-01-01
    ##  9 CentralPark_NY USW00094728 2021-01-09     0   2.8  -4.3 2021-01-01
    ## 10 CentralPark_NY USW00094728 2021-01-10     0   5    -1.6 2021-01-01
    ## # ℹ 2,180 more rows

Counting stuff in a group:

``` r
weather_df |>
  group_by(name) |>
  summarise(n_obs = n()) |>
  knitr::kable()
```

| name           | n_obs |
|:---------------|------:|
| CentralPark_NY |   730 |
| Molokai_HI     |   730 |
| Waterhole_WA   |   730 |

``` r
weather_df |>
  group_by(name) |>
  summarise(
    n_obs = n(),
    n_dist = n_distinct(month)
  ) |>
  knitr::kable()
```

| name           | n_obs | n_dist |
|:---------------|------:|-------:|
| CentralPark_NY |   730 |     24 |
| Molokai_HI     |   730 |     24 |
| Waterhole_WA   |   730 |     24 |

``` r
weather_df |>
  group_by(name, month) |>
  summarise(
    n_obs = n()
  )
```

    ## # A tibble: 72 × 3
    ## # Groups:   name [3]
    ##    name           month      n_obs
    ##    <chr>          <date>     <int>
    ##  1 CentralPark_NY 2021-01-01    31
    ##  2 CentralPark_NY 2021-02-01    28
    ##  3 CentralPark_NY 2021-03-01    31
    ##  4 CentralPark_NY 2021-04-01    30
    ##  5 CentralPark_NY 2021-05-01    31
    ##  6 CentralPark_NY 2021-06-01    30
    ##  7 CentralPark_NY 2021-07-01    31
    ##  8 CentralPark_NY 2021-08-01    31
    ##  9 CentralPark_NY 2021-09-01    30
    ## 10 CentralPark_NY 2021-10-01    31
    ## # ℹ 62 more rows

## $2\times2$ table

``` r
weather_df |>
  drop_na(tmax) |>
  filter(name != "Molokai_HI") |>
  mutate(
    cold = case_when(
      tmin < 5 ~ "cold",
      tmax >= 5 ~ "not cold"
    )
  ) |>
  group_by(name, cold) |>
  summarise(count = n()) |>
  knitr::kable()
```

| name           | cold     | count |
|:---------------|:---------|------:|
| CentralPark_NY | cold     |   238 |
| CentralPark_NY | not cold |   492 |
| Waterhole_WA   | cold     |   568 |
| Waterhole_WA   | not cold |   146 |

``` r
weather_df |>
  drop_na(tmax) |>
  filter(name != "Molokai_HI") |>
  mutate(
    cold = case_when(
      tmin < 5 ~ "cold",
      tmax >= 5 ~ "not cold"
    )
  ) |>
  janitor::tabyl(name, cold)
```

    ##            name cold not cold
    ##  CentralPark_NY  238      492
    ##    Waterhole_WA  568      146

General numeric summaries:

``` r
weather_df |>
  group_by(name) |>
  summarise(
    mean_tmax = mean(tmax, na.rm = TRUE)
  ) |>
  knitr::kable()
```

| name           | mean_tmax |
|:---------------|----------:|
| CentralPark_NY | 17.657671 |
| Molokai_HI     | 28.318793 |
| Waterhole_WA   |  7.380112 |

``` r
weather_df |>
  group_by(name, month) |>
  summarise(
    mean_tmax = mean(tmax, na.rm = TRUE),
    median_tmin = median(tmin, na.rm = TRUE),
    sd_prcp = sd(prcp, na.rm = TRUE)
  )
```

    ## # A tibble: 72 × 5
    ## # Groups:   name [3]
    ##    name           month      mean_tmax median_tmin sd_prcp
    ##    <chr>          <date>         <dbl>       <dbl>   <dbl>
    ##  1 CentralPark_NY 2021-01-01      4.27       -0.5     47.3
    ##  2 CentralPark_NY 2021-02-01      3.87       -1.85    98.1
    ##  3 CentralPark_NY 2021-03-01     12.3         5       71.3
    ##  4 CentralPark_NY 2021-04-01     17.6         8.05    52.4
    ##  5 CentralPark_NY 2021-05-01     22.1        11.1     74.7
    ##  6 CentralPark_NY 2021-06-01     28.1        18.0     43.3
    ##  7 CentralPark_NY 2021-07-01     28.4        21.1    151. 
    ##  8 CentralPark_NY 2021-08-01     28.8        22.2    236. 
    ##  9 CentralPark_NY 2021-09-01     24.8        17.5    333. 
    ## 10 CentralPark_NY 2021-10-01     19.9        13.9    151. 
    ## # ℹ 62 more rows

Create a plot based on the monthly summary:

``` r
weather_df |>
  group_by(name, month) |>
  summarise(
    mean_tmax = mean(tmax, na.rm = TRUE),
    median_tmin = median(tmin, na.rm = TRUE),
    sd_prcp = sd(prcp, na.rm = TRUE)
  ) |>
  ggplot(aes(x = month, y = mean_tmax, color = name)) +
  geom_point() +
  geom_line()
```

![](vis_3_eda_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

Format for readers:

``` r
weather_df |>
  group_by(name, month) |>
  summarise(
    mean_tmax = mean(tmax, na.rm = TRUE)
  ) |>
  pivot_wider(
    names_from = name,
    values_from = mean_tmax
  ) |>
  knitr::kable(
    digits = 3,
    col.names = c("Month", "Central Park", "Molokai", "Waterhole"))
```

| Month      | Central Park | Molokai | Waterhole |
|:-----------|-------------:|--------:|----------:|
| 2021-01-01 |        4.271 |  27.616 |     0.800 |
| 2021-02-01 |        3.868 |  26.368 |    -0.786 |
| 2021-03-01 |       12.294 |  25.861 |     2.623 |
| 2021-04-01 |       17.607 |  26.567 |     6.097 |
| 2021-05-01 |       22.084 |  28.577 |     8.203 |
| 2021-06-01 |       28.057 |  29.587 |    15.253 |
| 2021-07-01 |       28.352 |  29.994 |    17.335 |
| 2021-08-01 |       28.810 |  29.523 |    17.152 |
| 2021-09-01 |       24.787 |  29.673 |    12.647 |
| 2021-10-01 |       19.926 |  29.129 |     5.481 |
| 2021-11-01 |       11.537 |  28.847 |     3.533 |
| 2021-12-01 |        9.587 |  26.190 |    -2.097 |
| 2022-01-01 |        2.855 |  26.606 |     3.606 |
| 2022-02-01 |        7.650 |  26.829 |     2.989 |
| 2022-03-01 |       11.990 |  27.726 |     3.416 |
| 2022-04-01 |       15.810 |  27.723 |     2.463 |
| 2022-05-01 |       22.255 |  28.283 |     5.810 |
| 2022-06-01 |       26.090 |  29.157 |    11.127 |
| 2022-07-01 |       30.723 |  29.529 |    15.861 |
| 2022-08-01 |       30.500 |  30.697 |    18.830 |
| 2022-09-01 |       24.923 |  30.413 |    15.207 |
| 2022-10-01 |       17.426 |  29.223 |    11.884 |
| 2022-11-01 |       14.017 |  27.960 |     2.140 |
| 2022-12-01 |        6.761 |  27.348 |    -0.460 |

## Grouped mutates

``` r
weather_df |>
  group_by(name) |>
  mutate(mean_tmax = mean(tmax, na.rm = TRUE))
```

    ## # A tibble: 2,190 × 8
    ## # Groups:   name [3]
    ##    name           id          date        prcp  tmax  tmin month      mean_tmax
    ##    <chr>          <chr>       <date>     <dbl> <dbl> <dbl> <date>         <dbl>
    ##  1 CentralPark_NY USW00094728 2021-01-01   157   4.4   0.6 2021-01-01      17.7
    ##  2 CentralPark_NY USW00094728 2021-01-02    13  10.6   2.2 2021-01-01      17.7
    ##  3 CentralPark_NY USW00094728 2021-01-03    56   3.3   1.1 2021-01-01      17.7
    ##  4 CentralPark_NY USW00094728 2021-01-04     5   6.1   1.7 2021-01-01      17.7
    ##  5 CentralPark_NY USW00094728 2021-01-05     0   5.6   2.2 2021-01-01      17.7
    ##  6 CentralPark_NY USW00094728 2021-01-06     0   5     1.1 2021-01-01      17.7
    ##  7 CentralPark_NY USW00094728 2021-01-07     0   5    -1   2021-01-01      17.7
    ##  8 CentralPark_NY USW00094728 2021-01-08     0   2.8  -2.7 2021-01-01      17.7
    ##  9 CentralPark_NY USW00094728 2021-01-09     0   2.8  -4.3 2021-01-01      17.7
    ## 10 CentralPark_NY USW00094728 2021-01-10     0   5    -1.6 2021-01-01      17.7
    ## # ℹ 2,180 more rows

``` r
weather_df |>
  group_by(name) |>
  mutate(
    mean_tmax = mean(tmax, na.rm = TRUE),
    centered_tmax = tmax - mean_tmax) |>
  ggplot(aes(x = date, y = centered_tmax, color = name)) +
  geom_point()
```

![](vis_3_eda_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

Find hottest / coldest days:

``` r
weather_df |>
  mutate(
    temp_rank = min_rank(tmax)
  ) |>
  filter(temp_rank < 10) |>
  knitr::kable()
```

| name           | id          | date       | prcp |  tmax |  tmin | month      | temp_rank |
|:---------------|:------------|:-----------|-----:|------:|------:|:-----------|----------:|
| CentralPark_NY | USW00094728 | 2022-01-15 |    0 |  -6.0 | -12.1 | 2022-01-01 |         7 |
| CentralPark_NY | USW00094728 | 2022-12-24 |    0 |  -9.3 | -13.8 | 2022-12-01 |         4 |
| Waterhole_WA   | USS0023B17S | 2021-02-11 |   51 |  -5.6 | -10.9 | 2021-02-01 |         9 |
| Waterhole_WA   | USS0023B17S | 2021-12-26 |  102 | -11.4 | -18.3 | 2021-12-01 |         1 |
| Waterhole_WA   | USS0023B17S | 2021-12-27 |   25 |  -9.8 | -19.6 | 2021-12-01 |         2 |
| Waterhole_WA   | USS0023B17S | 2021-12-28 |    0 |  -6.0 | -11.4 | 2021-12-01 |         7 |
| Waterhole_WA   | USS0023B17S | 2021-12-29 |  102 |  -7.9 | -15.4 | 2021-12-01 |         6 |
| Waterhole_WA   | USS0023B17S | 2022-02-22 |  102 |  -9.3 | -16.6 | 2022-02-01 |         4 |
| Waterhole_WA   | USS0023B17S | 2022-12-18 |    0 |  -5.6 | -11.3 | 2022-12-01 |         9 |
| Waterhole_WA   | USS0023B17S | 2022-12-21 |    0 |  -9.6 | -18.4 | 2022-12-01 |         3 |

``` r
weather_df |>
  group_by(name) |>
  mutate(
    temp_rank = min_rank(tmax)
  ) |>
  filter(temp_rank < 4) |>
  knitr::kable()
```

| name           | id          | date       | prcp |  tmax |  tmin | month      | temp_rank |
|:---------------|:------------|:-----------|-----:|------:|------:|:-----------|----------:|
| CentralPark_NY | USW00094728 | 2022-01-15 |    0 |  -6.0 | -12.1 | 2022-01-01 |         2 |
| CentralPark_NY | USW00094728 | 2022-01-21 |    0 |  -5.5 |  -9.9 | 2022-01-01 |         3 |
| CentralPark_NY | USW00094728 | 2022-12-24 |    0 |  -9.3 | -13.8 | 2022-12-01 |         1 |
| Molokai_HI     | USW00022534 | 2021-01-18 |  234 |  22.2 |  19.4 | 2021-01-01 |         2 |
| Molokai_HI     | USW00022534 | 2021-03-18 |  142 |  21.7 |  18.9 | 2021-03-01 |         1 |
| Molokai_HI     | USW00022534 | 2022-11-28 |   56 |  22.2 |  20.6 | 2022-11-01 |         2 |
| Waterhole_WA   | USS0023B17S | 2021-12-26 |  102 | -11.4 | -18.3 | 2021-12-01 |         1 |
| Waterhole_WA   | USS0023B17S | 2021-12-27 |   25 |  -9.8 | -19.6 | 2021-12-01 |         2 |
| Waterhole_WA   | USS0023B17S | 2022-12-21 |    0 |  -9.6 | -18.4 | 2022-12-01 |         3 |

`desc()` descending functionn:

``` r
weather_df |>
  group_by(name) |>
  mutate(
    temp_rank = min_rank(desc(tmax))
  ) |>
  filter(temp_rank < 4) |>
  knitr::kable()
```

| name           | id          | date       | prcp | tmax | tmin | month      | temp_rank |
|:---------------|:------------|:-----------|-----:|-----:|-----:|:-----------|----------:|
| CentralPark_NY | USW00094728 | 2021-06-29 |    0 | 35.0 | 25.6 | 2021-06-01 |         3 |
| CentralPark_NY | USW00094728 | 2021-06-30 |  165 | 36.7 | 22.8 | 2021-06-01 |         1 |
| CentralPark_NY | USW00094728 | 2022-07-20 |    0 | 35.0 | 25.6 | 2022-07-01 |         3 |
| CentralPark_NY | USW00094728 | 2022-07-23 |    0 | 35.0 | 25.6 | 2022-07-01 |         3 |
| CentralPark_NY | USW00094728 | 2022-07-24 |    0 | 35.0 | 26.1 | 2022-07-01 |         3 |
| CentralPark_NY | USW00094728 | 2022-08-09 |    8 | 36.1 | 25.6 | 2022-08-01 |         2 |
| Molokai_HI     | USW00022534 | 2021-05-31 |    0 | 32.2 | 17.2 | 2021-05-01 |         2 |
| Molokai_HI     | USW00022534 | 2021-09-16 |    0 | 32.2 | 21.1 | 2021-09-01 |         2 |
| Molokai_HI     | USW00022534 | 2022-07-30 |    0 | 32.2 | 22.2 | 2022-07-01 |         2 |
| Molokai_HI     | USW00022534 | 2022-08-06 |    0 | 33.3 | 20.6 | 2022-08-01 |         1 |
| Molokai_HI     | USW00022534 | 2022-08-17 |    0 | 32.2 | 21.1 | 2022-08-01 |         2 |
| Molokai_HI     | USW00022534 | 2022-09-24 |    0 | 32.2 | 22.2 | 2022-09-01 |         2 |
| Molokai_HI     | USW00022534 | 2022-09-30 |    0 | 32.2 | 20.0 | 2022-09-01 |         2 |
| Waterhole_WA   | USS0023B17S | 2021-06-27 |    0 | 28.5 | 17.6 | 2021-06-01 |         3 |
| Waterhole_WA   | USS0023B17S | 2021-06-28 |    0 | 30.8 | 20.7 | 2021-06-01 |         2 |
| Waterhole_WA   | USS0023B17S | 2021-06-29 |    0 | 32.4 | 17.6 | 2021-06-01 |         1 |

``` r
weather_df |>
  group_by(name) |>
  filter(min_rank(tmax) < 4) |>
  arrange(tmax) |>
  knitr::kable()
```

| name           | id          | date       | prcp |  tmax |  tmin | month      |
|:---------------|:------------|:-----------|-----:|------:|------:|:-----------|
| Waterhole_WA   | USS0023B17S | 2021-12-26 |  102 | -11.4 | -18.3 | 2021-12-01 |
| Waterhole_WA   | USS0023B17S | 2021-12-27 |   25 |  -9.8 | -19.6 | 2021-12-01 |
| Waterhole_WA   | USS0023B17S | 2022-12-21 |    0 |  -9.6 | -18.4 | 2022-12-01 |
| CentralPark_NY | USW00094728 | 2022-12-24 |    0 |  -9.3 | -13.8 | 2022-12-01 |
| CentralPark_NY | USW00094728 | 2022-01-15 |    0 |  -6.0 | -12.1 | 2022-01-01 |
| CentralPark_NY | USW00094728 | 2022-01-21 |    0 |  -5.5 |  -9.9 | 2022-01-01 |
| Molokai_HI     | USW00022534 | 2021-03-18 |  142 |  21.7 |  18.9 | 2021-03-01 |
| Molokai_HI     | USW00022534 | 2021-01-18 |  234 |  22.2 |  19.4 | 2021-01-01 |
| Molokai_HI     | USW00022534 | 2022-11-28 |   56 |  22.2 |  20.6 | 2022-11-01 |

`lag` function:

``` r
weather_df |>
  mutate(
    lagged_temp = lag(tmax)
  )
```

    ## # A tibble: 2,190 × 8
    ##    name           id         date        prcp  tmax  tmin month      lagged_temp
    ##    <chr>          <chr>      <date>     <dbl> <dbl> <dbl> <date>           <dbl>
    ##  1 CentralPark_NY USW000947… 2021-01-01   157   4.4   0.6 2021-01-01        NA  
    ##  2 CentralPark_NY USW000947… 2021-01-02    13  10.6   2.2 2021-01-01         4.4
    ##  3 CentralPark_NY USW000947… 2021-01-03    56   3.3   1.1 2021-01-01        10.6
    ##  4 CentralPark_NY USW000947… 2021-01-04     5   6.1   1.7 2021-01-01         3.3
    ##  5 CentralPark_NY USW000947… 2021-01-05     0   5.6   2.2 2021-01-01         6.1
    ##  6 CentralPark_NY USW000947… 2021-01-06     0   5     1.1 2021-01-01         5.6
    ##  7 CentralPark_NY USW000947… 2021-01-07     0   5    -1   2021-01-01         5  
    ##  8 CentralPark_NY USW000947… 2021-01-08     0   2.8  -2.7 2021-01-01         5  
    ##  9 CentralPark_NY USW000947… 2021-01-09     0   2.8  -4.3 2021-01-01         2.8
    ## 10 CentralPark_NY USW000947… 2021-01-10     0   5    -1.6 2021-01-01         2.8
    ## # ℹ 2,180 more rows

``` r
weather_df |>
  group_by(name) |>
  mutate(
    lagged_temp = lag(tmax),
    temp_change = tmax - lagged_temp
  ) |>
  filter(min_rank(temp_change) < 3) |>
  knitr::kable()
```

| name           | id          | date       | prcp | tmax |  tmin | month      | lagged_temp | temp_change |
|:---------------|:------------|:-----------|-----:|-----:|------:|:-----------|------------:|------------:|
| CentralPark_NY | USW00094728 | 2022-02-24 |    0 |  1.7 |  -1.6 | 2022-02-01 |        20.0 |       -18.3 |
| CentralPark_NY | USW00094728 | 2022-12-24 |    0 | -9.3 | -13.8 | 2022-12-01 |        14.4 |       -23.7 |
| Molokai_HI     | USW00022534 | 2021-01-18 |  234 | 22.2 |  19.4 | 2021-01-01 |        27.8 |        -5.6 |
| Molokai_HI     | USW00022534 | 2022-11-28 |   56 | 22.2 |  20.6 | 2022-11-01 |        27.2 |        -5.0 |
| Waterhole_WA   | USS0023B17S | 2021-06-30 |    0 | 21.5 |  10.9 | 2021-06-01 |        32.4 |       -10.9 |
| Waterhole_WA   | USS0023B17S | 2022-06-28 |    0 | 12.4 |   5.7 | 2022-06-01 |        23.6 |       -11.2 |

``` r
weather_df |>
  group_by(name) |> 
  mutate(
    lagged_temp = lag(tmax),
    temp_change = tmax - lagged_temp
  ) |>
  summarise(
    sd_temp_change = sd(temp_change, na.rm = TRUE),
    max_temp_change = max(temp_change, na.rm = TRUE)
  ) |>
  knitr::kable()
```

| name           | sd_temp_change | max_temp_change |
|:---------------|---------------:|----------------:|
| CentralPark_NY |       4.431918 |            12.2 |
| Molokai_HI     |       1.244298 |             5.6 |
| Waterhole_WA   |       3.039734 |            11.1 |

## Learning assessment

``` r
library(haven)

pulse_df = 
  read_sas("data/public_pulse_data.sas7bdat") |>
  janitor::clean_names() |>
  pivot_longer(
    cols = bdi_score_bl:bdi_score_12m,
    names_to = "visit",
    values_to = "bdi_score",
    names_prefix = "bdi_score_"
  )

pulse_df|>
  group_by(visit) |>
  summarise(
    mean_bdi = mean(bdi_score, na.rm = TRUE)
  ) |>
  knitr::kable(digits = 1)
```

| visit | mean_bdi |
|:------|---------:|
| 01m   |      6.0 |
| 06m   |      5.7 |
| 12m   |      6.1 |
| bl    |      8.0 |

## FAS data

``` r
litters_df = 
  read_csv("data/FAS_litters.csv", na = c("NA", ".", "")) |>
  janitor::clean_names() |>
  separate(
    group, into = c("dose", "tx_day"), sep = 3)

pups_df =
  read_csv("data/FAS_pups.csv", na = c("NA", ".", "")) |>
  janitor::clean_names()

fas_df = 
  left_join(pups_df, litters_df, by = "litter_number")
```

Create a table that we care about:

``` r
fas_df |> 
  group_by(dose, tx_day) |> 
  drop_na(dose) |> 
  summarize(
    mean_pivot = mean(pd_pivot, na.rm = TRUE)
  ) |>
  knitr::kable()
```

| dose | tx_day | mean_pivot |
|:-----|:-------|-----------:|
| Con  | 7      |   7.000000 |
| Con  | 8      |   6.236364 |
| Low  | 7      |   7.938776 |
| Low  | 8      |   7.720930 |
| Mod  | 7      |   6.983607 |
| Mod  | 8      |   7.041667 |

``` r
fas_df |> 
  group_by(dose, tx_day) |> 
  drop_na(dose) |> 
  summarize(
    mean_pivot = mean(pd_pivot, na.rm = TRUE)
  ) |> 
  pivot_wider(
    names_from = tx_day, 
    values_from = mean_pivot) |> 
  knitr::kable(digits = 3)
```

| dose |     7 |     8 |
|:-----|------:|------:|
| Con  | 7.000 | 6.236 |
| Low  | 7.939 | 7.721 |
| Mod  | 6.984 | 7.042 |
