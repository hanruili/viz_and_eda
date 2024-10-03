vis_2
================
Hanrui Li
2024-10-01

``` r
library(tidyverse)
library(patchwork)
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
    tmax = tmax / 10) |>
  select(name, id, everything())

weather_df
```

    ## # A tibble: 2,190 × 6
    ##    name           id          date        prcp  tmax  tmin
    ##    <chr>          <chr>       <date>     <dbl> <dbl> <dbl>
    ##  1 CentralPark_NY USW00094728 2021-01-01   157   4.4   0.6
    ##  2 CentralPark_NY USW00094728 2021-01-02    13  10.6   2.2
    ##  3 CentralPark_NY USW00094728 2021-01-03    56   3.3   1.1
    ##  4 CentralPark_NY USW00094728 2021-01-04     5   6.1   1.7
    ##  5 CentralPark_NY USW00094728 2021-01-05     0   5.6   2.2
    ##  6 CentralPark_NY USW00094728 2021-01-06     0   5     1.1
    ##  7 CentralPark_NY USW00094728 2021-01-07     0   5    -1  
    ##  8 CentralPark_NY USW00094728 2021-01-08     0   2.8  -2.7
    ##  9 CentralPark_NY USW00094728 2021-01-09     0   2.8  -4.3
    ## 10 CentralPark_NY USW00094728 2021-01-10     0   5    -1.6
    ## # ℹ 2,180 more rows

``` r
weather_df |> 
  ggplot(aes(x = tmin, y = tmax)) + 
  geom_point(aes(color = name), alpha = .5)
```

![](vis_2_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
weather_df |> 
  ggplot(aes(x = tmin, y = tmax, color = name)) + 
  geom_point(alpha = .3) + 
  labs(
    title = "Temperature Scatterplot",
    x = "Minimum Temp (C)",
    y = "Maximum Temp (C)",
    color = "Location",
    caption = "Weather data taken from rnoaa package for three stationns."
  )
```

![](vis_2_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Scales – start with `x` and `y`

``` r
weather_df |> 
  ggplot(aes(x = tmin, y = tmax, color = name)) + 
  geom_point(alpha = .3) + 
  labs(
    title = "Temperature Scatterplot",
    x = "Minimum Temp (C)",
    y = "Maximum Temp (C)",
    color = "Location",
    caption = "Weather data taken from rnoaa package for three stationns."
  ) +
  scale_x_continuous(
    breaks = c(-15, 0, 20),
    labels = c("-15C", "0", "20")
  ) +
  scale_y_continuous(
    limits = c(0, 30),
    transform = "sqrt"
  )
```

![](vis_2_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Look at color:

``` r
weather_df |> 
  ggplot(aes(x = tmin, y = tmax, color = name)) + 
  geom_point(alpha = .3) + 
  labs(
    title = "Temperature Scatterplot",
    x = "Minimum Temp (C)",
    y = "Maximum Temp (C)",
    color = "Location",
    caption = "Weather data taken from rnoaa package for three stationns."
  ) +
  viridis::scale_color_viridis(discrete = TRUE)
```

![](vis_2_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

# Themes

``` r
ggp_scatterplot =
  weather_df |> 
  ggplot(aes(x = tmin, y = tmax, color = name)) + 
  geom_point(alpha = .3) + 
  labs(
    title = "Temperature Scatterplot",
    x = "Minimum Temp (C)",
    y = "Maximum Temp (C)",
    color = "Location",
    caption = "Weather data taken from rnoaa package for three stationns."
  ) +
  viridis::scale_color_viridis(discrete = TRUE)
```

``` r
ggp_scatterplot +
  theme(legend.position = "bottom")
```

![](vis_2_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
ggp_scatterplot + 
  theme_bw() +
  theme(legend.position = "bottom")
```

![](vis_2_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
ggp_scatterplot + 
  theme(legend.position = "bottom") + 
  theme_minimal()
```

![](vis_2_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
ggp_scatterplot + 
  theme(legend.position = "bottom") +
  theme_classic()
```

![](vis_2_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
ggp_scatterplot + 
  theme(legend.position = "bottom") + 
  ggthemes::theme_excel() 
```

![](vis_2_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

New scatterplot

``` r
weather_df |> 
  ggplot(aes(x = date, y = tmax, color = name)) + 
  geom_point() + 
  geom_smooth(se = FALSE) +
  viridis::scale_color_viridis(discrete = TRUE) +
  labs(
    x = "Date",
    y = "Maximum Temp (C)",
    title = "Seasonal variation in Max Temp",
    size = "Precipitation"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](vis_2_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

Extra bonus stuff in `ggplot`

Use different datasets in different `geom`s

``` r
central_park_df =
  weather_df |>
  filter(name == "CentralPark_NY")

molokai_df = 
  weather_df |>
  filter(name == "Molokai_HI")

molokai_df |>
  ggplot(aes(x = date, y = tmax, color = name)) +
  geom_point() +
  geom_line(data = central_park_df)
```

![](vis_2_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

## Multiple panels

``` r
weather_df |>
  ggplot(aes(x = tmax, fill = name)) +
  geom_density() +
  facet_grid(. ~ name)
```

![](vis_2_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

# `patchwork`

``` r
ggp_tmax_tmin = 
  weather_df |>
  ggplot(aes(x = tmax, y = tmin, color = name)) +
  geom_point(alpha = .3)

ggp_tmax_density = 
  weather_df |>
  ggplot(aes(x = tmax, fill = name)) +
  geom_density(alpha = .3)

ggp_tmax_tmin + ggp_tmax_density
```

![](vis_2_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
ggp_tmax_date = 
  weather_df |>
  ggplot(aes(x = date, y = tmax, color = name)) +
  geom_point() +
  geom_smooth(se = FALSE)

(ggp_tmax_tmin + ggp_tmax_density) / ggp_tmax_date
```

![](vis_2_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->

## Data manipulation

``` r
weather_df |>
  ggplot(aes(x = name, y = tmax, fill = name)) +
  geom_violin(alpha = .5)
```

![](vis_2_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
weather_df |> 
  mutate(name = fct_relevel(name, c("Molokai_HI", "CentralPark_NY", "Waterhole_WA"))) |>
  ggplot(aes(x = name, y = tmax, fill = name)) +
  geom_violin(alpha = .5)
```

![](vis_2_files/figure-gfm/unnamed-chunk-17-2.png)<!-- -->

PULSE data next.

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
  ) |>
  mutate(visit = ifelse(visit == "bl", "00m", visit))

pulse_df |>
  ggplot(aes(x = visit, y = bdi_score)) +
  geom_boxplot()
```

![](vis_2_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

FAS data.

``` r
litters_df = 
  read_csv("data/FAS_litters.csv", na = c("NA", ".", "")) |>
  janitor::clean_names() |>
  separate(group, into = c("dose", "tx_day"), sep = 3)

pups_df = 
  read_csv("data/FAS_pups.csv", na = c("NA", ".", "")) |>
  janitor::clean_names() |>
  pivot_longer(
    pd_ears:pd_walk,
    names_to = "outcome",
    values_to = "pn_day",
    names_prefix = "pd_"
  )

fas_df = 
  left_join(pups_df, litters_df, by = "litter_number")

fas_df |> 
  drop_na(tx_day) |>
  ggplot(aes(x = dose, y = pn_day)) + 
  geom_boxplot() + 
  facet_grid(tx_day ~ outcome)
```

![](vis_2_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->
