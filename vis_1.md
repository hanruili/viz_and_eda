viz_and_eda
================
Hanrui Li
2024-09-26

Import the weather data.

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
```

    ## using cached file: /Users/helena/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2024-09-26 10:17:38.084403 (8.651)

    ## file min/max dates: 1869-01-01 / 2024-09-30

    ## using cached file: /Users/helena/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USW00022534.dly

    ## date created (size, mb): 2024-09-26 10:17:54.972764 (3.932)

    ## file min/max dates: 1949-10-01 / 2024-09-30

    ## using cached file: /Users/helena/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2024-09-26 10:18:00.00449 (1.036)

    ## file min/max dates: 1999-09-01 / 2024-09-30

Making first plot.

``` r
ggplot(weather_df, aes(x = tmin, y = tmax)) + 
  geom_point()
```

    ## Warning: Removed 17 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](vis_1_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
weather_df |>
  ggplot(aes(x = tmin, y = tmax)) +
  geom_point()
```

    ## Warning: Removed 17 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](vis_1_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
ggp_weather_scatterplot = 
  weather_df |>
  ggplot(aes(x = tmin, y = tmax)) +
  geom_point()

ggp_weather_scatterplot
```

    ## Warning: Removed 17 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](vis_1_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

More scatterplots.

``` r
weather_df |>
  ggplot(aes(x = tmin, y = tmax, colour = name)) +
  geom_point()
```

    ## Warning: Removed 17 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](vis_1_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
weather_df |>
  ggplot(aes(x = tmin, y = tmax, colour = name)) +
  geom_point(alpha = 0.3, size = 0.8) + 
  geom_smooth(se = FALSE)
```

    ## Warning: Removed 17 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 17 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](vis_1_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Where you define aesthetics can matter.

``` r
weather_df |>
  ggplot(aes(x = tmin, y = tmax)) +
  geom_point(aes(color = name), alpha = 0.3, size = 0.8) + 
  geom_smooth(se = FALSE)
```

    ## Warning: Removed 17 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 17 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](vis_1_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
# Here geom_smooth() cannot get color = name
```

Use facetinng real quick

``` r
weather_df |>
  ggplot(aes(x = tmin, y = tmax, colour = name)) +
  geom_point(alpha = 0.3, size = 0.8) + 
  geom_smooth(se = FALSE) +
  facet_grid(. ~ name)
```

    ## Warning: Removed 17 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 17 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](vis_1_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
# facet_grid to separate, easier to read

weather_df |>
  ggplot(aes(x = tmin, y = tmax, colour = name)) +
  geom_point(alpha = 0.3, size = 0.8) + 
  geom_smooth(se = FALSE) +
  facet_grid(name ~ .)
```

    ## Warning: Removed 17 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).
    ## Removed 17 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](vis_1_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

``` r
# more difficult to read
```

Make a more interesting scatterplot

``` r
weather_df |>
  ggplot(aes(x = date, y = tmax, colour = name)) +
  geom_point()
```

    ## Warning: Removed 17 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](vis_1_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
weather_df |>
  ggplot(aes(x = date, y = tmax, colour = name, size = prcp)) +
  geom_point(alpha = 0.3) +
  geom_smooth(se = FALSE) +
  facet_grid(. ~ name)
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## Warning: Removed 17 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: The following aesthetics were dropped during statistical transformation: size.
    ## ℹ This can happen when ggplot fails to infer the correct grouping structure in
    ##   the data.
    ## ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
    ##   variable into a factor?
    ## The following aesthetics were dropped during statistical transformation: size.
    ## ℹ This can happen when ggplot fails to infer the correct grouping structure in
    ##   the data.
    ## ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
    ##   variable into a factor?
    ## The following aesthetics were dropped during statistical transformation: size.
    ## ℹ This can happen when ggplot fails to infer the correct grouping structure in
    ##   the data.
    ## ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
    ##   variable into a factor?

    ## Warning: Removed 19 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](vis_1_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

Learning assessment

``` r
weather_df |>
  filter(name == "CentralPark_NY") |>
  mutate(
    tmax_fahr = tmax * (9/5) + 32,
    tmin_fahr = tmin * (9/5) + 32
  ) |>
  ggplot(aes(x = tmin_fahr, y = tmax_fahr)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](vis_1_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

## Small tings

``` r
weather_df |>
  ggplot(aes(x = tmin, y = tmax)) +
  geom_point(aes(color = name), alpha = 0.3, size.= 0.6) +
  geom_smooth(se = FALSE)
```

    ## Warning in geom_point(aes(color = name), alpha = 0.3, size. = 0.6): Ignoring
    ## unknown parameters: `size.`

    ## `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'

    ## Warning: Removed 17 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 17 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](vis_1_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
weather_df |>
  ggplot(aes(x = tmin, y = tmax)) +
  geom_hex()
```

    ## Warning: Removed 17 rows containing non-finite outside the scale range
    ## (`stat_binhex()`).

    ## Warning: Computation failed in `stat_binhex()`.
    ## Caused by error in `compute_group()`:
    ## ! The package "hexbin" is required for `stat_bin_hex()`.

![](vis_1_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
weather_df |>
  ggplot(aes(x = tmin, y = tmax)) + 
  geom_point(colour = "lightblue")
```

    ## Warning: Removed 17 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](vis_1_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

## Univariate plots

``` r
weather_df |>
  ggplot(aes(x = tmin)) +
  geom_histogram()
```

    ## Warning: Removed 17 rows containing non-finite outside the scale range
    ## (`stat_bin()`).

![](vis_1_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
weather_df |>
  ggplot(aes(x = tmin, fill = name)) +
  geom_histogram(position = "dodge")
```

    ## Warning: Removed 17 rows containing non-finite outside the scale range
    ## (`stat_bin()`).

![](vis_1_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

how would I fix this? maybe facet?

``` r
weather_df |>
  ggplot(aes(x = tmin, fill = name)) + 
  geom_histogram() + 
  facet_grid(. ~ name)
```

    ## Warning: Removed 17 rows containing non-finite outside the scale range
    ## (`stat_bin()`).

![](vis_1_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

maybe a density plot?

``` r
weather_df |>
  ggplot(aes(x = tmin, fill = name)) + 
  geom_density(alpha = 0.3)
```

    ## Warning: Removed 17 rows containing non-finite outside the scale range
    ## (`stat_density()`).

![](vis_1_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
weather_df |>
  ggplot(aes(x = tmin, y = tmax, fill = name)) + 
  geom_boxplot()
```

    ## Warning: Removed 17 rows containing missing values or values outside the scale range
    ## (`stat_boxplot()`).

![](vis_1_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

## Violin plots

``` r
weather_df |>
  ggplot(aes(x = tmin, y = tmax, fill = name)) + 
  geom_violin()
```

    ## Warning: Removed 17 rows containing non-finite outside the scale range
    ## (`stat_ydensity()`).

    ## Warning: `position_dodge()` requires non-overlapping x intervals.

![](vis_1_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

## Ridge plot

``` r
weather_df |>
  ggplot(aes(x = tmin, y = name)) +
  geom_density_ridges(colour = "blue")
```

    ## Warning: Removed 17 rows containing non-finite outside the scale range
    ## (`stat_density_ridges()`).

![](vis_1_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

Learning assessment

``` r
weather_df |>
  ggplot(aes(x = prcp, fill = name)) +
  geom_density(alpha = 0.3)
```

    ## Warning: Removed 15 rows containing non-finite outside the scale range
    ## (`stat_density()`).

![](vis_1_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
weather_df |>
  ggplot(aes(x = name, y = prcp)) +
  geom_boxplot()
```

    ## Warning: Removed 15 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

![](vis_1_files/figure-gfm/unnamed-chunk-22-2.png)<!-- -->

``` r
weather_df |>
  filter(prcp > 10, prcp < 1000) |>
  ggplot(aes(x = prcp, fill = name)) +
  geom_density(alpha = 0.3)
```

![](vis_1_files/figure-gfm/unnamed-chunk-22-3.png)<!-- -->

## Saving and embedding plots

``` r
ggp_weather = 
  weather_df |>
  ggplot(aes(x = date, y = tmax, colour = name)) +
  geom_point()

ggsave("ggp_weather.pdf", ggp_weather, width = 8, height = 6)
```

    ## Warning: Removed 17 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

embedding plots

``` r
weather_df |>
  ggplot(aes(x = date, y = tmax, colour = name)) +
  geom_point()
```

    ## Warning: Removed 17 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](vis_1_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

``` r
weather_df |>
  ggplot(aes(x = date, y = tmax, colour = name)) +
  geom_point()
```

    ## Warning: Removed 17 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](vis_1_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->
