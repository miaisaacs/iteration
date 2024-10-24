Writing Functions
================
Mia Isaacs
2024-10-24

# load libraries

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

# write my first function!

as an example, here’s a z-score computation

``` r
x_vec = rnorm(n = 25, mean = 10, sd = 3.5)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.54255410  0.41878052 -2.06772257 -0.70197943 -2.26769123 -0.60001775
    ##  [7] -0.28572817  0.39121342 -0.04227709  0.37595524  0.39612455  0.92339722
    ## [13]  0.37392432 -0.78542391  0.95527687  1.17400766  2.09123917  0.42912985
    ## [19]  0.34410869  0.43150712 -1.53086903  0.86742416 -0.12989934 -0.67097758
    ## [25] -0.63205680

now i’ll write a function to do this

``` r
z_scores = function(x) {
  
  if(!is.numeric(x)) {
    stop("x needs to be numeric")
  }
  
  if (length(x) < 5) {
    stop("you need at least five numbers to compute z score")
  }
  
  z = (x-mean(x)) / sd(x)
  
  return(z)
  
  }

z_scores(x = x_vec)
```

    ##  [1]  0.54255410  0.41878052 -2.06772257 -0.70197943 -2.26769123 -0.60001775
    ##  [7] -0.28572817  0.39121342 -0.04227709  0.37595524  0.39612455  0.92339722
    ## [13]  0.37392432 -0.78542391  0.95527687  1.17400766  2.09123917  0.42912985
    ## [19]  0.34410869  0.43150712 -1.53086903  0.86742416 -0.12989934 -0.67097758
    ## [25] -0.63205680

does this always work?

``` r
z_scores(x = 3)
```

    ## Error in z_scores(x = 3): you need at least five numbers to compute z score

``` r
z_scores(x = c("my", "name", "is", "mia"))
```

    ## Error in z_scores(x = c("my", "name", "is", "mia")): x needs to be numeric

# a new function!

``` r
mean_and_sd = function(x) {

  mean_x = mean(x)
  sd_x = sd(x)
  
  out_df =
    tibble(
      mean = mean_x,
      sd = sd_x
    )
  return(out_df)
  
  }

mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0  3.77

# check stuff using a simulation

``` r
sim_df =
  tibble(
    x = rnorm(30, 10, 5)
  )

sim_df |> 
  summarize(
    mean = mean(x),
    sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean `sd(x)`
    ##   <dbl>   <dbl>
    ## 1  11.4    4.73

simulation function to check sample mean and sd

``` r
sim_mean_sd = function(samp_size, true_mean, true_sd) {
  
  sim_df =
  tibble(
    x = rnorm(samp_size, true_mean, true_sd)
  )

out_df =
  sim_df |> 
  summarize(
    mean = mean(x),
    sd(x)
  )
  
return(out_df)
  
}

sim_mean_sd(samp_size = 3000, true_mean = 4, true_sd = .12)
```

    ## # A tibble: 1 × 2
    ##    mean `sd(x)`
    ##   <dbl>   <dbl>
    ## 1  4.00   0.120

# revisit LoTR words

``` r
fellowship_ring = readxl::read_excel("./data/LotR_Words.xlsx", range = "B3:D6") |>
  mutate(movie = "fellowship_ring")

two_towers = readxl::read_excel("./data/LotR_Words.xlsx", range = "F3:H6") |>
  mutate(movie = "two_towers")

return_king = readxl::read_excel("./data/LotR_Words.xlsx", range = "J3:L6") |>
  mutate(movie = "return_king")

lotr_tidy = bind_rows(fellowship_ring, two_towers, return_king) |>
  janitor::clean_names() |>
  pivot_longer(
    female:male,
    names_to = "sex",
    values_to = "words") |> 
  mutate(race = str_to_lower(race)) |> 
  select(movie, everything()) 
```

write a data cleaning function

``` r
lotr_load_and_tidy = function(path, range, movie_name) {
  
  df = 
    readxl::read_excel(path, range = range) |>
    janitor::clean_names() |>
    pivot_longer(
      female:male,
      names_to = "sex",
      values_to = "words") |>
    mutate(
      race = str_to_lower(race),
      movie = movie_name) |> 
    select(movie, everything())
  
  df
  
}

lotr_tidy = 
  bind_rows(
    lotr_load_and_tidy("data/LotR_Words.xlsx", "B3:D6", "fellowship_ring"),
    lotr_load_and_tidy("data/LotR_Words.xlsx", "F3:H6", "two_towers"),
    lotr_load_and_tidy("data/LotR_Words.xlsx", "J3:L6", "return_king"))
```

# NSDUH

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)

data_marj = 
  nsduh_html |> 
  html_table() |> 
  nth(1) |>
  slice(-1) |> 
  select(-contains("P Value")) |>
  pivot_longer(
    -State,
    names_to = "age_year", 
    values_to = "percent") |>
  separate(age_year, into = c("age", "year"), sep = "\\(") |>
  mutate(
    year = str_replace(year, "\\)", ""),
    percent = str_replace(percent, "[a-c]$", ""),
    percent = as.numeric(percent)) |>
  filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
```

``` r
nsduh_table <- function(html, table_num, table_name) {
  
  table = 
    html |> 
    html_table() |> 
    nth(table_num) |>
    slice(-1) |> 
    select(-contains("P Value")) |>
    pivot_longer(
      -State,
      names_to = "age_year", 
      values_to = "percent") |>
    separate(age_year, into = c("age", "year"), sep = "\\(") |>
    mutate(
      year = str_replace(year, "\\)", ""),
      percent = str_replace(percent, "[a-c]$", ""),
      percent = as.numeric(percent),
      name = table_name) |>
    filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
  
  table
  
}
```
