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

    ##  [1] -0.86178957  0.39510660  0.35392994  1.32111937 -0.52841145  0.66446295
    ##  [7]  0.18827613  0.59197879  0.39800539 -0.59111255  1.10817012 -0.44501248
    ## [13] -2.08778016  1.18026475  0.81099636  1.28930380  0.57246068  0.95536707
    ## [19] -1.34347408 -0.09781882 -1.18150552 -1.70673078 -1.50089126  0.68710956
    ## [25] -0.17202484

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

    ##  [1] -0.86178957  0.39510660  0.35392994  1.32111937 -0.52841145  0.66446295
    ##  [7]  0.18827613  0.59197879  0.39800539 -0.59111255  1.10817012 -0.44501248
    ## [13] -2.08778016  1.18026475  0.81099636  1.28930380  0.57246068  0.95536707
    ## [19] -1.34347408 -0.09781882 -1.18150552 -1.70673078 -1.50089126  0.68710956
    ## [25] -0.17202484

does this always work?

``` r
z_scores(x = 3)
```

    ## Error in z_scores(x = 3): you need at least five numbers to compute z score

``` r
z_scores(x = c("my", "name", "is", "mia"))
```

    ## Error in z_scores(x = c("my", "name", "is", "mia")): x needs to be numeric
