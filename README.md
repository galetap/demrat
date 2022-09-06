
# demrat

Provides a framework for estimating demographic rates (growth, crude
birth, and total fertility rate) of a population represented by a
cemetery sample using age-at-death ratios.

The methodology accounts for the effect of stochastic variation in small
skeletal samples.

## Installation and loading

-   Install the latest developmental version from
    [GitHub](https://github.com/galetap/demrat) as follow:

``` r
if(!require(devtools)) install.packages("devtools")
devtools::install_github("galetap/demrat")
```

-   Loading package

``` r
library(demrat)  
```

## Data

The package contains two model datasets `BA` and `BAraw`. Both datasets
are adopted from Bocquet-Appel (2002), Table 1
(<https://doi.org/10.1086/342429>).

**BA**

Summary data for 68 European Mesolithic and Neolithic sites. Each row
contains information about a single site.

``` r
BA
```

    ## # A tibble: 68 × 12
    ##    Site         Front   C14    dt  D0_4 D5_19   D20_ Total D5_19…¹   D5_ D5_D20_
    ##    <chr>        <int> <int> <int> <dbl> <dbl>  <dbl> <dbl>   <dbl> <dbl>   <dbl>
    ##  1 Aisne series  5000  4900   100 10    15     25       50   0.375  40      1.6 
    ##  2 Aiterhofen …  5400  5300   100  5.4  21.4  115.     142   0.157 137.     1.19
    ##  3 Ajdovska Ja…  5500  4394  1106  6     8     11       25   0.421  19      1.73
    ##  4 Aven de la …  5500  3176  2324  3.2   7.8   49       60   0.137  56.8    1.16
    ##  5 Bade-Wutemb…  5400  5250   150  7.2  12.8   11       31   0.538  23.8    2.16
    ##  6 Baume Bourb…  5500  4700   800  1.75  3.91   9.33    15   0.295  13.2    1.42
    ##  7 Belleville    4900  2548  2352  9.8  35.6   95.6    141   0.272 131.     1.37
    ##  8 Breuil-en-V…  4900  2700  2200 28    20     40       88   0.333  60      1.5 
    ##  9 Brochtorff …  5900  3900  2000  2    15     55       72   0.214  70      1.27
    ## 10 Bruchstedt    5400  5250   150  8.2  18.8   34       61   0.356  52.8    1.55
    ## # … with 58 more rows, 1 more variable: Culture <chr>, and abbreviated variable
    ## #   name ¹​D5_19_D5_

``` r
# For column description, run help(BA).
```

**BAraw**

Estimation of age-at-death for 5,115 skeletons from 68 archaeological
sites presented in `BA`).

``` r
BAraw
```

    ## # A tibble: 5,115 × 4
    ##    Site         Culture   Age_min Age_max
    ##    <chr>        <fct>       <dbl>   <dbl>
    ##  1 Aisne series Neolithic       5      20
    ##  2 Aisne series Neolithic       0       5
    ##  3 Aisne series Neolithic      20     100
    ##  4 Aisne series Neolithic       0       5
    ##  5 Aisne series Neolithic      20     100
    ##  6 Aisne series Neolithic      20     100
    ##  7 Aisne series Neolithic      20     100
    ##  8 Aisne series Neolithic       5      20
    ##  9 Aisne series Neolithic       5      20
    ## 10 Aisne series Neolithic      20     100
    ## # … with 5,105 more rows

``` r
# For column description, run help(BAraw).
```

## Key functions

#### `dr`: Calculate summary information from raw age-at-death data

Calculate number of skeletons in several age-at-death groups (e.g., D1+,
D20+) and six demographic ratios (D1+/D20+, D3+/D20+, D5+/D20+,
D5-14/D20+ aka juvenility index, D5-19/D5+ aka P index, and D0-14/D0+)
by Site and Culture.

``` r
dr(BAraw)
```

    ## # A tibble: 68 × 18
    ##    Site      Culture     n    D0   D1_   D3_   D5_  D15_ D0_14 D5_14 D5_19  D20_
    ##    <chr>     <fct>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ##  1 Aisne se… Neolit…    50   2    48    44      40  30   20    10       15    25
    ##  2 Aiterhof… Neolit…   141   1   140   138     136 122   19    14       21   115
    ##  3 Ajdovska… Neolit…    25   1.2  23.8  21.4    19  13.7 11.3   5.33     8    11
    ##  4 Aven de … Neolit…    60   0.6  59.4  58.2    57  51.7  8.33  5.33     8    49
    ##  5 Bade-Wut… Neolit…    31   1.4  29.6  26.8    24  15.3 15.7   8.67    13    11
    ##  6 Baume Bo… Neolit…    15   0.4  14.6  13.8    13  10.3  4.67  2.67     4     9
    ##  7 Bellevil… Neolit…   142   2   140   136     132 108   34    24       36    96
    ##  8 Breuil-e… Neolit…    88   5.6  82.4  71.2    60  46.7 41.3  13.3     20    40
    ##  9 Brochtor… Neolit…    72   0.4  71.6  70.8    70  60   12    10       15    55
    ## 10 Bruchste… Neolit…    61   1.6  59.4  56.2    53  40.3 20.7  12.7     19    34
    ## # … with 58 more rows, and 6 more variables: D1_D20_ <dbl>, D3_D20_ <dbl>,
    ## #   D5_D20_ <dbl>, JI <dbl>, P <dbl>, D0_14_D0_ <dbl>

#### `diest`: Estimation of demographoc rates for cemetery sample(s)

`diest` function estimates growth, crude birth, and total fertility
rates based on age-at-death ratios (e.g., D5+/D20+) by Site and Culture.

By default, the prediction model is based on the reference set of 100
simulated reference skeletal samples with the same number of adult
skeletons (D20+) as the real skeletal sample under study.

Estimation for sites with a large number of skeletons and/or estimation
for many sites can be slow.

``` r
# Estimation from raw age-at-death data
BAraw %>%
  dr() %>% 
  slice(12, 24) %>%
  diest()
```

    ## # A tibble: 22 × 7
    ##    Site         Culture   DV     IV         Est    Lwr   Upr
    ##    <chr>        <fct>     <chr>  <chr>    <dbl>  <dbl> <dbl>
    ##  1 Cala Colombo Neolithic TFR    D5_D20_  7.65   3.72  15.7 
    ##  2 Cala Colombo Neolithic TFR    D3_D20_  8.33   4.34  16.0 
    ##  3 Cala Colombo Neolithic TFR    D1_D20_  5.67   3.48   9.24
    ##  4 Cala Colombo Neolithic CBR    D5_D20_ 61.8   28.0   95.6 
    ##  5 Cala Colombo Neolithic CBR    D3_D20_ 64.5   33.3   95.6 
    ##  6 Cala Colombo Neolithic CBR    D1_D20_ 47.1   23.5   70.7 
    ##  7 Cala Colombo Neolithic Growth D5_D20_  1.50  -1.10   4.10
    ##  8 Cala Colombo Neolithic Growth D3_D20_  1.72  -0.638  4.08
    ##  9 Cala Colombo Neolithic Growth D1_D20_  0.403 -1.45   2.25
    ## 10 Cala Colombo Neolithic CBR    P       56.2   NA     NA   
    ## # … with 12 more rows

``` r
# Estimation from summary data 
BA %>%
  slice(12, 24) %>%
  diest()
```

    ## # A tibble: 10 × 7
    ##    Site         Culture    DV     IV          Est    Lwr    Upr
    ##    <chr>        <chr>      <chr>  <chr>     <dbl>  <dbl>  <dbl>
    ##  1 Cala Colombo Neolithic  TFR    D5_D20_  8.07    3.93  16.6  
    ##  2 Cala Colombo Neolithic  CBR    D5_D20_ 64.1    30.3   97.9  
    ##  3 Cala Colombo Neolithic  Growth D5_D20_  1.68   -0.917  4.28 
    ##  4 Cala Colombo Neolithic  CBR    P       58.2    NA     NA    
    ##  5 Cala Colombo Neolithic  Growth P        1.81   NA     NA    
    ##  6 Djerdap      Mesolithic TFR    D5_D20_  4.50    3.38   6.01 
    ##  7 Djerdap      Mesolithic CBR    D5_D20_ 35.4    18.3   52.5  
    ##  8 Djerdap      Mesolithic Growth D5_D20_ -0.310  -1.36   0.740
    ##  9 Djerdap      Mesolithic CBR    P       34.4    NA     NA    
    ## 10 Djerdap      Mesolithic Growth P       -0.0944 NA     NA

A mortality regime of a population from which reference skeletal samples
are drawn can be set by the user (run `help(diest)`).

``` r
BA %>%
  slice(12) %>%
  diest(samples = 200, e0_min = 25, e0_max = 30)
```

    ## # A tibble: 5 × 7
    ##   Site         Culture   DV     IV        Est    Lwr   Upr
    ##   <chr>        <chr>     <chr>  <chr>   <dbl>  <dbl> <dbl>
    ## 1 Cala Colombo Neolithic TFR    D5_D20_  8.42  3.77  18.8 
    ## 2 Cala Colombo Neolithic CBR    D5_D20_ 63.4  31.2   95.7 
    ## 3 Cala Colombo Neolithic Growth D5_D20_  2.11 -0.722  4.95
    ## 4 Cala Colombo Neolithic CBR    P       58.2  NA     NA   
    ## 5 Cala Colombo Neolithic Growth P        1.81 NA     NA

#### `simdr`: Create a reference set of simulated skeletal samples

`simdr_CD` is a helper function that creates a reference set of skeletal
samples drawn from Coale and Demeny (1983) set of model life tables. For
parameters description, run `help(simdr_CD)`.

``` r
# Simulation based on the default settings 
simdr_CD() %>% 
  as_tibble()
```

    ## # A tibble: 100 × 30
    ##    SSS   Samples D20_raw e0_min e0_max Growth…¹ Growt…² Alpha Beta  Growth l27.5
    ##    <lgl>   <dbl>   <dbl>  <dbl>  <dbl>    <dbl>   <dbl> <lgl> <lgl>  <dbl> <dbl>
    ##  1 TRUE      100      50     20     30       -4       8 NA    NA     4.32  0.339
    ##  2 TRUE      100      50     20     30       -4       8 NA    NA     5.05  0.355
    ##  3 TRUE      100      50     20     30       -4       8 NA    NA    -3.94  0.460
    ##  4 TRUE      100      50     20     30       -4       8 NA    NA     5.44  0.416
    ##  5 TRUE      100      50     20     30       -4       8 NA    NA    -2.28  0.474
    ##  6 TRUE      100      50     20     30       -4       8 NA    NA     5.00  0.416
    ##  7 TRUE      100      50     20     30       -4       8 NA    NA     0.505 0.431
    ##  8 TRUE      100      50     20     30       -4       8 NA    NA     2.61  0.460
    ##  9 TRUE      100      50     20     30       -4       8 NA    NA    -2.03  0.446
    ## 10 TRUE      100      50     20     30       -4       8 NA    NA     5.88  0.446
    ## # … with 90 more rows, 19 more variables: e0 <dbl>, CBR <dbl>, TFR <dbl>,
    ## #   n <int>, D0 <int>, D1_ <int>, D3_ <int>, D5_ <int>, D15_ <int>, D20_ <int>,
    ## #   D0_14 <int>, D5_14 <int>, D5_19 <int>, D1_D20_ <dbl>, D3_D20_ <dbl>,
    ## #   D5_D20_ <dbl>, JI <dbl>, P <dbl>, D0_14_D0_ <dbl>, and abbreviated variable
    ## #   names ¹​Growth_min, ²​Growth_max

``` r
# Simulation based on the user-defined settings
simdr_CD(samples = 200, D20_raw = 20, e0_min = 30, e0_max = 40) %>% 
  as_tibble()
```

    ## # A tibble: 200 × 30
    ##    SSS   Samples D20_raw e0_min e0_max Growth…¹ Growt…² Alpha Beta  Growth l27.5
    ##    <lgl>   <dbl>   <dbl>  <dbl>  <dbl>    <dbl>   <dbl> <lgl> <lgl>  <dbl> <dbl>
    ##  1 TRUE      200      20     30     40       -4       8 NA    NA     4.32  0.489
    ##  2 TRUE      200      20     30     40       -4       8 NA    NA    -3.88  0.531
    ##  3 TRUE      200      20     30     40       -4       8 NA    NA    -1.29  0.474
    ##  4 TRUE      200      20     30     40       -4       8 NA    NA     1.89  0.597
    ##  5 TRUE      200      20     30     40       -4       8 NA    NA     0.253 0.489
    ##  6 TRUE      200      20     30     40       -4       8 NA    NA    -1.85  0.571
    ##  7 TRUE      200      20     30     40       -4       8 NA    NA     0.919 0.474
    ##  8 TRUE      200      20     30     40       -4       8 NA    NA    -2.74  0.610
    ##  9 TRUE      200      20     30     40       -4       8 NA    NA    -3.82  0.558
    ## 10 TRUE      200      20     30     40       -4       8 NA    NA    -1.27  0.558
    ## # … with 190 more rows, 19 more variables: e0 <dbl>, CBR <dbl>, TFR <dbl>,
    ## #   n <int>, D0 <int>, D1_ <int>, D3_ <int>, D5_ <int>, D15_ <int>, D20_ <int>,
    ## #   D0_14 <int>, D5_14 <int>, D5_19 <int>, D1_D20_ <dbl>, D3_D20_ <dbl>,
    ## #   D5_D20_ <dbl>, JI <dbl>, P <dbl>, D0_14_D0_ <dbl>, and abbreviated variable
    ## #   names ¹​Growth_min, ²​Growth_max
