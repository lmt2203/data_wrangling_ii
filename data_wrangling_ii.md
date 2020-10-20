Data Wrangling II
================
Linh Tran
10/19/2020

  - [Reading Data From The Web](#reading-data-from-the-web)
      - [Extracting tables](#extracting-tables)
      - [CSS Selectors](#css-selectors)
      - [Using an API](#using-an-api)

# Reading Data From The Web

``` r
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.3     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ─────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(rvest)
```

    ## Loading required package: xml2

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     pluck

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
library(httr)
```

## Extracting tables

``` r
url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"
drug_use_html = read_html(url)

drug_use_html
```

    ## {html_document}
    ## <html lang="en">
    ## [1] <head>\n<link rel="P3Pv1" href="http://www.samhsa.gov/w3c/p3p.xml">\n<tit ...
    ## [2] <body>\r\n\r\n<noscript>\r\n<p>Your browser's Javascript is off. Hyperlin ...

``` r
# Extracting the tables from HTML
drug_use_html %>% 
  html_node(css = "table")
```

    ## {html_node}
    ## <table class="rti" border="1" cellspacing="0" cellpadding="1" width="100%" summary="This is a table containing 16 columns.">
    ## [1] <caption>Table 1 – <span class="boldital">Marijuana Use in the Past Year, ...
    ## [2] <thead><tr>\n<th class="left" scope="col">State</th>\r\n<th scope="col">1 ...
    ## [3] <tfoot><tr>\n<td scope="row" colspan="16">NOTE: State and census region e ...
    ## [4] <tbody>\n<tr>\n<th scope="row">Total U.S.</th>\r\n<td width="6%" class="h ...

``` r
# Let's get the contents from the first list element
table_marj =
  drug_use_html %>% 
  html_nodes(css = "table") %>% 
  first() %>% 
  html_table()      

# Remove notes at the bottom of the table and convert to a tibble
table_marj = 
  drug_use_html %>% 
  html_nodes(css = "table") %>% 
  first() %>% 
  html_table() %>% 
  slice(-1) %>% 
  as_tibble()
table_marj
```

    ## # A tibble: 56 x 16
    ##    State `12+(2013-2014)` `12+(2014-2015)` `12+(P Value)` `12-17(2013-201…
    ##    <chr> <chr>            <chr>            <chr>          <chr>           
    ##  1 Tota… 12.90a           13.36            0.002          13.28b          
    ##  2 Nort… 13.88a           14.66            0.005          13.98           
    ##  3 Midw… 12.40b           12.76            0.082          12.45           
    ##  4 South 11.24a           11.64            0.029          12.02           
    ##  5 West  15.27            15.62            0.262          15.53a          
    ##  6 Alab… 9.98             9.60             0.426          9.90            
    ##  7 Alas… 19.60a           21.92            0.010          17.30           
    ##  8 Ariz… 13.69            13.12            0.364          15.12           
    ##  9 Arka… 11.37            11.59            0.678          12.79           
    ## 10 Cali… 14.49            15.25            0.103          15.03           
    ## # … with 46 more rows, and 11 more variables: `12-17(2014-2015)` <chr>,
    ## #   `12-17(P Value)` <chr>, `18-25(2013-2014)` <chr>, `18-25(2014-2015)` <chr>,
    ## #   `18-25(P Value)` <chr>, `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>,
    ## #   `26+(P Value)` <chr>, `18+(2013-2014)` <chr>, `18+(2014-2015)` <chr>,
    ## #   `18+(P Value)` <chr>

## CSS Selectors

``` r
swm_html = 
  read_html("https://www.imdb.com/list/ls070150896/")  #the info isn't stored in a table, so we're going to isolate the CSS selector for elements we care about

# For each element, I'll use the CSS elector in html_nodes() to extract the relevant HTML code, and convert it to text
title_vec =
  swm_html %>% 
  html_nodes(".lister-item-header a") %>% 
  html_text()

gross_rev_vec =
  swm_html %>% 
  html_nodes(".text-small:nth-child(7) span:nth-child(5)") %>% 
  html_text()

runtime_vec = 
  swm_html %>% 
  html_nodes(".runtime") %>% 
  html_text()

swm_df = 
  tibble(
    title = title_vec,
    rev = gross_rev_vec,
    runtime = runtime_vec
  )
```

## Using an API

``` r
# First import dataset for annual water consumption in NYC as csv and parse it
nyc_water = GET("https://data.cityofnewyork.us/resource/ia2d-e54m.csv") %>% 
  content("parsed")
```

    ## Parsed with column specification:
    ## cols(
    ##   year = col_double(),
    ##   new_york_city_population = col_double(),
    ##   nyc_consumption_million_gallons_per_day = col_double(),
    ##   per_capita_gallons_per_person_per_day = col_double()
    ## )

``` r
# Can also import this dataset as a JSON file
nyc_water = 
  GET("https://data.cityofnewyork.us/resource/ia2d-e54m.json") %>% 
  content("text") %>% 
  jsonlite::fromJSON() %>% 
  as_tibble()

# Behavioral Risk Factor Surveillance System dataset
brfss_smart2010 =
  GET("https://chronicdata.cdc.gov/resource/acme-vg9e.csv",
      query = list("$limit" = 5000)) %>% 
  content("parsed")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   year = col_double(),
    ##   sample_size = col_double(),
    ##   data_value = col_double(),
    ##   confidence_limit_low = col_double(),
    ##   confidence_limit_high = col_double(),
    ##   display_order = col_double(),
    ##   locationid = col_logical()
    ## )

    ## See spec(...) for full column specifications.

``` r
# Things could get more complicated
poke = 
  GET("http://pokeapi.co/api/v2/pokemon/1") %>% 
  content()
poke$name
```

    ## [1] "bulbasaur"

``` r
poke$height
```

    ## [1] 7

``` r
poke$abilities
```

    ## [[1]]
    ## [[1]]$ability
    ## [[1]]$ability$name
    ## [1] "overgrow"
    ## 
    ## [[1]]$ability$url
    ## [1] "https://pokeapi.co/api/v2/ability/65/"
    ## 
    ## 
    ## [[1]]$is_hidden
    ## [1] FALSE
    ## 
    ## [[1]]$slot
    ## [1] 1
    ## 
    ## 
    ## [[2]]
    ## [[2]]$ability
    ## [[2]]$ability$name
    ## [1] "chlorophyll"
    ## 
    ## [[2]]$ability$url
    ## [1] "https://pokeapi.co/api/v2/ability/34/"
    ## 
    ## 
    ## [[2]]$is_hidden
    ## [1] TRUE
    ## 
    ## [[2]]$slot
    ## [1] 3

``` r
# It wouldn't be terrible to just download the CSV, document where it came from carefully, and move on. APIs are more helpful when the full dataset is complex and you only need pieces, or when the data are updated regularly
```
