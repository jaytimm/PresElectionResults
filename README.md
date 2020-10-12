uspols
======

A simple and transparently constructed/aggregated collection of American
political data, including federal election returns and Twitter details
about American lawmakers.

Election returns:

-   for congressional races for both chambers (from 1976 onward) made
    available via [MEDSL](). We have ever so slightly massaged these
    data for …. House results by congressional districts; Senate returns
    by state.

-   Presidential returns by congressional district for 2008, 2012, and
    2016 elections via [The DailyKos]().

-   Presidential returns by state for elections since 1864 scraped from
    Wikipedia.

With the least … , we have made uniform … within and across data sets.

The formats presented here are the ones I like t … but code-links get
back to iriginal data sources, so users can tweak things to their
liking. I am not familiar with the conventions (if any exist) of sharing
election return data. ??

Installation
------------

``` r
library(tidyverse)
```

``` r
library(devtools)
devtools::install_github("jaytimm/uspols")
library(uspols) 
```

Details
-------

### § MEDSL: Senate returns by states (1976-)

### § MEDSL: House returns by congressional district (1976-)

[Code](https://github.com/jaytimm/uspols/blob/master/medsl.md)

``` r
uspols::uspols_medsl_house %>%
  head() %>% knitr::kable()
```

|  year| state\_abbrev | district\_code | winner                      |  democrat|  republican|  other|
|-----:|:--------------|:---------------|:----------------------------|---------:|-----------:|------:|
|  1976| AL            | 01             | Jack Edwards                |     37.48|       62.52|   0.00|
|  1976| AL            | 02             | William L. “Bill” Dickinson |     42.39|       57.60|   0.00|
|  1976| AL            | 03             | Bill Nichols                |     98.97|        0.00|   1.03|
|  1976| AL            | 04             | Tom Bevill                  |     80.38|       19.62|   0.00|
|  1976| AL            | 05             | Ronnie G. Flippo            |     99.99|        0.00|   0.00|
|  1976| AL            | 06             | John H. Buchanan, Jr.       |     42.69|       56.68|   0.63|

### § DailyKos: Presidential returns by congressional district (2008-)

[Code](https://github.com/jaytimm/uspols/blob/master/daily-kos.md)

``` r
uspols::uspols_dk_pres %>%
  head() %>% knitr::kable()
```

|  year| state\_abbrev | district\_code | winner       | democrat | republican |
|-----:|:--------------|:---------------|:-------------|:---------|:-----------|
|  2008| AK            | 00             | John McCain  | 38.1     | 59.7       |
|  2012| AK            | 00             | Mitt Romney  | 41.2     | 55.3       |
|  2016| AK            | 00             | Donald Trump | 37.6     | 52.8       |
|  2008| AL            | 01             | John McCain  | 38.5     | 60.9       |
|  2012| AL            | 01             | Mitt Romney  | 37.4     | 61.8       |
|  2016| AL            | 01             | Donald Trump | 34.1     | 63.5       |

### § Wikipedia: Presidential returns by state (1864-)

[Code](https://github.com/jaytimm/uspols/blob/master/wikipedia.md) \|
Thoughts, et.

``` r
uspols::uspols_wiki_pres %>%
  head() %>% knitr::kable()
```

|  year| state\_abbrev | winner              |  democrat|  republican|
|-----:|:--------------|:--------------------|---------:|-----------:|
|  1864| CA            | Abraham Lincoln     |     41.40|       58.60|
|  1864| CT            | Abraham Lincoln     |     48.60|       51.40|
|  1864| DE            | George B. McClellan |     51.80|       48.20|
|  1864| IA            | Abraham Lincoln     |     36.92|       63.08|
|  1864| IL            | Abraham Lincoln     |     45.60|       54.40|
|  1864| IN            | Abraham Lincoln     |     46.50|       53.50|

### § Lawmaker Twitter handles: Congresses 115 & 116

[Code](https://github.com/jaytimm/twitter-and-us-lawmakers/blob/master/twitter-handles.md)

``` r
uspols::uspols_twitter_handles %>% head()
## # A tibble: 6 x 12
##   congress chamber screen_name bioguide_id member account_type handle_type
##      <int> <chr>   <chr>       <chr>       <chr>  <chr>        <chr>      
## 1      115 House   KYCOMER     C001108     James… campaign     prev_names 
## 2      115 House   REPJACKYRO… R000608     Jacky… office       prev_names 
## 3      115 House   REPESPAILL… E000297     Adria… office       screen_name
## 4      115 House   REPTREY     H001074     Trey … office       screen_name
## 5      115 House   REPDWIGHTE… E000296     Dwigh… office       screen_name
## 6      115 House   ROGERMARSH… M001198     Roger… campaign     screen_name
## # … with 5 more variables: state_abbrev <chr>, district_code <int>,
## #   bioname <chr>, party_name <chr>, born <int>
```

### § VoteView: Congressional details
