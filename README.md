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

### MEDSL: Senate returns by states (1976-)

### MEDSL: House returns by congressional district (1976-)

[Code](https://github.com/jaytimm/uspols/blob/master/medsl.md)

### DailyKos: Presidential returns by congressional district (2008-)

[Code](https://github.com/jaytimm/uspols/blob/master/daily-kos.md)

``` r
uspols::uspols_dk_pres %>%
  head() %>% knitr::kable()
```

| state\_abbrev | district\_code | year | winner | democrat | republican |
|:--------------|:---------------|:-----|:-------|:---------|:-----------|
| AK            | 00             | 2008 | McCain | 38.1     | 59.7       |
| AK            | 00             | 2012 | Romney | 41.2     | 55.3       |
| AK            | 00             | 2016 | Trump  | 37.6     | 52.8       |
| AL            | 01             | 2008 | McCain | 38.5     | 60.9       |
| AL            | 01             | 2012 | Romney | 37.4     | 61.8       |
| AL            | 01             | 2016 | Trump  | 34.1     | 63.5       |

### Wikipedia: Presidential returns by state (1864-)

[Code](https://github.com/jaytimm/uspols/blob/master/wikipedia.md)

``` r
uspols::uspols_wiki_pres %>%
  head()
## # A tibble: 6 x 8
##   GEOID state_abbrev state        year candidate        vote_share party  winner
##   <chr> <chr>        <chr>       <int> <chr>                 <dbl> <chr>   <int>
## 1 45    SC           South Caro…  1936 Franklin D. Roo…       98.6 Democ…      1
## 2 45    SC           South Caro…  1932 Franklin D. Roo…       98.0 Democ…      1
## 3 28    MS           Mississippi  1936 Franklin D. Roo…       97.1 Democ…      1
## 4 45    SC           South Caro…  1916 Woodrow Wilson         96.7 Democ…      1
## 5 45    SC           South Caro…  1924 John W. Davis          96.6 Democ…      1
## 6 45    SC           South Caro…  1920 James M. Cox           96.0 Democ…      1
```

### Twitter: Lawmaker handles

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

### VoteView: Congressional details
