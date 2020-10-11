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

### § MEDSL: Senate returns by states (1976-)

### § MEDSL: House returns by congressional district (1976-)

[Code](https://github.com/jaytimm/uspols/blob/master/medsl.md)

``` r
uspols::uspols_medsl_house %>%
  head() %>%
  select(year, GEOID, candidate, democrat:republican) %>%
  knitr::kable()
```

<table>
<colgroup>
<col style="width: 4%" />
<col style="width: 4%" />
<col style="width: 5%" />
<col style="width: 5%" />
<col style="width: 4%" />
<col style="width: 4%" />
<col style="width: 4%" />
<col style="width: 3%" />
<col style="width: 4%" />
<col style="width: 4%" />
<col style="width: 3%" />
<col style="width: 5%" />
<col style="width: 5%" />
<col style="width: 4%" />
<col style="width: 2%" />
<col style="width: 3%" />
<col style="width: 14%" />
<col style="width: 4%" />
<col style="width: 3%" />
<col style="width: 5%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">state</th>
<th style="text-align: left;">state_po</th>
<th style="text-align: right;">state_fips</th>
<th style="text-align: right;">state_cen</th>
<th style="text-align: right;">state_ic</th>
<th style="text-align: left;">office</th>
<th style="text-align: right;">district</th>
<th style="text-align: left;">stage</th>
<th style="text-align: left;">special</th>
<th style="text-align: left;">writein</th>
<th style="text-align: left;">mode</th>
<th style="text-align: right;">totalvotes</th>
<th style="text-align: left;">unofficial</th>
<th style="text-align: right;">version</th>
<th style="text-align: right;">year</th>
<th style="text-align: left;">GEOID</th>
<th style="text-align: left;">candidate</th>
<th style="text-align: right;">democrat</th>
<th style="text-align: right;">other</th>
<th style="text-align: right;">republican</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Alabama</td>
<td style="text-align: left;">AL</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">63</td>
<td style="text-align: right;">41</td>
<td style="text-align: left;">US House</td>
<td style="text-align: right;">1</td>
<td style="text-align: left;">gen</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: left;">total</td>
<td style="text-align: right;">157170</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">20171005</td>
<td style="text-align: right;">1976</td>
<td style="text-align: left;">0101</td>
<td style="text-align: left;">Jack Edwards</td>
<td style="text-align: right;">37.48</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">62.52</td>
</tr>
<tr class="even">
<td style="text-align: left;">Alabama</td>
<td style="text-align: left;">AL</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">63</td>
<td style="text-align: right;">41</td>
<td style="text-align: left;">US House</td>
<td style="text-align: right;">2</td>
<td style="text-align: left;">gen</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: left;">total</td>
<td style="text-align: right;">156362</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">20171005</td>
<td style="text-align: right;">1976</td>
<td style="text-align: left;">0102</td>
<td style="text-align: left;">William L. “Bill” Dickinson</td>
<td style="text-align: right;">42.39</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">57.60</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Alabama</td>
<td style="text-align: left;">AL</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">63</td>
<td style="text-align: right;">41</td>
<td style="text-align: left;">US House</td>
<td style="text-align: right;">3</td>
<td style="text-align: left;">gen</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: left;">total</td>
<td style="text-align: right;">108048</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">20171005</td>
<td style="text-align: right;">1976</td>
<td style="text-align: left;">0103</td>
<td style="text-align: left;">Bill Nichols</td>
<td style="text-align: right;">98.97</td>
<td style="text-align: right;">1.03</td>
<td style="text-align: right;">0.00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Alabama</td>
<td style="text-align: left;">AL</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">63</td>
<td style="text-align: right;">41</td>
<td style="text-align: left;">US House</td>
<td style="text-align: right;">4</td>
<td style="text-align: left;">gen</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: left;">total</td>
<td style="text-align: right;">176022</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">20171005</td>
<td style="text-align: right;">1976</td>
<td style="text-align: left;">0104</td>
<td style="text-align: left;">Tom Bevill</td>
<td style="text-align: right;">80.38</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">19.62</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Alabama</td>
<td style="text-align: left;">AL</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">63</td>
<td style="text-align: right;">41</td>
<td style="text-align: left;">US House</td>
<td style="text-align: right;">5</td>
<td style="text-align: left;">gen</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: left;">total</td>
<td style="text-align: right;">113560</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">20171005</td>
<td style="text-align: right;">1976</td>
<td style="text-align: left;">0105</td>
<td style="text-align: left;">Ronnie G. Flippo</td>
<td style="text-align: right;">99.99</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">0.00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Alabama</td>
<td style="text-align: left;">AL</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">63</td>
<td style="text-align: right;">41</td>
<td style="text-align: left;">US House</td>
<td style="text-align: right;">6</td>
<td style="text-align: left;">gen</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: left;">total</td>
<td style="text-align: right;">162518</td>
<td style="text-align: left;">FALSE</td>
<td style="text-align: right;">20171005</td>
<td style="text-align: right;">1976</td>
<td style="text-align: left;">0106</td>
<td style="text-align: left;">John H. Buchanan, Jr.</td>
<td style="text-align: right;">42.69</td>
<td style="text-align: right;">0.63</td>
<td style="text-align: right;">56.68</td>
</tr>
</tbody>
</table>

### § DailyKos: Presidential returns by congressional district (2008-)

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

### § Wikipedia: Presidential returns by state (1864-)

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
