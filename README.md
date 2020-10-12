uspols
======

A simple collection of American political data, including federal
election returns and Twitter details for US lawmakers, aggregated from
existing resources as an **R data package**.

> -   House and Senate returns by congressional district and state,
>     respectively, (from 1976 onward) made available via [MEDSL]().  
> -   Presidential returns by congressional district for 2008, 2012, and
>     2016 elections via [The DailyKos]().
> -   Presidential returns by state for elections since 1864, scraped
>     from Wikipedia.

Formats have been tweaked ever so slightly for a uniform output across
data sets. Links to R code demonstrate all details of work-flow from raw
data to package table.

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

<table>
<colgroup>
<col style="width: 4%" />
<col style="width: 8%" />
<col style="width: 11%" />
<col style="width: 12%" />
<col style="width: 25%" />
<col style="width: 5%" />
<col style="width: 9%" />
<col style="width: 8%" />
<col style="width: 9%" />
<col style="width: 5%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: right;">year</th>
<th style="text-align: right;">congress</th>
<th style="text-align: left;">state_abbrev</th>
<th style="text-align: left;">district_code</th>
<th style="text-align: left;">winner</th>
<th style="text-align: left;">icpsr</th>
<th style="text-align: left;">party_win</th>
<th style="text-align: right;">democrat</th>
<th style="text-align: right;">republican</th>
<th style="text-align: right;">other</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: right;">1976</td>
<td style="text-align: right;">95</td>
<td style="text-align: left;">AL</td>
<td style="text-align: left;">01</td>
<td style="text-align: left;">Jack Edwards</td>
<td style="text-align: left;">10721</td>
<td style="text-align: left;">republican</td>
<td style="text-align: right;">37.48</td>
<td style="text-align: right;">62.52</td>
<td style="text-align: right;">0.00</td>
</tr>
<tr class="even">
<td style="text-align: right;">1976</td>
<td style="text-align: right;">95</td>
<td style="text-align: left;">AL</td>
<td style="text-align: left;">02</td>
<td style="text-align: left;">William L. “Bill” Dickinson</td>
<td style="text-align: left;">10717</td>
<td style="text-align: left;">republican</td>
<td style="text-align: right;">42.39</td>
<td style="text-align: right;">57.60</td>
<td style="text-align: right;">0.00</td>
</tr>
<tr class="odd">
<td style="text-align: right;">1976</td>
<td style="text-align: right;">95</td>
<td style="text-align: left;">AL</td>
<td style="text-align: left;">03</td>
<td style="text-align: left;">Bill Nichols</td>
<td style="text-align: left;">11037</td>
<td style="text-align: left;">democrat</td>
<td style="text-align: right;">98.97</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">1.03</td>
</tr>
<tr class="even">
<td style="text-align: right;">1976</td>
<td style="text-align: right;">95</td>
<td style="text-align: left;">AL</td>
<td style="text-align: left;">04</td>
<td style="text-align: left;">Tom Bevill</td>
<td style="text-align: left;">11000</td>
<td style="text-align: left;">democrat</td>
<td style="text-align: right;">80.38</td>
<td style="text-align: right;">19.62</td>
<td style="text-align: right;">0.00</td>
</tr>
<tr class="odd">
<td style="text-align: right;">1976</td>
<td style="text-align: right;">95</td>
<td style="text-align: left;">AL</td>
<td style="text-align: left;">05</td>
<td style="text-align: left;">Ronnie G. Flippo</td>
<td style="text-align: left;">14419</td>
<td style="text-align: left;">democrat</td>
<td style="text-align: right;">99.99</td>
<td style="text-align: right;">0.00</td>
<td style="text-align: right;">0.00</td>
</tr>
<tr class="even">
<td style="text-align: right;">1976</td>
<td style="text-align: right;">95</td>
<td style="text-align: left;">AL</td>
<td style="text-align: left;">06</td>
<td style="text-align: left;">John H. Buchanan, Jr.</td>
<td style="text-align: left;">10706</td>
<td style="text-align: left;">republican</td>
<td style="text-align: right;">42.69</td>
<td style="text-align: right;">56.68</td>
<td style="text-align: right;">0.63</td>
</tr>
</tbody>
</table>

### § DailyKos: Presidential returns by congressional district (2008-)

[Code](https://github.com/jaytimm/uspols/blob/master/daily-kos.md)

``` r
uspols::uspols_dk_pres %>%
  head() %>% knitr::kable()
```

|  year| state\_abbrev | district\_code | winner       | party\_win | democrat | republican |
|-----:|:--------------|:---------------|:-------------|:-----------|:---------|:-----------|
|  2008| AK            | 00             | John McCain  | republican | 38.1     | 59.7       |
|  2012| AK            | 00             | Mitt Romney  | republican | 41.2     | 55.3       |
|  2016| AK            | 00             | Donald Trump | republican | 37.6     | 52.8       |
|  2008| AL            | 01             | John McCain  | republican | 38.5     | 60.9       |
|  2012| AL            | 01             | Mitt Romney  | republican | 37.4     | 61.8       |
|  2016| AL            | 01             | Donald Trump | republican | 34.1     | 63.5       |

### § Wikipedia: Presidential returns by state (1864-)

[Code](https://github.com/jaytimm/uspols/blob/master/wikipedia.md) \|
Thoughts, et.

``` r
uspols::uspols_wiki_pres %>%
  head() %>% knitr::kable()
```

|  year| state\_abbrev | winner              | party\_win |  democrat|  republican|
|-----:|:--------------|:--------------------|:-----------|---------:|-----------:|
|  1864| CA            | Abraham Lincoln     | republican |     41.40|       58.60|
|  1864| CT            | Abraham Lincoln     | republican |     48.60|       51.40|
|  1864| DE            | George B. McClellan | democrat   |     51.80|       48.20|
|  1864| IA            | Abraham Lincoln     | republican |     36.92|       63.08|
|  1864| IL            | Abraham Lincoln     | republican |     45.60|       54.40|
|  1864| IN            | Abraham Lincoln     | republican |     46.50|       53.50|

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
