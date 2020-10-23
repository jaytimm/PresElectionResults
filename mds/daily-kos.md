DailyKos: Presidential returns by congressional district (2008-)
----------------------------------------------------------------

``` r
library(tidyverse)
```

### § Load data per url

``` r
url <- 
  'https://docs.google.com/spreadsheets/d/1zLNAuRqPauss00HDz4XbTH2HqsCzMe0pR8QmD1K8jk8/edit#gid=0'
```

``` r
house <- gsheet::gsheet2tbl(url) %>% janitor::clean_names()
start_prez <- min(grep('president', colnames(house)))
end_prez <-  min(grep('house', colnames(house))) - 1
house3 <- house[3:nrow(house), c(1, 4:9)]

colnames(house3)[1:7] <- c('code',
                           'Hillary-Clinton_2016_democrat',
                           'Donald-Trump_2016_republican',
                           'Barack-Obama_2012_democrat',
                           'Mitt-Romney_2012_republican',
                           'Barack-Obama_2008_democrat',
                           'John-McCain_2008_republican') 
# uniform to wiki set
```

### § A simple clean

``` r
dk <- house3 %>%
  gather(key = election, value = percent, -code) %>%
  separate(code, c('state_abbrev', 'district_code')) %>%
  separate(election, c('candidate', 'year', 'party'), sep = "_") %>%

  group_by(year, state_abbrev, district_code) %>%
  mutate(winner = candidate[which.max(percent)],
         winner = gsub('-', ' ', winner),
         party_win = party[which.max(percent)],
         percent = as.numeric(percent)) %>%
  
  ungroup() %>%
  select(-candidate) %>%
  spread(party, percent) %>%
  mutate(district_code = gsub('[A-Z][A-Z]', '00', district_code),
         year = as.integer(year)) %>%
  select(year, state_abbrev:republican)
```

### § Some hand corrections

``` r
dk$winner[dk$state_abbrev == 'FL' & 
            dk$year == 2012 & dk$district_code == '7'] <- 'Obama'
dk$winner[dk$state_abbrev == 'OH' & 
            dk$year == 2008 & dk$district_code == '10'] <- 'Obama'
dk$winner[dk$state_abbrev == 'NY' & 
            dk$year == 2008 & dk$district_code == '22'] <- 'McCain'
```
