MEDSL: Presidential returns by county (2000-)
---------------------------------------------

> This data set is super clean.

``` r
library(tidyverse)
```

``` r
url1 <- 'https://raw.githubusercontent.com/MEDSL/county-returns/master/countypres_2000-2016.csv'
county <- read.csv(url1)
```

### § Uniform output

``` r
# 2000 - MO - Kansas City -
## only one version -- 
county1 <- county %>%
  filter(!is.na(candidatevotes)) %>%
  select(-state, -office, -version, -totalvotes) %>%
  group_by(year, state_po, county) %>%
  mutate(per = round(candidatevotes/sum(candidatevotes)*100, 1)) %>%

  mutate(winner =  candidate[which.max(candidatevotes)],
         party_win = party[which.max(candidatevotes)]) %>%
  ungroup() %>%
  select(-candidate, -candidatevotes) %>%
  filter(party %in% c('republican', 'democrat')) %>%
  spread(party, per) %>%
  rename(state_abbrev = state_po)
```
