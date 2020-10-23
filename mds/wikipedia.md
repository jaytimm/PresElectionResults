Wikipedia: Presidential returns by state (1864-)
------------------------------------------------

### § Some introductories

``` r
library(tidyverse)
setwd(git_dir)
## A simple summary of electoral presidential election results, 
## and some standard spellings
pres <- read.csv('us_pres_1864.csv') 
```

``` r
options(tigris_use_cache = TRUE, tigris_class = "sf")
states_full <- tigris::states(cb = TRUE) %>% 
  data.frame() %>%
  select(NAME, STATEFP, STUSPS) %>%
  rename(state_abbrev = STUSPS)

states <- states_full %>%
   filter(!NAME %in%  c('Pennsylvania',  'California') &
           !STATEFP %in% c('78', '69', '66', '72', '60')) %>%
  mutate(which_table = ifelse(NAME %in% c('New York', 'Missouri'), 3, 2)) 
```

### § Bulk of presidential results by state per Wikipedia

``` r
base_url <- 
  'https://en.wikipedia.org/wiki/United_States_presidential_elections_in_'
```

``` r
states_correct <- list()
for (i in 1:nrow(states)) {
  states_correct[[i]] <- 
    paste0(base_url, gsub(' ', '_', states$NAME[i])) %>%
    xml2::read_html() %>%
    rvest::html_node(
      xpath = paste0('//*[@id="mw-content-text"]/div/table[', 
                     states$which_table[i],']')) %>%
    rvest::html_table(fill = TRUE) 
  
  x <- states_correct[[i]][,c(1, 2, 4)]
  colnames(x) <- c('year', 'candidate', 'vote_share')
  y <- states_correct[[i]][,c(1, 5, 7)]
  colnames(y) <- c('year', 'candidate', 'vote_share')
  
  states_correct[[i]] <- rbind(x, y) %>%
    mutate(candidate = gsub('\\[.*\\]|\\(.*\\)', '', candidate),
           year = gsub("\\D+", "", year),
           year = as.integer(substr(year, 1,4)),
           vote_share = as.numeric(gsub('^$|-|%', 0, vote_share))
           )
  } 

names(states_correct) <- states$NAME
states_correct1 <- states_correct %>%
  bind_rows(.id = 'state_name')  %>%
  filter(candidate != 'TBD')
```

### § PA & CA results

> Presidential election results for California and Pennsylvania are
> structured differently on Wikipedia.

``` r
pa_url <- 'https://en.wikipedia.org/wiki/United_States_presidential_elections_in_Pennsylvania'
ca_url <- 'https://en.wikipedia.org/wiki/United_States_presidential_elections_in_California'
```

#### Pennsylvania

``` r
returns <- pa_url %>%
  xml2::read_html() %>%
  rvest::html_node(xpath = '//*[@id="mw-content-text"]/div/table[2]') %>%
  rvest::html_table(fill = TRUE)  

x <- returns[,c(1, 5, 6)]
colnames(x) <- c('year', 'candidate', 'vote_share')
y <- returns[,c(1, 9, 10)]
colnames(y) <- c('year', 'candidate', 'vote_share')

returns1 <- rbind(x, y) %>%
  filter(year > 1860) %>%
  mutate(vote_share = gsub('^.* \\(', '', vote_share),
         vote_share = as.numeric(gsub('%.*$', '', vote_share)),
         candidate = gsub(' Howard ', ' H\\. ', candidate),
         candidate = gsub('Charles Evans', 'Charles E\\.', candidate),
         candidate = gsub('Winfield Scott', 'Winfield S\\.', candidate),
         state_name = 'Pennsylvania') %>%
  select(state_name, year, candidate, vote_share) %>%
  unique() %>%
  arrange(desc(vote_share)) %>%
  group_by(year) %>%
  slice(1:2) %>% 
  ungroup()
```

#### California

``` r
returns_ca <- ca_url %>%
  xml2::read_html() %>%
  rvest::html_node(xpath = '//*[@id="mw-content-text"]/div/table[2]') %>%
  rvest::html_table(fill = TRUE) 

returns_ca <- returns_ca[-1,] 

x <- returns_ca[,c(1, 2, 5)]
colnames(x) <- c('year', 'candidate', 'vote_share')
y <- returns_ca[,c(1, 6, 9)]
colnames(y) <- c('year', 'candidate', 'vote_share')

returns_ca1 <- rbind(x, y) %>%
  filter(year > 1860) %>%
  mutate(vote_share = as.numeric(gsub('%', '', vote_share)),
         candidate = gsub('^Y|^N|\\[.*\\]', '', candidate), 
         year = gsub('\\[.*\\]', '', year), 
         state_name = 'California') %>%
  filter(candidate != 'TBD')  %>%
  select(state_name, year, candidate, vote_share)
```

### § Piecing things together

``` r
###
full <- bind_rows(states_correct1,
                  returns1,
                  returns_ca)%>%
  mutate(vote_share = ifelse(year %in% c('1864', '1868') &
                               vote_share == 0,
                             NA, vote_share),
         candidate = trimws(candidate),
         district_code = 'statewide') %>%
  na.omit() %>% 
  left_join(pres %>% select(-electoral_votes)) %>%
  rename(NAME = state_name) %>%
  
  left_join(states_full) %>%
  
  select(year, state_abbrev, district_code, candidate:party) 
```

### § A quick re-structure

> Modified for most frequent application-types/use-cases.

``` r
full1 <- full %>%
  group_by(state_abbrev, year) %>%   
  mutate(winner = candidate[which.max(vote_share)],
         party = tolower(party),
         party_win = party[which.max(vote_share)]) %>%
  ungroup() %>%
  select(-candidate) %>%
  spread(party, vote_share)
```
