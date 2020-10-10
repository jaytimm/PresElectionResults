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

MEDSL: House returns by congressional district (1976-)
------------------------------------------------------

``` r
#Follow MEDSL convention per at-large Reps as '0'
library(tidyverse)
# medsl_house <- "https://raw.githubusercontent.com/MEDSL/constituency-returns/master/1976-2018-house.csv"
# read.csv(url(medsl_house))

medsl_house_file <- "1976-2018-house.csv"
medsl_senate_file <- "1976-2018-senate.csv"

medsl_house <- read.csv(paste0(git_dir, medsl_house_file), 
                        na.strings = c("", "NA")) %>%
  ##
  mutate(GEOID = paste0(stringr::str_pad (state_fips,2, pad = 0),
                        stringr::str_pad (district,2, pad = 0))) 
```

Load data –

``` r
house_returns <- medsl_house %>%
  distinct() %>%
  mutate(totalvotes = ifelse(state == 'Florida' & year == 2018, totalvotes/2, totalvotes)) %>%
  
  group_by(state, district, candidate) %>%
  arrange(party) %>%
  fill(party) %>% # correct (some) party affiliation NAs -- non-write-ins -- 
  
  group_by_at(vars(-version, -totalvotes)) %>% 
  filter(version == max(version) & stage == 'gen') %>% ## newest version
  
  group_by_at(vars(-candidatevotes, -party)) %>% 
  mutate(fullcandidatevotes = sum(candidatevotes)) %>% ## Zeldin -- could demonstrate -- 
  filter(candidatevotes == max(candidatevotes)) %>%
  
  group_by_at(vars(-candidatevotes, -party, -candidate, -fullcandidatevotes, -writein)) %>% 
  mutate(winner = ifelse(fullcandidatevotes == max(fullcandidatevotes), 'y', 'n')) %>%
  ungroup() %>%
  select(-candidatevotes)
```

Re-work (slightly) :

``` r
hrs <- house_returns %>%
  filter(writein == FALSE) %>%
  
      mutate(party = ifelse(grepl('^democrat', party), 'democrat', party)) %>%
      mutate(party = ifelse(grepl('^republican', party), 'republican', party)) %>%
      ## independent + green ? -- 
      mutate(party = ifelse(!grepl('^republican|^democrat', party), 'other', party)) %>%

  mutate(candidate = ifelse(winner == 'n', NA, candidate)) %>%
  #mutate(party1 = ifelse(winner == 'n', NA, party)) %>%
  
  group_by_at(vars(-fullcandidatevotes, -winner)) %>% 
  summarize(candidatevotes = sum(fullcandidatevotes)) %>% # agg over "other"
  ungroup() %>%
  
  fill(candidate) %>%  # fill(party1) %>%
  mutate(candidatevotes = round(candidatevotes/totalvotes *100, 2)) %>%
  
  group_by_at(vars(-candidatevotes)) %>%
  filter(candidatevotes == max(candidatevotes)) %>% # some races more than one dem, eg--
 
  spread(party, candidatevotes) %>%
  replace(., is.na(.), 0) 
```

DailyKos: Presidential returns by congressional district (2008-)
----------------------------------------------------------------

``` r
url <- 'https://docs.google.com/spreadsheets/d/1oRl7vxEJUUDWJCyrjo62cELJD2ONIVl-D9TSUKiK9jk/edit#gid=1178631925'

house <- gsheet::gsheet2tbl(url) %>% janitor::clean_names()
start_prez <- min(grep('president', colnames(house)))
end_prez <-  min(grep('house', colnames(house))) - 1
house3 <- house[3:nrow(house), c(2, start_prez:end_prez)]

colnames(house3)[2:7] <- c('Clinton_2016_democrat',
                           'Trump_2016_republican',
                           'Obama_2012_democrat',
                           'Romney_2012_republican',
                           'Obama_2008_democrat',
                           'McCain_2008_republican') # uniform to wiki set
```

Clean things –

``` r
dk <- house3 %>%
  gather(key = election, value = percent, -code) %>%
  separate(code, c('state_abbrev', 'district_code')) %>%
  separate(election, c('candidate', 'year', 'party')) %>%

  group_by(year, state_abbrev, district_code) %>%
  mutate(winner = candidate[which.max(percent)]) %>%
  ungroup() %>%
  select(-candidate) %>%
  spread(party, percent) %>%
  mutate(district_code = gsub('[A-Z][A-Z]', '00', district_code))

## hand corrections --
dk$winner[dk$state_abbrev == 'FL' & 
            dk$year == 2012 & dk$district_code == '7'] <- 'Obama'
dk$winner[dk$state_abbrev == 'OH' & 
            dk$year == 2008 & dk$district_code == '10'] <- 'Obama'
dk$winner[dk$state_abbrev == 'NY' & 
            dk$year == 2008 & dk$district_code == '22'] <- 'McCain'
```

Wikipedia: Presidential returns by state (1864-)
------------------------------------------------

``` r
setwd(git_dir)
pres <- read.csv('us_pres_1864.csv') 
```

``` r
base_url <- 'https://en.wikipedia.org/wiki/United_States_presidential_elections_in_'
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

``` r
states_correct <- list()
for (i in 1:nrow(states)) {
  states_correct[[i]] <- 
    paste0(base_url, gsub(' ', '_', states$NAME[i])) %>%
    xml2::read_html() %>%
    rvest::html_node(xpath = paste0('//*[@id="mw-content-text"]/div/table[', 
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
           vote_share = as.numeric(gsub('^$|-|%', 0, vote_share))    )
} ## end for loop -- 

names(states_correct) <- states$NAME
states_correct1 <- states_correct %>%
  bind_rows(.id = 'state_name')  %>%
  filter(candidate != 'TBD')
```

``` r
##### PA -- 
url <- 'https://en.wikipedia.org/wiki/United_States_presidential_elections_in_Pennsylvania'

returns <- url %>%
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

``` r
##### CA -- 
url <- 'https://en.wikipedia.org/wiki/United_States_presidential_elections_in_California'

returns_ca <- url %>%
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

``` r
setwd(git_dir)
## hand corrections on the names --
#pres <- write.csv(returns_ca1, 'ca_pres_1864xx.csv')
returns_ca2 <- read.csv('ca_pres_1864.csv')
```

``` r
###
full <- bind_rows(states_correct1,
                  returns1,
                  returns_ca2)%>%
  mutate(vote_share = ifelse(year %in% c('1864', '1868') &
                               vote_share == 0,
                             NA, vote_share),
         candidate = trimws(candidate)) %>%
  na.omit() %>% 
  left_join(pres %>% select(-electoral_votes)) %>%
  arrange(desc(vote_share)) %>%
  group_by(state_name, year) %>%
  mutate(winner = row_number()) %>%
  ungroup()%>%
  rename(NAME = state_name) %>%
  
  left_join(states_full) %>%
  select(STATEFP:state_abbrev, NAME:winner) %>%
  rename(GEOID = STATEFP, state = NAME)

# setwd(pdir)
# #saveRDS(pres, 'pres_1864.rds')
# #saveRDS(full, 'pres_elections_state.rds')
# write.csv(full, 'pres_elections_state.csv', 
#           row.names = F)
```
