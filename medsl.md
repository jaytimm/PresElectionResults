### MEDSL: House returns by congressional district (1976-)

``` r
#Follow MEDSL convention per at-large Reps as '0'
library(tidyverse)
# medsl_house <- "https://raw.githubusercontent.com/MEDSL/constituency-returns/master/1976-2018-house.csv"
# read.csv(url(medsl_house))
```

``` r
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
  mutate(totalvotes = ifelse(state == 'Florida' & year == 2018, 
                             totalvotes/2, totalvotes)) %>%
  
  # correct (some) party affiliation NAs -- non-write-ins --
  group_by(state, district, candidate) %>%
  arrange(party) %>%
  fill(party) %>%  
  
  # filter to latest version
  group_by_at(vars(-version, -totalvotes)) %>% 
  filter(version == max(version) & stage == 'gen') %>% 
  
  # agg over votes from multiple parties
  group_by_at(vars(-candidatevotes, -party)) %>% 
  mutate(fullcandidatevotes = sum(candidatevotes)) %>%
  filter(candidatevotes == max(candidatevotes)) %>%
  
  group_by_at(vars(-candidatevotes, -party, -candidate, 
                   -fullcandidatevotes, -writein)) %>% 
  mutate(winner = ifelse(fullcandidatevotes == max(fullcandidatevotes), 
                         'y', 'n')) %>%
  ungroup() %>%
  select(-candidatevotes)
```

Re-work (slightly) :

``` r
hrs <- house_returns %>%
  filter(writein == FALSE) %>%
  
      mutate(party = ifelse(grepl('^democrat', party), 
                            'democrat', party)) %>%
      mutate(party = ifelse(grepl('^republican', party), 
                            'republican', party)) %>%
      ## independent + green ? -- 
      mutate(party = ifelse(!grepl('^republican|^democrat', party), 
                            'other', party)) %>%

  mutate(candidate = ifelse(winner == 'n', NA, candidate)) %>%
  #mutate(party1 = ifelse(winner == 'n', NA, party)) %>%
  
  # agg over "other"
  group_by_at(vars(-fullcandidatevotes, -winner)) %>% 
  summarize(candidatevotes = sum(fullcandidatevotes)) %>% 
  ungroup() %>%
  
  fill(candidate) %>%  # fill(party1) %>%
  mutate(candidatevotes = round(candidatevotes/totalvotes *100, 2)) %>%
  
  # some races more than one dem, eg--
  group_by_at(vars(-candidatevotes)) %>%
  filter(candidatevotes == max(candidatevotes)) %>% 
 
  spread(party, candidatevotes) %>%
  replace(., is.na(.), 0) 
```
