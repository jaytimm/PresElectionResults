### MEDSL: House returns by congressional district (1976-)

``` r
#Follow MEDSL convention per at-large Reps as '0'
library(tidyverse)
# medsl_house <- "https://raw.githubusercontent.com/MEDSL/constituency-returns/master/1976-2018-house.csv"
# read.csv(url(medsl_house))
```

``` r
medsl_house <- read.csv(paste0(git_dir, medsl_house_file), na.strings = c("", "NA"))
medsl_senate <- read.csv(paste0(git_dir, medsl_senate_file), na.strings = c("", "NA"))
```

Load data –

``` r
returns <- medsl_house %>%
  #medsl_senate %>%
  distinct() %>%
  mutate(totalvotes = ifelse(state == 'Florida' & year == 2018, 
                             totalvotes/2, totalvotes),
         
         #Florida 24th, 2016 --
         totalvotes = ifelse(totalvotes == 0, 1, totalvotes),
         candidatevotes = ifelse(candidatevotes == 0, 1, candidatevotes)) %>%
  
  # correct (some) party affiliation NAs -- non-write-ins --
  group_by(state, district, candidate) %>% # house
  #  group_by(state, candidate) %>% # senate
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
  ungroup() %>%
  select(-candidatevotes)
```

Re-work (slightly) :

``` r
hrs <- returns %>%
  filter(writein == FALSE & unofficial == FALSE) %>%
  
      mutate(party = ifelse(grepl('^democrat', party), 
                            'democrat', party)) %>%
      mutate(party = ifelse(grepl('^republican', party), 
                            'republican', party)) %>%
      ## independent + green ? -- 
      mutate(party = ifelse(!grepl('^republican|^democrat', party), 
                            'other', party)) %>%

  # agg over "other"
  group_by_at(vars(-fullcandidatevotes)) %>% 
  summarize(candidatevotes = sum(fullcandidatevotes)) %>% 
  ungroup() %>%
  
  mutate(candidatevotes = round(candidatevotes/totalvotes *100, 2)) %>%
  
  # some races more than one dem, eg--
  group_by_at(vars(-candidate, -candidatevotes)) %>%
  arrange(desc(candidatevotes)) %>%
  slice(1) %>%
  #filter(candidatevotes == max(candidatevotes)) %>% 
  ungroup() 
```

A simple clean – for uniform output and get rid of some less useful
columns –
