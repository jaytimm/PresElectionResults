MEDSL: House returns by congressional district (1976-)
------------------------------------------------------

> Work-flow for both House & Senate

``` r
#Follow MEDSL convention per at-large Reps as '0'
library(tidyverse)
medsl_house_file <- 
  "https://raw.githubusercontent.com/MEDSL/constituency-returns/master/1976-2018-house.csv"
medsl_house <- read.csv(url(medsl_house_file), na.strings = c("", "NA"))
```

### Load data & slight clean

``` r
which <-  medsl_senate 

returns <- which %>%
  distinct() %>%
  mutate(totalvotes = ifelse(state == 'Florida' & year == 2018, 
                             totalvotes/2, totalvotes),
         
         #Florida 24th, 2016 --
         totalvotes = ifelse(totalvotes == 0, 1, totalvotes),
         candidatevotes = ifelse(candidatevotes == 0, 1, 
                                 candidatevotes)) %>%
  
  # correct (some) party affiliation NAs -- non-write-ins --
  group_by(state, district, candidate) %>% # house
  #group_by(state, candidate) %>% # senate
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

### Some small re-workings

``` r
hrs <- returns %>%
  filter(writein == FALSE & unofficial == FALSE & special == FALSE) %>%
  
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

### Uniform output

``` r
final <- hrs %>%
  group_by_at(vars(-party, -candidate, -candidatevotes)) %>% 
  mutate(winner = candidate[which.max(candidatevotes)],
         party_win = party[which.max(candidatevotes)]) %>%
  
  ungroup() %>%
  select(-candidate) %>%
  spread(party, candidatevotes) %>%
  replace(., is.na(.), 0) %>%
  
  left_join(data.frame(year = c(1974 + 2*rep(c(1:22))), 
                       congress = c(95:116)), by = 'year') %>%
  
  select(year, congress, state_po, district, winner, party_win, democrat, republican, other) %>%
  mutate(district = stringr::str_pad (district, 2, pad = 0)) %>%
  rename(district_code = district, state_abbrev = state_po) 
```

### Add some VoteView details

``` r
vv <- lapply(c(94:116), function (x)
                    Rvoteview::member_search (
                      chamber = 'Senate', 
                      congress = x)) %>% # better ways, but 'seo_name' is helpful for distance matching -- 
  bind_rows() %>%
  

  ## at-large -- HOUSE ONLY --
  group_by(congress, state_abbrev) %>%
  mutate(x = length(unique(district_code))) %>%
  ungroup() %>%
  mutate(district_code = ifelse(x==1, 0, district_code)) %>%
  mutate(district_code = stringr::str_pad (as.numeric(district_code), 2, pad = 0)) 
```

### Match MEDSL candidate-name & VoteView

> In MEDSL, per every election winner (no specials) & per every
> year/state/district: find orthographically closest name from list of
> lawmakers in VoteView –

``` r
ctable <- final

for(i in 1:nrow(ctable)) {
  
    x2 <- subset(
      
      vv, ## filter voteview table -- 
      congress == ctable[i,]$congress &
       state_abbrev == ctable[i,]$state_abbrev) # Senate 
      #state_abbrev == ctable[i,]$state_abbrev & district_code == ctable[i,]$district_code) #house
    
    x3 <- which.min(adist(ctable[i,]$winner, 
                          gsub('-', ' ', x2$seo_name), 
                          ignore.case = TRUE))
    
    ctable$icpsr[i] <- as.character(x2[x3, 2])
}

ctable1 <- ctable %>% select(year:winner, icpsr, party_win:other)
```
