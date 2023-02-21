# PresElectionResults

``` r
library(dplyr)
```

## Presidential Election Results – Britannica

> Sources: Electoral and popular vote totals based on data from the
> Office of the Clerk of the U.S. House of Representatives; the United
> States Office of the Federal Register; the Federal Election
> Commission; Congressional Quarterly’s Guide to U.S. Elections, 4th
> ed. (2001); and the official certified state vote totals.

``` r
url99 <- 'https://www.britannica.com/topic/United-States-Presidential-Election-Results-1788863'

ecs <- url99 |>
   xml2::read_html() |>
    rvest::html_node(
      xpath = '//*[@class="md-raw-html"]/div/table/tbody') |>
    rvest::html_table(fill = TRUE) 

colnames(ecs) <- c('year', 
                   'candidate', 
                   'party', 
                   'ec_votes', 
                   'pop_votes', 
                   'pop_per')


fns <- data.frame(year = c(1789,
                           1800, 
                           1800,
                           1824,
                           2000),
                  
                  candidate = c('George Washington', 
                                'Thomas Jefferson', 
                                'Aaron Burr',
                                'John Quincy Adams',
                                'Al Gore'),
                  z = 1)


pres_results <- ecs |>
  mutate(pop_votes = as.integer(gsub(',', '', pop_votes)),
         pop_per = as.numeric(pop_per),
         candidate = gsub('[0-9]', '', candidate) |> trimws(),
         candidate = gsub('George Bush$', 'George H.W. Bush', candidate),
         
         # Horace Greeley
         party = gsub('/Liberal Republican', '', party),
         party = gsub('[0-9]', '', party),
         party = gsub('no distinct party designations|no formally organized parties', 
                                '', 
                                party),
         party = trimws(party)) |>

  left_join(fns) |>
  mutate(ec_votes = ifelse(!is.na(z), 
                           gsub('.$', '', ec_votes), 
                           ec_votes),
         ec_votes = ifelse(candidate == 'Al Gore', 
                           gsub('.$', '', ec_votes), 
                           ec_votes),
         
         ec_votes = as.integer(ec_votes)) |>
  select(-z) |>
  
  group_by(year) |>
  mutate(ec_total = sum(ec_votes, na.rm = T)) |> ungroup() |>
  filter(!party %in% 'not a candidate',
         !candidate %in% 'not voted')
```

## Presidential election results by county (2000-2020) – MIT Election Data and Science Lab (MEDSL)

<https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ>

``` r
library(dplyr)
setwd(dataraw_dir)
county <- read.csv('countypres_2000-2020.csv')

pres_by_county <- county |>
  filter(!is.na(candidatevotes)) |>
  group_by_at(vars(all_of(colnames(county)[c(1:8,10)]))) |>
  summarize(candidatevotes = sum(candidatevotes),
            totalvotes = mean(totalvotes)) |> ungroup() |>
  mutate(per = round(candidatevotes/totalvotes*100, 1)) |>
  
  mutate(party = tolower(party),
         #party = stringr::str_to_title(tolower(party)),
         county_name = stringr::str_to_title(tolower(county_name))) |>
  
  group_by(year, state_po, county_name, county_fips) |>

  mutate(winner =  candidate[which.max(candidatevotes)],
         winner = stringr::str_to_title(tolower(winner)),
         party_win = party[which.max(candidatevotes)]) |>
  ungroup() |>
  select(-candidate, -candidatevotes) |>
  filter(party %in% c('republican', 'democrat')) |>
  tidyr::spread(party, per) |>
  rename(state_abbrev = state_po)|>
  mutate(GEOID = stringr::str_pad(county_fips, 5, pad = "0")) |>
  select(1, 3, 4, 12, 8:11) 
```

## Presidential election results by congressional district (2020) – Daily Kos

> Per 2022 district boundaries

``` r
uro <- 'https://docs.google.com/spreadsheets/d/1CKngqOp8fzU22JOlypoxNsxL6KSAH920Whc-rd7ebuM/edit?skip_itp2_check=true&pli=1#gid=1871835782'

pres_by_cd <- gsheet::gsheet2tbl(uro) |> 
  janitor::clean_names() |>
  mutate(winner = ifelse(biden > trump, 
                         'Joe Biden', 
                         'Donald Trump'),
         party_win = ifelse(biden > trump, 
                            'democrat', 
                            'republican'),
         house_rep_party = ifelse(party == '\\(D\\)', 
                                  'democrat', 
                                  'republican')) |> 
  
  rename(democrat = biden,
         republican = trump,
         house_rep = incumbent) |>
  
  tidyr::separate(district, into = c('state_abbrev', 'district_code'), sep = '-') |>
  mutate(district_code = gsub('AL', '00', district_code)) |>
  select(state_abbrev, district_code, 
         house_rep, house_rep_party,
         winner, party_win, 
         democrat, republican)
```

## Presidential election results by state (1864-) – Wikipedia

### Table details by state

``` r
options(tigris_use_cache = TRUE, tigris_class = "sf")
states_full <- tigris::states(cb = TRUE) |> 
  data.frame() |>
  select(NAME, STATEFP, STUSPS) |>
  rename(state_abbrev = STUSPS)

###
w3a <- c('New Mexico', 
         'Alaska', 
         'Arizona', 
         'District of Columbia',
         'Hawaii')

w3b <- c('New York')
w3c <- c('Pennsylvania')
w3d <- c('California')
w5 <- c('Nebraska')
w4a <- c('Maine')
w4b <- c('Utah')
w6 <- c('Arkansas', 'Florida')

states <- states_full |>
  
  mutate (l1 = case_when (NAME %in% c(w3a, w6, w4b) ~ list(list(1,3,4,5)), #124
                          !NAME %in% c(w3a, w3c, w3d, w6, w4b) ~ list(list(1,2,3,4)),
                          NAME %in% w3c ~ list(list(1,5,6,6)),
                          NAME %in% w3d ~ list(list(1,2,5,6))),
          
          l2 = case_when (NAME %in% c(w3a, w6, w4b) ~ list(list(1,7,8, 9)),
                          !NAME %in% c(w3a, w3c, w3d, w6, w4b) ~ list(list(1,5,6,7)),
                          NAME %in% w3c ~ list(list(1,9,10,10)),
                          NAME %in% w3d ~ list(list(1,8,11,12))),
          
          ns = case_when (NAME %in% c(w3a, w3b, w3c, w3d) ~ 3,
                          NAME %in% c(w4a, w4b) ~ 4,
                          NAME %in% c(w5) ~ 5,
                          NAME %in% c(w6) ~ 6,
                          !NAME %in% c(w3a, w3b, w3c, w3d, w4a, w4b, w5, w6) ~ 2)
  ) |>
  
  mutate(ns = ns) |>
  filter(!STATEFP %in% c('78', '69', '66', '72', '60')) |>
  mutate(NAME = gsub('Washington', 'Washington (state)', NAME))
```

### Scrape tables

``` r
base_url <- 
  'https://en.wikipedia.org/wiki/United_States_presidential_elections_in_'
```

``` r
states_correct <- list()

for (i in 1:nrow(states)) {
  
  states_correct[[i]] <- 
    paste0(base_url, gsub(' ', '_', states$NAME[i])) |>
    xml2::read_html() |>
    rvest::html_node(
      xpath = paste0('//*[@id="mw-content-text"]/div/table[', 
                     states$ns[i],']')) |>
    rvest::html_table(fill = TRUE) 
  
  if(nrow(states_correct[[i]]) > 2){
    
    x <- states_correct[[i]][, states$l1[i] |> unlist()]
    colnames(x) <- c('year', 'candidate', 'votes', 'vote_share')
    y <- states_correct[[i]][, states$l2[i] |> unlist()]
    colnames(y) <- c('year', 'candidate', 'votes', 'vote_share')
    
    states_correct[[i]] <- rbind(x, y) |>
      mutate(year = gsub("\\D+", "", year),
             year = as.integer(substr(year, 1,4)),
             vote_share = as.character(vote_share)
             )
  } else(states_correct[[i]] <- states_correct[[i]])
} 

names(states_correct) <- states$NAME
states_correct1 <- states_correct |>
  bind_rows(.id = 'state_name')  |>
  mutate(votes = as.integer(gsub('\\(.*\\)|,', '', votes))) |>
  mutate(vote_share = gsub('(^.*\\()(.*)(\\)$)', '\\2', vote_share),
         vote_share =as.numeric(gsub('%', '', vote_share)),
         # wiki_party = gsub('(^.*\\()([A-Za-z.]*)(\\)$)', '\\2', candidate),
         candidate = gsub('\\(.*\\)|\\[.*\\]', '', candidate) |> trimws()) |>

  filter(candidate != 'TBD',
         nchar(candidate) > 5,
         !is.na(year))
```

### Resolve wiki and britannica candidate names

``` r
brits <- unique(pres_results$candidate)
wikis <- unique(states_correct1$candidate)
## setdiff(brits, wikis)

nnb <- sapply(wikis, function(x){
   brits[adist(x, brits) |> which.min()]
})

xx <- data.frame(wiki = nnb |> names() |> trimws(),
                 brit = nnb |> unname() |> trimws()) |>
  unique() 

ms <- c('Adlai Stevenson II',
        'Al Smith',
        'Charles Pinckney')

cross <- xx |>
  # mutate(brit = ifelse(wiki %in% ms, NA, brit)) |>
  mutate(wiki =  ifelse(brit == 'William H. Crawford', 'William H. Crawford', wiki)) |>
  mutate(brit = ifelse(wiki == 'John Q. Adams', 'John Quincy Adams', brit)) |> 
  mutate(brit = ifelse(wiki == 'Charles Pinckney', 'Charles Cotesworth Pinckney', brit)) |> 
  unique() |> na.omit()
```

### Final election return table

``` r
pres_by_state <- states_correct1 |>
  mutate(vote_share = ifelse(year %in% c('1864', '1868') &
                               vote_share == 0,
                             NA, vote_share),
         
         candidate = trimws(candidate)) |>
  
  ## na.omit() |> 
  left_join(cross, by = c('candidate' = 'wiki')) |>
  left_join(pres_results |> select(year:party) |>
              mutate(party = ifelse(party %in% 'Democratic', 'Democrat', party)), 
            by = c('brit' = 'candidate',
                                                'year' = 'year')) |>
  mutate(party = ifelse(candidate %in% c('Al Smith', 'Adlai Stevenson II'),
                        'Democrat', party)) |>
  
  rename(NAME = state_name) |>
  left_join(states) |>
  select(year, state_abbrev, brit, party, votes:vote_share) |>
  rename(candidate = brit) |>
  filter(year >= 1864, !is.na(vote_share)) |>
  select(-votes) |>
  unique() |>
  mutate(party = ifelse(!party %in% c('Democrat', 'Republican'), 
                        'Other', party)) |>
  
  group_by(year, state_abbrev, party) |>
  filter(vote_share == max(vote_share)) |>
  ungroup() |>
  
  group_by(state_abbrev, year) |>   
  mutate(winner = candidate[which.max(vote_share)],
         #party = tolower(party),
         party_win = tolower(party[which.max(vote_share)])) |>
  ungroup() |>
  
  select(-candidate) |>
  
  unique() |>
  tidyr::spread(party, vote_share)

colnames(pres_by_state) <- colnames(pres_by_state) |> tolower() 
```

## Equal-area simple feature geometries – Daily Kos

<https://docs.google.com/spreadsheets/d/1LrBXlqrtSZwyYOkpEEXFwQggvtR0bHHTxs9kq4kjOjw/edit#gid=0>

``` r
fnames <- c('HexCDv30wm',
            'HexSTv30wm',
            'TileInv10', 
            'TileOutv10')

sfs <- lapply(fnames, function(x) {

  sf::st_read(dsn = paste0(dataraw_dir, x),
                     layer = x,
                     quiet = TRUE) })
names(sfs) <- fnames

xsf_HexCDv30wm <- sfs$HexCDv30 |>
  rename(state_abbrev = STATEAB,
         state = STATENAME) |>
  mutate(district_code = gsub('[A-Z][A-Z]', 0, CDLABEL),
         district_code = stringr::str_pad (district_code, 2, pad = 0)) |>
  select(GEOID, state, state_abbrev, district_code)


xsf_HexSTv30wm <- sfs$HexSTv30wm |>
  rename(state_abbrev = STATEAB, state = STATENAME)
    
xsf_TileInv10 <- sfs$TileInv10 %>% select(5:7) %>%
  rename(state_abbrev = State, state = StateName)

xsf_TileOutv10 <- sfs$TileOutv10 %>% select(5:7) %>%
  rename(state_abbrev = State, state = StateName)

xsf_HexCDv30wm |> head()
```

    ## Simple feature collection with 6 features and 4 fields
    ## Geometry type: MULTIPOLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: -13819550 ymin: 3690450 xmax: -12273940 ymax: 5728412
    ## Projected CRS: Mercator_2SP
    ##   GEOID      state state_abbrev district_code                       geometry
    ## 1  0402    Arizona           AZ            02 MULTIPOLYGON (((-12305045 3...
    ## 2  0405    Arizona           AZ            05 MULTIPOLYGON (((-12305045 3...
    ## 3  0606 California           CA            06 MULTIPOLYGON (((-13696151 5...
    ## 4  0608 California           CA            08 MULTIPOLYGON (((-13588284 5...
    ## 5  0610 California           CA            10 MULTIPOLYGON (((-13694006 5...
    ## 6  0614 California           CA            14 MULTIPOLYGON (((-13685745 5...

## FRED Historical Population data

<https://fred.stlouisfed.org/release/tables?rid=118&eid=259194&od=#>

``` r
freds <- lapply(1900:2022, function(x){
  
  ur <- paste0('https://fred.stlouisfed.org/release/tables?rid=118&eid=259194&od=', x, '-01-01#')
  
  ur |>
   xml2::read_html() |>
    rvest::html_node(
      xpath = '//*[@class="table-responsive"]/table/tbody') |>
    rvest::html_table(fill = TRUE) |>
    
    mutate(year = x,
           NAME = X2) |>
    
    rowwise() |>
    mutate(population = ifelse(x < 1950, 
                               gsub('\\.|,', '', X4) |> as.integer(),
                               gsub('\\.|,', '', X3) |> as.integer())) |>
    ungroup() |>
    select(year, NAME, population)
  
})

fred_pop_by_state <- freds |>
  bind_rows() |>
  #filter(!is.na(population)) |>
  left_join(states_full) |>
  select(1,5,2, 3)

fred_pop_by_state |>
  head()|>
  knitr::kable()
```

| year | state_abbrev | NAME       | population |
|-----:|:-------------|:-----------|-----------:|
| 1900 | AL           | Alabama    |    1830000 |
| 1900 | AK           | Alaska     |         NA |
| 1900 | AZ           | Arizona    |     124000 |
| 1900 | AR           | Arkansas   |    1314000 |
| 1900 | CA           | California |    1490000 |
| 1900 | CO           | Colorado   |     543000 |

## Output

``` r
setwd(data_dir)
usethis::use_data(pres_results, overwrite=TRUE)
usethis::use_data(pres_by_county, overwrite=TRUE)
usethis::use_data(pres_by_cd, overwrite=TRUE)
usethis::use_data(pres_by_state, overwrite=TRUE)

usethis::use_data(fred_pop_by_state, overwrite=TRUE)

usethis::use_data(xsf_HexCDv30wm, overwrite=TRUE)
usethis::use_data(xsf_HexSTv30wm, overwrite=TRUE)
usethis::use_data(xsf_TileInv10, overwrite=TRUE)
usethis::use_data(xsf_TileOutv10, overwrite=TRUE)
```
