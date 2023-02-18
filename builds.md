# PresElectionResults

``` r
library(dplyr)
```

## Presidential Election Results – Britannica

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
                   'electoral_votes', 
                   'popular_votes', 
                   'popular_percentage')


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
  mutate(popular_votes = as.integer(gsub(',', '', popular_votes)),
         popular_percentage = as.numeric(popular_percentage),
         candidate = gsub('[0-9]', '', candidate) |> trimws(),
         candidate = gsub('George Bush$', 'George H.W. Bush', candidate),
         party = gsub('[0-9]', '', party),
         party = gsub('no distinct party designations|no formally organized parties', 
                                '', 
                                party)) |>
  filter(!party %in% 'not a candidate',
         !candidate %in% 'not voted') |>
  left_join(fns) |>
  mutate(electoral_votes = ifelse(!is.na(z), 
                                  gsub('.$', '', electoral_votes), 
                                  electoral_votes),
         electoral_votes = ifelse(candidate == 'Al Gore', 
                                  gsub('.$', '', electoral_votes), 
                                  electoral_votes),
         
         electoral_votes = as.integer(electoral_votes)) |>
  select(-z)
```

``` r
pres_results |> tail() |> knitr::kable()
```

| year | candidate       | party      | electoral_votes | popular_votes | popular_percentage |
|-----:|:-------------|:---------|-------------:|------------:|----------------:|
| 2012 | Barack Obama    | Democratic |             332 |      65446032 |               50.9 |
| 2012 | Mitt Romney     | Republican |             206 |      60589084 |               47.1 |
| 2016 | Donald Trump    | Republican |             304 |      62979636 |               46.0 |
| 2016 | Hillary Clinton | Democratic |             227 |      65844610 |               48.1 |
| 2020 | Joe Biden       | Democratic |             306 |      81268924 |               51.3 |
| 2020 | Donald Trump    | Republican |             232 |      74216154 |               46.9 |

## Presidential election results by county (2000-2020) – MIT Election Data and Science Lab (MEDSL)

<https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ>

``` r
library(dplyr)
setwd(dataraw_dir)
county <- read.csv('countypres_2000-2020.csv')

pres_by_county <- county |>
  filter(!is.na(candidatevotes)) |>
  filter(mode == 'TOTAL') |>
  select(-state, -office, -version, -totalvotes) |>
  mutate(#party = tolower(party),
         party = gsub('DEMOCRAT', 'DEMOCRATIC', party),
         party = stringr::str_to_title(tolower(party)),
         county_name = stringr::str_to_title(tolower(county_name))) |>
  
  group_by(year, state_po, county_name, county_fips) |>
  mutate(per = round(candidatevotes/sum(candidatevotes)*100, 1)) |>

  mutate(winner =  candidate[which.max(candidatevotes)],
         winner = stringr::str_to_title(tolower(winner)),
         party_win = party[which.max(candidatevotes)]) |>
  ungroup() |>
  select(-candidate, -candidatevotes, -mode) |>
  filter(party %in% c('Republican', 'Democratic')) |>
  tidyr::spread(party, per) |>
  rename(state_abbrev = state_po)

colnames(pres_by_county) <- colnames(pres_by_county) |> tolower() 

pres_by_county |> slice(1:5) |> knitr::kable()
```

| year | state_abbrev | county_name | county_fips | winner         | party_win  | democratic | republican |
|----:|:----------|:---------|---------:|:-----------|:--------|--------:|--------:|
| 2000 | AL           | Autauga     |        1001 | George W. Bush | Republican |       28.7 |       69.7 |
| 2000 | AL           | Baldwin     |        1003 | George W. Bush | Republican |       24.8 |       72.4 |
| 2000 | AL           | Barbour     |        1005 | Al Gore        | Democratic |       49.9 |       49.0 |
| 2000 | AL           | Bibb        |        1007 | George W. Bush | Republican |       38.2 |       60.2 |
| 2000 | AL           | Blount      |        1009 | George W. Bush | Republican |       27.7 |       70.5 |

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
                            'Democratic', 
                            'Republican'),
         house_rep_party = ifelse(party == '\\(D\\)', 'Democratic', 'Republican')) |> 
  rename(democrat = biden,
         republican = trump,
         house_rep = incumbent) |>
  select(district, house_rep, house_rep_party,
         winner, party_win, democrat, republican)
  
pres_by_cd |> slice(1:5) |> knitr::kable()
```

| district | house_rep       | house_rep_party | winner       | party_win  | democrat | republican |
|:-------|:-------------|:-------------|:----------|:---------|-------:|---------:|
| AK-AL    | Mary Peltola    | Republican      | Donald Trump | Republican |     43.0 |       53.1 |
| AL-01    | Jerry Carl      | Republican      | Donald Trump | Republican |     35.3 |       63.6 |
| AL-02    | Barry Moore     | Republican      | Donald Trump | Republican |     34.8 |       64.2 |
| AL-03    | Mike Rogers     | Republican      | Donald Trump | Republican |     32.5 |       66.6 |
| AL-04    | Robert Aderholt | Republican      | Donald Trump | Republican |     18.6 |       80.4 |

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
w6 <- c('Arkansas')

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
  left_join(pres_results |> select(year:party), by = c('brit' = 'candidate',
                                                'year' = 'year')) |>
  mutate(party = ifelse(candidate %in% c('Al Smith', 'Adlai Stevenson II'),
                        'Democratic', party)) |>
  
  rename(NAME = state_name) |>
  left_join(states) |>
  select(year, state_abbrev, brit, party, votes:vote_share) |>
  rename(candidate = brit) |>
  filter(year >= 1864, !is.na(vote_share)) |>
  select(-votes) |>
  unique() |>
  mutate(party = ifelse(!party %in% c('Democratic', 'Republican'), 
                        'Other', party)) |>
  
  group_by(year, state_abbrev, party) |>
  filter(vote_share == max(vote_share)) |>
  ungroup() |>
  
  group_by(state_abbrev, year) |>   
  mutate(winner = candidate[which.max(vote_share)],
         #party = tolower(party),
         party_win = party[which.max(vote_share)]) |>
  ungroup() |>
  
  select(-candidate) |>
  
  unique() |>
  tidyr::spread(party, vote_share)

colnames(pres_by_state) <- colnames(pres_by_state) |> tolower() 
```

``` r
pres_by_state |> head() |> knitr::kable()
```

| year | state_abbrev | winner              | party_win  | democratic | other | republican |
|-----:|:-----------|:-----------------|:----------|----------:|-----:|----------:|
| 1864 | CA           | Abraham Lincoln     | Republican |      41.40 |    NA |      58.60 |
| 1864 | CT           | Abraham Lincoln     | Republican |      48.62 |    NA |      51.38 |
| 1864 | DE           | George B. McClellan | Democratic |      51.80 |    NA |      48.20 |
| 1864 | IA           | Abraham Lincoln     | Republican |      36.92 |    NA |      63.08 |
| 1864 | IL           | Abraham Lincoln     | Republican |      45.60 |    NA |      54.40 |
| 1864 | IN           | Abraham Lincoln     | Republican |      46.50 |    NA |      53.50 |

## Equal-area simple feature geometries – Daily Kos

<https://docs.google.com/spreadsheets/d/1LrBXlqrtSZwyYOkpEEXFwQggvtR0bHHTxs9kq4kjOjw/edit#gid=0>

``` r
fnames <- c('HexCDv30wm',
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

## Output

``` r
setwd(data_dir)
usethis::use_data(pres_results, overwrite=TRUE)
usethis::use_data(pres_by_county, overwrite=TRUE)
usethis::use_data(pres_by_cd, overwrite=TRUE)
usethis::use_data(pres_by_state, overwrite=TRUE)

usethis::use_data(xsf_HexCDv30wm, overwrite=TRUE)
usethis::use_data(xsf_TileInv10, overwrite=TRUE)
usethis::use_data(xsf_TileOutv10, overwrite=TRUE)
```
