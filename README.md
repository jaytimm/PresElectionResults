# PresElectionResults

A data package for US Presidential election results. All build details
are available
[here](https://github.com/jaytimm/PresElectionResults/blob/master/builds.md),
and are fully reproducible.

-   [Installation](#installation)
-   [Presidential Election Results via
    Britannica](#presidential-election-results-via-britannica)
-   [Presidential election results by county (2000-2020) via MIT
    Election Data and Science Lab
    (MEDSL)](#presidential-election-results-by-county-(2000-2020)-via-mit-election-data-and-science-lab-(medsl))
-   [Presidential election results by congressional district (2020) via
    Daily
    Kos](#presidential-election-results-by-congressional-district-(2020)-via-daily-kos)
-   [Presidential election results by state (1864-) via
    Wikipedia](#presidential-election-results-by-state-(1864-)-via-wikipedia)
-   [Equal-area simple feature geometries via Daily
    Kos](#equal-area-simple-feature-geometries-via-daily-kos)
-   [FRED Historical Population Data](#fred-historical-population-data)

## Installation

``` r
library(dplyr)
```

``` r
devtools::install_github("jaytimm/PresElectionResults")
```

## Presidential Election Results via Britannica

``` r
PresElectionResults::pres_results |>
  tail() |> knitr::kable()
```

| year | candidate       | party      | ec_votes | pop_votes | pop_per | ec_total |
|-----:|:----------------|:-----------|---------:|----------:|--------:|---------:|
| 2012 | Barack Obama    | Democratic |      332 |  65446032 |    50.9 |      538 |
| 2012 | Mitt Romney     | Republican |      206 |  60589084 |    47.1 |      538 |
| 2016 | Donald Trump    | Republican |      304 |  62979636 |    46.0 |      538 |
| 2016 | Hillary Clinton | Democratic |      227 |  65844610 |    48.1 |      538 |
| 2020 | Joe Biden       | Democratic |      306 |  81268924 |    51.3 |      538 |
| 2020 | Donald Trump    | Republican |      232 |  74216154 |    46.9 |      538 |

## Presidential election results by county (2000-2020) via MIT Election Data and Science Lab (MEDSL)

``` r
PresElectionResults::pres_by_county |>
  head() |> knitr::kable()
```

| year | state_abbrev | county_name | GEOID | winner         | party_win  | democrat | republican |
|----:|:----------|:----------|:-----|:------------|:---------|-------:|---------:|
| 2000 | AL           | Autauga     | 01001 | George W. Bush | republican |     28.7 |       69.7 |
| 2000 | AL           | Baldwin     | 01003 | George W. Bush | republican |     24.8 |       72.4 |
| 2000 | AL           | Barbour     | 01005 | Al Gore        | democrat   |     49.9 |       49.0 |
| 2000 | AL           | Bibb        | 01007 | George W. Bush | republican |     38.2 |       60.2 |
| 2000 | AL           | Blount      | 01009 | George W. Bush | republican |     27.7 |       70.5 |
| 2000 | AL           | Bullock     | 01011 | Al Gore        | democrat   |     69.2 |       29.2 |

## Presidential election results by congressional district (2020) via Daily Kos

``` r
PresElectionResults::pres_by_cd |>
  head() |> knitr::kable()
```

| icpsr | state_abbrev | district_code | house_rep       | house_rep_party | winner       | party_win  | democrat | republican |
|----:|:--------|:--------|:----------|:----------|:--------|:-------|------:|-------:|
| 22168 | AK           | 00            | Mary Peltola    | democrat        | Donald Trump | republican |     43.0 |       53.1 |
| 22108 | AL           | 01            | Jerry Carl      | republican      | Donald Trump | republican |     35.3 |       63.6 |
| 22140 | AL           | 02            | Barry Moore     | republican      | Donald Trump | republican |     34.8 |       64.2 |
| 20301 | AL           | 03            | Mike Rogers     | republican      | Donald Trump | republican |     32.5 |       66.6 |
| 29701 | AL           | 04            | Robert Aderholt | republican      | Donald Trump | republican |     18.6 |       80.4 |
| 22366 | AL           | 05            | Dale Strong     | republican      | Donald Trump | republican |     35.6 |       62.7 |

## Presidential election results by state (1864-) via Wikipedia

``` r
PresElectionResults::pres_by_state |>
  head() |> knitr::kable()
```

| year | state_abbrev | winner              | party_win  | democrat | other | republican |
|-----:|:------------|:------------------|:----------|--------:|------:|----------:|
| 1864 | CA           | Abraham Lincoln     | republican |    41.40 |    NA |      58.60 |
| 1864 | CT           | Abraham Lincoln     | republican |    48.62 |    NA |      51.38 |
| 1864 | DE           | George B. McClellan | democrat   |    51.80 |    NA |      48.20 |
| 1864 | IA           | Abraham Lincoln     | republican |    36.92 |    NA |      63.08 |
| 1864 | IL           | Abraham Lincoln     | republican |    45.60 |    NA |      54.40 |
| 1864 | IN           | Abraham Lincoln     | republican |    46.50 |    NA |      53.50 |

## Equal-area simple feature geometries via Daily Kos

``` r
# devtools::install_github("yutannihilation/ggsflabel")
library(sf)
library(ggplot2)
PresElectionResults::xsf_HexCDv30wm |> 
  ggplot() + 
  geom_sf(aes(fill = state),
          color = 'white') +

  geom_sf_text(data = PresElectionResults::xsf_HexSTv30wm,
                          aes(label = state_abbrev),
                          size = 3,
                          color='black') +
  
  ## scale_fill_distiller(palette = "RdBu", direction=-1) +
  theme_minimal()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = 'none') +
labs(title = "Equal-area US Congressional District geometry",
     caption = "Source: DailyKos")
```

![](figure-markdown_github/unnamed-chunk-9-1.png)

``` r
library(sf)
library(ggplot2)
PresElectionResults::xsf_TileOutv10 %>% 
  ggplot() + 
  geom_sf(aes(fill = state),
           color = 'white') +
  geom_sf(data = PresElectionResults::xsf_TileInv10, 
          fill = NA, 
          show.legend = F, 
          color="gray", 
          lwd=.5) +
  
  geom_sf_text(data = PresElectionResults::xsf_TileInv10,
                          aes(label = state_abbrev),
                          size = 3,
                          color='black') +
  ##scale_fill_distiller(palette = "RdBu", direction=-1) +
  theme_minimal()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = 'none') +
labs(title = "Equal-area US State geometry",
     caption = "Source: DailyKos")
```

![](figure-markdown_github/unnamed-chunk-10-1.png)

## FRED Historical Population Data

``` r
PresElectionResults::fred_pop_by_state |>
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
