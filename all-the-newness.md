VoteView: LEgislation & roll calls
----------------------------------

Fifteen US Senators served in the impeachment trials of both President
Clinton in 1999 and President Trump in 2020. Here we take a quick look
at how they voted using the `Rvoteview` package.

The table below summarizes impeachment roll calls for the two 1999
articles & the two 2020 articles. There was also a resolution to censure
President Clinton. (Note that Mitt Romney (UT) voted in support of
Article I and against Article II in the 2020 impeachment trial of
President Trump.)

``` r
library(tidyverse)

res <- Rvoteview::voteview_search("impeachment") %>%
  filter(chamber == 'Senate' & 
           date %in% c('2020-02-05', '1999-02-12')) %>%
  select(id, date, bill_number, text, question, yea, nay) %>%
  arrange(date) 
```

``` r
res %>%
  select(-id) %>%
  mutate(article = c('-', 'I', 'II', 'II', 'I')) %>%
  arrange(date, article) %>% 
  #knitr::kable()
  kableExtra::kbl(format = 'latex' ) %>%
  kableExtra::kable_classic(full_width = T) #html_font = "Cambria"
```

Here we look at how the 15 US Senators – members of both the 106th &
116th congresses – voted on Article I from the 2020 trial and Article II
of the 1999 trial.

``` r
votes <- Rvoteview::voteview_download(res$id)
```

################ 

Historical presidential election results
----------------------------------------

``` r
last_dem <- uspols::uspols_wiki_pres %>%
  #filter(party_win == 'democrat') %>% 
  group_by(state_abbrev, party_win) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  mutate(label = paste0(year, ' - ', winner))
```

**Nine US states** have not voted for a Democratic Presidential
candidate since LBJ.

``` r
last_sum <- last_dem %>%
  group_by(year, party_win, winner) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(party_win, desc(n))
```

So, roughly 1/5 of the country (here, of total states) hasn’t voted for
a Democrat since LBJ –

``` r
sometext <- strsplit(paste0(
  "You can even try to make some crazy things like this paragraph. ", 
  "It may seem like a useless feature right now but it's so cool ",
  "and nobody can resist. ;)"
), " ")[[1]]
text_formatted <- paste(
  kableExtra::text_spec(sometext, 
                        color = kableExtra::spec_color(1:length(sometext), end = 0.9),
                        font_size = kableExtra::spec_font_size(1:length(sometext), begin = 5, end = 20)),
  collapse = " ")

# To display the text, type `r text_formatted` outside of the chunk
```

<span
style="     color: rgba(68, 1, 84, 1) !important;font-size: 5px;">You</span>
<span
style="     color: rgba(71, 13, 96, 1) !important;font-size: 6px;">can</span>
<span
style="     color: rgba(72, 24, 106, 1) !important;font-size: 6px;">even</span>
<span
style="     color: rgba(72, 34, 116, 1) !important;font-size: 7px;">try</span>
<span
style="     color: rgba(71, 45, 122, 1) !important;font-size: 7px;">to</span>
<span
style="     color: rgba(69, 54, 129, 1) !important;font-size: 8px;">make</span>
<span
style="     color: rgba(66, 64, 134, 1) !important;font-size: 8px;">some</span>
<span
style="     color: rgba(62, 73, 137, 1) !important;font-size: 9px;">crazy</span>
<span
style="     color: rgba(59, 81, 139, 1) !important;font-size: 9px;">things</span>
<span
style="     color: rgba(55, 90, 140, 1) !important;font-size: 10px;">like</span>
<span
style="     color: rgba(51, 98, 141, 1) !important;font-size: 10px;">this</span>
<span
style="     color: rgba(48, 106, 142, 1) !important;font-size: 11px;">paragraph.</span>
<span
style="     color: rgba(44, 113, 142, 1) !important;font-size: 11px;">It</span>
<span
style="     color: rgba(41, 121, 142, 1) !important;font-size: 12px;">may</span>
<span
style="     color: rgba(38, 129, 142, 1) !important;font-size: 12px;">seem</span>
<span
style="     color: rgba(35, 136, 142, 1) !important;font-size: 13px;">like</span>
<span
style="     color: rgba(33, 144, 141, 1) !important;font-size: 13px;">a</span>
<span
style="     color: rgba(31, 150, 139, 1) !important;font-size: 14px;">useless</span>
<span
style="     color: rgba(31, 158, 137, 1) !important;font-size: 14px;">feature</span>
<span
style="     color: rgba(33, 165, 133, 1) !important;font-size: 15px;">right</span>
<span
style="     color: rgba(38, 173, 129, 1) !important;font-size: 15px;">now</span>
<span
style="     color: rgba(48, 180, 124, 1) !important;font-size: 16px;">but</span>
<span
style="     color: rgba(59, 187, 117, 1) !important;font-size: 16px;">it’s</span>
<span
style="     color: rgba(74, 193, 109, 1) !important;font-size: 17px;">so</span>
<span
style="     color: rgba(90, 200, 100, 1) !important;font-size: 17px;">cool</span>
<span
style="     color: rgba(108, 205, 90, 1) !important;font-size: 18px;">and</span>
<span
style="     color: rgba(127, 211, 78, 1) !important;font-size: 18px;">nobody</span>
<span
style="     color: rgba(145, 215, 66, 1) !important;font-size: 19px;">can</span>
<span
style="     color: rgba(166, 219, 53, 1) !important;font-size: 19px;">resist.</span>
<span
style="     color: rgba(187, 223, 39, 1) !important;font-size: 20px;">;)</span>

``` r
library(sf)

uspols::xsf_TileOutv10 %>% 
  left_join(last_dem %>%
              filter(party_win == 'democrat'), by ='state_abbrev') %>%
  
  ggplot() + 
  geom_sf(aes(fill = label),
          color = 'black' , 
          alpha = .85) + 
  
  ggsflabel::geom_sf_text(data = uspols::xsf_TileInv10,
                          aes(label = state_abbrev), 
                          size = 3,
                          color = 'white') +
  theme_minimal() +
  ggthemes::scale_fill_stata() +
  theme(axis.title.x=element_blank(),  ## add a simple theme --
        axis.text.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        legend.title=element_blank(),
        legend.position = 'right') +
  labs(title = "Last vote for a Democratic Presidential candidate")
```

![](all-the-newness_files/figure-markdown_github/unnamed-chunk-11-1.png)

######### 

The south = Dixie + KE & OK
---------------------------

``` r
library(tidyverse)
south <- c('SC', 'MS', 'FL', 
           'AL', 'GA', 'LA', 'TX', 
           'VA', 'AR', 'NC', 'TE',
           'OK', 'KE')
```

Rvoteview: House composition
----------------------------

*Obviously do this the once* –

``` r
house <- lapply(c(66:116), function (x)
                    Rvoteview::member_search (
                      chamber = 'House', 
                      congress = x)) %>% 
  bind_rows() %>%
    mutate(x = length(unique(district_code))) %>%
    ungroup() %>%
    mutate(district_code = ifelse(x==1, 0, district_code)) %>%
    mutate(district_code = stringr::str_pad (as.numeric(district_code), 2, pad = 0)) 
```

Southern states versus non-Southern states
------------------------------------------

\~75% of GOP House members are from Dixie. In 1960, this % was less than
10.

``` r
house %>%
  mutate(is_south = ifelse(state_abbrev %in% south, 
                           'south', 'non-south')) %>%
  group_by(congress, is_south, party_name) %>%
  summarize (n = n()) %>%
  group_by(congress, is_south) %>%
  
  mutate(per = round(n/sum(n)*100, 1)) %>%
  filter(party_name == 'Republican Party') %>%
  ungroup() %>%
  mutate(year = 1919 + 2*rep(c(1:49), each = 2)) %>%
  
  ggplot() +
  geom_line(aes(x = year, y= per, color = is_south), size = 1.25) +
  ggthemes::scale_color_stata()+
  theme_minimal() +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_continuous(breaks=seq(1919,2019,10)) +
  labs(title="Republican percentage of House seats, since 1919") 
```

![](all-the-newness_files/figure-markdown_github/unnamed-chunk-14-1.png)

A comparison of ideal points for members of the 111th, 113th & 115th
Houses & Presidential vote margins for the 2008, 2012 & 2016 elections,
respectively. Plots re-created, in part, from Barber & McCarty (2015).

Two observations/points to the plot below:

``` r
uspols::uspols_dk_pres %>%
  mutate(margin = republican - democrat,
         congress = case_when(year == 2008 ~ 111, 
                              year == 2012 ~ 113,
                              year == 2016 ~ 115)) %>%
  
  left_join(house %>% filter(congress %in% c('111', '113', '115')), 
            by = c('congress', 'state_abbrev', 'district_code')) %>%
  
  
  ggplot(aes(y = nominate.dim1, 
             x = margin, 
             color = as.factor(party_name)))+ 
  
  geom_point()+ #
  geom_smooth(method="lm", se=T) +
  ggthemes::scale_color_stata()+
  theme_minimal() +
  theme(legend.position = "none", 
        plot.title = element_text(size=12),
        axis.title = element_text())+
  facet_wrap(~year, nrow = 1) +
  xlab('Margins') + ylab('DW-Nominate D1') +
  labs(title="Presidential Election Margins & DW-Nominate scores")
```

![](all-the-newness_files/figure-markdown_github/unnamed-chunk-15-1.png)

################## 

Founding fathers —

1.  A relevant correspondance –

An excerpt from this letter sent by Alexander Hamilton to George
Washington on 1792-08-18, and quoted by Representative Adam Schiff on
2020-1-22 during the impeachment trial of Donald J. Trump. Accessed here
via this Git Hub resource that makes the Founders Online database of
writings/correspondances available as a collection of RDS files.

An excerpt from this letter sent by Alexander Hamilton to George
Washington on 1792-08-18, and quoted by Representative Adam Schiff on
2020-1-22 during the impeachment trial of Donald J. Trump. Accessed here
via this Git Hub resource that makes the Founders Online database of
writings/correspondances available as a collection of RDS files.

``` r
ffc_dir <- '/home/jtimm/jt_work/GitHub/git_projects/FoundersArchiveCorpus/data/'
```

``` r
setwd(ffc_dir)
gfiles <- list.files(path = ffc_dir, 
                     pattern = "rds", 
                     recursive = TRUE) 

ffc_washington <- readRDS(gfiles[8])

ah <- ffc_washington %>% 
  filter(grepl('No popular Government was ever without its Catalines', og_text))

ah1 <- strsplit(ah$og_text, 'absurdity refutes itself.\n')[[1]][2]
ah2 <- strsplit(ah1, 'Objection the 15')[[1]][1]

ah3 <- gsub('When a man unprincipled', '**When a man unprincipled', ah2)
ah3 <- gsub('whirlwind', 'whirlwind**', ah3)
ah3 <- gsub('\n\n', '\n', ah3)
ah3 <- gsub('\n', '\n>\n', ah3)
ah3 <- paste('>', gsub(' *(\n*)*$', '\\1', ah3))
```

1.  search using `quicknews` –

``` r
qorp <- quanteda::corpus(ffc_washington)
#quanteda::docnames(qorp) <- korpus$status_id

quicknews::qnews_search_contexts(qorp = qorp, 
                                        search = "federal government", 
                                        window = 10,
                                        highlight_color = '|') %>%
  #left_join(metas, by = c('docname' = 'link')) %>%
  select(docname, context) %>%
  sample_n(7)  %>%
  knitr::kable(caption = 'Search-in-context: COVID-19 & coronavirus')
```

<table>
<caption>
Search-in-context: COVID-19 & coronavirus
</caption>
<thead>
<tr>
<th style="text-align:left;">
docname
</th>
<th style="text-align:left;">
context
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
text2197
</td>
<td style="text-align:left;">
… with the appointments of Comptroler of the treasury under the \|\|
federal Government \|\| , is now on his way to Phila . in …
</td>
</tr>
<tr>
<td style="text-align:left;">
text1215
</td>
<td style="text-align:left;">
… family . Our Rulers continue as obstinately opposed to the \|\|
Federal Government \|\| as ever , and I have no Idea that they …
</td>
</tr>
<tr>
<td style="text-align:left;">
text354
</td>
<td style="text-align:left;">
… my opinion . As to the general measures of the \|\| federal government
\|\| , when I have seen them attacked artfully and insidiously …
</td>
</tr>
<tr>
<td style="text-align:left;">
text1454
</td>
<td style="text-align:left;">
… the Providence district , could hold that office under the \|\|
Federal Government \|\| ; - and the Governour has great influence among
the …
</td>
</tr>
<tr>
<td style="text-align:left;">
text20968
</td>
<td style="text-align:left;">
… which have led to the formation and establishment of a \|\| Federal
Government \|\| , we esteem your acceptance of the Office of President …
</td>
</tr>
<tr>
<td style="text-align:left;">
text11513
</td>
<td style="text-align:left;">
… before named may be continued after the Organization of the \|\|
Federal Government \|\| within this State in the Offices which they now
respectively …
</td>
</tr>
<tr>
<td style="text-align:left;">
text10560
</td>
<td style="text-align:left;">
… office I am desirous of continuing in it under the \|\| federal
Government \|\| , wishing for the Satisfaction of contributing my part
of …
</td>
</tr>
</tbody>
</table>

References
----------

Barber, M., & McCarty, N. (2015). Causes and consequences of
polarization. *Political negotiation: A handbook*, 37, 39-43.

McCarty, Nolan, Keith T Poole, and Howard Rosenthal. 2016. *Polarized
America: The Dance of Ideology and Unequal Riches*. mit Press.
