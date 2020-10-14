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
#options(knitr.table.format = "latex")
```

``` r
library(tidyverse)

res <- Rvoteview::voteview_search("impeachment") %>%
  filter(chamber == 'Senate' & 
           date %in% c('2020-02-05', '1999-02-12')) %>%
  select(id, date, bill_number, text, question, yea, nay) %>%
  arrange(date) 
```

``` r
#library(kableExtra)
res %>%
  select(-id) %>%
  mutate(article = c('-', 'I', 'II', 'II', 'I')) %>%
  arrange(date, article) %>% 
  #knitr::kable() %>%
  kableExtra::kbl() %>%
  kableExtra::kable_styling(font_size = 9)
```

<table class="table" style="font-size: 9px; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
date
</th>
<th style="text-align:left;">
bill\_number
</th>
<th style="text-align:left;">
text
</th>
<th style="text-align:left;">
question
</th>
<th style="text-align:right;">
yea
</th>
<th style="text-align:right;">
nay
</th>
<th style="text-align:left;">
article
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
1999-02-12
</td>
<td style="text-align:left;">
SRES44
</td>
<td style="text-align:left;">
A resolution relating to the censure of William Jefferson Clinton.
</td>
<td style="text-align:left;">
On the Motion
</td>
<td style="text-align:right;">
43
</td>
<td style="text-align:right;">
56
</td>
<td style="text-align:left;">

-   </td>
    </tr>
    <tr>
    <td style="text-align:left;">
    1999-02-12
    </td>
    <td style="text-align:left;">
    HRES611
    </td>
    <td style="text-align:left;">
    A resolution impeaching William Jefferson Clinton, President of the
    United States, for high crimes and misdemeanors.
    </td>
    <td style="text-align:left;">
    Guilty or Not Guilty
    </td>
    <td style="text-align:right;">
    50
    </td>
    <td style="text-align:right;">
    51
    </td>
    <td style="text-align:left;">
    I
    </td>
    </tr>
    <tr>
    <td style="text-align:left;">
    1999-02-12
    </td>
    <td style="text-align:left;">
    HRES611
    </td>
    <td style="text-align:left;">
    A resolution impeaching William Jefferson Clinton, President of the
    United States, for high crimes and misdemeanors.
    </td>
    <td style="text-align:left;">
    Guilty or Not Guilty
    </td>
    <td style="text-align:right;">
    45
    </td>
    <td style="text-align:right;">
    56
    </td>
    <td style="text-align:left;">
    II
    </td>
    </tr>
    <tr>
    <td style="text-align:left;">
    2020-02-05
    </td>
    <td style="text-align:left;">
    HRES755
    </td>
    <td style="text-align:left;">
    A resolution impeaching Donald John Trump, President of the United
    States, for high crimes and misdemeanors.
    </td>
    <td style="text-align:left;">
    Guilty or Not Guilty
    </td>
    <td style="text-align:right;">
    47
    </td>
    <td style="text-align:right;">
    53
    </td>
    <td style="text-align:left;">
    I
    </td>
    </tr>
    <tr>
    <td style="text-align:left;">
    2020-02-05
    </td>
    <td style="text-align:left;">
    HRES755
    </td>
    <td style="text-align:left;">
    A resolution impeaching Donald John Trump, President of the United
    States, for high crimes and misdemeanors.
    </td>
    <td style="text-align:left;">
    Guilty or Not Guilty
    </td>
    <td style="text-align:right;">
    48
    </td>
    <td style="text-align:right;">
    52
    </td>
    <td style="text-align:left;">
    II
    </td>
    </tr>
    </tbody>
    </table>

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
last_sum %>% 
  select(-party_win) %>%
  knitr::kable() %>%
  # kableExtra::kbl() %>% #, caption = "Group Rows"
  # kableExtra::kable_paper("striped", full_width = T) %>%
  kableExtra::pack_rows("Democrat", 1, 7) %>%
  kableExtra::pack_rows("Republican", 8, 10)
```

<table>
<thead>
<tr>
<th style="text-align:right;">
year
</th>
<th style="text-align:left;">
winner
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr grouplength="7">
<td colspan="3" style="border-bottom: 1px solid;">
<strong>Democrat</strong>
</td>
</tr>
<tr>
<td style="text-align:right; padding-left:  2em;" indentlevel="1">
2016
</td>
<td style="text-align:left;">
Hillary Clinton
</td>
<td style="text-align:right;">
21
</td>
</tr>
<tr>
<td style="text-align:right; padding-left:  2em;" indentlevel="1">
1964
</td>
<td style="text-align:left;">
Lyndon B. Johnson
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:right; padding-left:  2em;" indentlevel="1">
1996
</td>
<td style="text-align:left;">
Bill Clinton
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:right; padding-left:  2em;" indentlevel="1">
2012
</td>
<td style="text-align:left;">
Barack Obama
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:right; padding-left:  2em;" indentlevel="1">
1976
</td>
<td style="text-align:left;">
Jimmy Carter
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:right; padding-left:  2em;" indentlevel="1">
1992
</td>
<td style="text-align:left;">
Bill Clinton
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:right; padding-left:  2em;" indentlevel="1">
2008
</td>
<td style="text-align:left;">
Barack Obama
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr grouplength="3">
<td colspan="3" style="border-bottom: 1px solid;">
<strong>Republican</strong>
</td>
</tr>
<tr>
<td style="text-align:right; padding-left:  2em;" indentlevel="1">
2016
</td>
<td style="text-align:left;">
Donald Trump
</td>
<td style="text-align:right;">
30
</td>
</tr>
<tr>
<td style="text-align:right; padding-left:  2em;" indentlevel="1">
1988
</td>
<td style="text-align:left;">
George H. W. Bush
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:right; padding-left:  2em;" indentlevel="1">
1984
</td>
<td style="text-align:left;">
Ronald Reagan
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:right;">
2004
</td>
<td style="text-align:left;">
George W. Bush
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:right;">
1972
</td>
<td style="text-align:left;">
Richard Nixon
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:right;">
2000
</td>
<td style="text-align:left;">
George W. Bush
</td>
<td style="text-align:right;">
1
</td>
</tr>
</tbody>
</table>

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
text1221
</td>
<td style="text-align:left;">
… and judiciary . this seems to be an improvement on \|\| federal
government \|\| beyond what has been made by any other States . …
</td>
</tr>
<tr>
<td style="text-align:left;">
text5721
</td>
<td style="text-align:left;">
… peculiar Satisfaction with which I anticipated the effects of the \|\|
Federal Government \|\| ( and which has been amply verified in the
administration …
</td>
</tr>
<tr>
<td style="text-align:left;">
text1356
</td>
<td style="text-align:left;">
… the present Inhabitants did not choose to Live under the \|\| Federal
Government \|\| they would be permitted to sell their Estates & c …
</td>
</tr>
<tr>
<td style="text-align:left;">
text26670
</td>
<td style="text-align:left;">
… Since notwithstanding the repeated assurances I have received from the
\|\| federal government \|\| , of its determination to exclude those
privateers from any …
</td>
</tr>
<tr>
<td style="text-align:left;">
text19091
</td>
<td style="text-align:left;">
… or improper . It has been said that in the \|\| federal government
\|\| they are unnecessary , because the powers are enumerated , …
</td>
</tr>
<tr>
<td style="text-align:left;">
text10465
</td>
<td style="text-align:left;">
… & to all the Gentlemen of both Houses of the \|\| Federal Government
\|\| from Massachusetts ; and to the honorable Messrs Johnson , …
</td>
</tr>
<tr>
<td style="text-align:left;">
text10298
</td>
<td style="text-align:left;">
… length arrived when there appears a prospect of an efficient \|\|
federal government \|\| , under which , Officers are to be appointed by
…
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
