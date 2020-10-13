
library(tidyverse)


# #DailyKos equal-area shapefiles
# base <- 'https://drive.google.com/uc?authuser=0&id='
#
# #Tile map
# uspols_state_outer <- paste0(base, '0B2X3Bx1aCHsJdGF4ZWRTQmVyV2s&export=download/TileOutv10.zip')
# uspols_state_inner <- paste0(base, '0B2X3Bx1aCHsJR1c0SzNyWlAtZjA&export=download/TileInv10.zip')
#
#
# #Simple function for online shapefile download/unzip/to sf
# get_url_shape <- function (url) {
#   temp <- tempdir()
#   zip_name <- paste0(temp, '\\', basename(url))
#   download.file(url, zip_name,
#                 quiet = TRUE)
#   unzip(zip_name, exdir = temp)
#   x <- sf::st_read(dsn = gsub('\\.zip', '', zip_name),
#                    layer = gsub('\\.zip','', basename(url)),
#                    quiet = TRUE)
#   unlink(temp)
#   x}


sf_dir <- '/home/jtimm/jt_work/GitHub/packages/uspols/data-raw/'

fnames <- c('HexCDv21', 'HexSTv20',
            'TileInv10', 'TileOutv10')

# x <- 'TileOutv10'

sfs <- lapply(fnames, function(x) {

  sf::st_read(dsn = paste0(sf_dir, x),
                     layer = x,
                     quiet = TRUE) })

names(sfs) <- fnames

sf_HexCDv21 <- sfs$HexCDv21 %>%
  rename(state_abbrev = STATEAB,
         state = STATENAME) %>%
  mutate(district_code = gsub('[A-Z][A-Z]', 0, CDLABEL),
         district_code = stringr::str_pad (district_code, 2, pad = 0)) %>%
  select(GEOID, state, state_abbrev, district_code)

sf_HexSTv20 <- sfs$HexSTv20 %>%
  rename(state_abbrev = STATEAB, state = STATENAME)

sf_TileInv10 <- sfs$TileInv10 %>% select(5:7) %>%
  rename(state_abbrev = State, state = StateName)

sf_TileOutv10 <- sfs$TileOutv10 %>% select(5:7) %>%
  rename(state_abbrev = State, state = StateName)

setwd('/home/jtimm/jt_work/GitHub/packages/uspols')
usethis::use_data(sf_HexCDv21, overwrite=TRUE)
usethis::use_data(sf_HexSTv20, overwrite=TRUE)
usethis::use_data(sf_TileInv10, overwrite=TRUE)
usethis::use_data(sf_TileOutv10, overwrite=TRUE)


