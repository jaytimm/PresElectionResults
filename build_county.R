library(dplyr)

dataraw_dir <- "/home/jtimm/Dropbox/GitHub/packages/PresElectionResults/data-raw/"
data_dir    <- "/home/jtimm/Dropbox/GitHub/packages/PresElectionResults/data/"

county <- read.delim(paste0(dataraw_dir, 'countypres_2000-2024.tab'), sep = '\t')

pres_by_county <- county |>
  filter(!is.na(candidatevotes),
         !candidate %in% c("TOTAL VOTES CAST")) |>
  group_by(year, state_po, county_name, county_fips, candidate, party) |>
  mutate(has_total = any(mode %in% c("TOTAL", "TOTAL VOTES"))) |>
  filter(!has_total | mode %in% c("TOTAL", "TOTAL VOTES")) |>
  ungroup() |>
  group_by_at(vars(all_of(colnames(county)[c(1:8,10)]))) |>
  summarize(candidatevotes = sum(candidatevotes),
            totalvotes = mean(totalvotes)) |> ungroup() |>
  group_by(year, state_po, county_name, county_fips) |>
  mutate(totalvotes_actual = sum(candidatevotes)) |>
  ungroup() |>
  mutate(per = round(candidatevotes/totalvotes_actual*100, 1)) |>
  mutate(party = tolower(party),
         county_name = stringr::str_to_title(tolower(county_name))) |>
  group_by(year, state_po, county_name, county_fips) |>
  mutate(winner    = candidate[which.max(candidatevotes)],
         winner    = stringr::str_to_title(tolower(winner)),
         party_win = party[which.max(candidatevotes)]) |>
  ungroup() |>
  select(-candidate, -candidatevotes) |>
  filter(party %in% c('republican', 'democrat')) |>
  tidyr::spread(party, per) |>
  rename(state_abbrev = state_po) |>
  mutate(GEOID = stringr::str_pad(county_fips, 5, pad = "0")) |>
  select(1, 3, 4, 12, 8:11)

# Spot check TX 2024
cat("TX 2024 Anderson:\n")
print(pres_by_county |> filter(state_abbrev == "TX", county_name == "Anderson", year == 2024))

cat("\nTX 2024 D+R sum (should be ~100):\n")
pres_by_county |>
  filter(state_abbrev == "TX", year == 2024) |>
  mutate(total = democrat + republican) |>
  summarize(mean_total = mean(total, na.rm=TRUE), min_total = min(total, na.rm=TRUE)) |>
  print()

cat("\nNC 2024 row count (should be 100):\n")
pres_by_county |> filter(state_abbrev == "NC", year == 2024) |> nrow() |> print()

# Save
save(pres_by_county, file = paste0(data_dir, "pres_by_county.rda"))
cat("\nDone -- pres_by_county saved.\n")
