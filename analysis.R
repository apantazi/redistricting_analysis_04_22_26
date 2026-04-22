library(tidyverse)

files <- list.files("./", pattern = "\\.csv$", full.names = TRUE)

df <- map_dfr(files, \(f) {
  filename <- basename(f) |> str_remove("\\.csv$")
  parts <- str_split_fixed(filename, "_", 3)
  read_csv(f, show_col_types = FALSE) |>
    dplyr::select(ID, Label, starts_with("E_24_PRES"), starts_with("E_20_PRES")) |>
    mutate(
      state = parts[1],
      map_year = parts[2],
      plan_type = parts[3],
      filename = basename(f)
    )
})

df <- df |>
  mutate(
    margin_24 = (E_24_PRES_Dem - E_24_PRES_Rep) / E_24_PRES_Total,
    dem_pct_24 = E_24_PRES_Dem / E_24_PRES_Total,
    rep_pct_24 = E_24_PRES_Rep / E_24_PRES_Total,
    margin_20 = (E_20_PRES_Dem - E_20_PRES_Rep) / E_20_PRES_Total,
    dem_pct_20 = E_20_PRES_Dem / E_20_PRES_Total,
    rep_pct_20 = E_20_PRES_Rep / E_20_PRES_Total,
    margin_avg = (margin_20 + margin_24) / 2
  )

comparison <- df |>
  mutate(era = if_else(map_year == "2026", "new", "old")) |>
  select(state, era, ID, Label, margin_24, margin_20, margin_avg, 
         dem_pct_24, rep_pct_24, dem_pct_20, rep_pct_20) |>
  pivot_wider(
    id_cols = c(state, ID),
    names_from = era,
    values_from = c(margin_24, margin_20, margin_avg, 
                    dem_pct_24, rep_pct_24, dem_pct_20, rep_pct_20, Label)
  ) |>
  mutate(
    shift_24 = margin_24_new - margin_24_old,
    shift_20 = margin_20_new - margin_20_old,
    shift_avg = margin_avg_new - margin_avg_old
  ) |>
  arrange(state, ID)

analysis <- comparison |>
  filter(ID != 0) |>
  mutate(
    party_24_new = if_else(margin_24_new > 0, "D", "R"),
    party_20_new = if_else(margin_20_new > 0, "D", "R"),
    party_avg_new = if_else(margin_avg_new > 0, "D", "R"),
    party_24_old = if_else(margin_24_old > 0, "D", "R"),
    party_20_old = if_else(margin_20_old > 0, "D", "R"),
    party_avg_old = if_else(margin_avg_old > 0, "D", "R"),
    flipped_24 = party_24_old != party_24_new,
    flipped_20 = party_20_old != party_20_new,
    flipped_avg = party_avg_old != party_avg_new,
    flip_dir_24 = case_when(
      !flipped_24 ~ "No flip",
      party_24_old == "R" & party_24_new == "D" ~ "R→D",
      party_24_old == "D" & party_24_new == "R" ~ "D→R"
    ),
    flip_dir_20 = case_when(
      !flipped_20 ~ "No flip",
      party_20_old == "R" & party_20_new == "D" ~ "R→D",
      party_20_old == "D" & party_20_new == "R" ~ "D→R"
    ),
    flip_dir_avg = case_when(
      !flipped_avg ~ "No flip",
      party_avg_old == "R" & party_avg_new == "D" ~ "R→D",
      party_avg_old == "D" & party_avg_new == "R" ~ "D→R"
    ),
    elections_agree_new = party_24_new == party_20_new,
    rating_avg_new = case_when(
      abs(margin_avg_new) < 0.02 ~ "Toss-up",
      abs(margin_avg_new) < 0.05 ~ paste0("Lean ", party_avg_new),
      abs(margin_avg_new) < 0.10 ~ paste0("Likely ", party_avg_new),
      TRUE ~ paste0("Safe ", party_avg_new)
    ),
    rating_24_new = case_when(
      abs(margin_24_new) < 0.02 ~ "Toss-up",
      abs(margin_24_new) < 0.05 ~ paste0("Lean ", party_24_new),
      abs(margin_24_new) < 0.10 ~ paste0("Likely ", party_24_new),
      TRUE ~ paste0("Safe ", party_24_new)
    ),
    rating_20_new = case_when(
      abs(margin_20_new) < 0.02 ~ "Toss-up",
      abs(margin_20_new) < 0.05 ~ paste0("Lean ", party_20_new),
      abs(margin_20_new) < 0.10 ~ paste0("Likely ", party_20_new),
      TRUE ~ paste0("Safe ", party_20_new)
    )
  )

cat("=== FLIPS: 2024 vs 2020 vs AVERAGE ===\n")
tibble(
  metric = c("2024 only", "2020 only", "Average"),
  R_to_D = c(
    sum(analysis$flip_dir_24 == "R→D"),
    sum(analysis$flip_dir_20 == "R→D"),
    sum(analysis$flip_dir_avg == "R→D")
  ),
  D_to_R = c(
    sum(analysis$flip_dir_24 == "D→R"),
    sum(analysis$flip_dir_20 == "D→R"),
    sum(analysis$flip_dir_avg == "D→R")
  )
) |>
  mutate(net_D = R_to_D - D_to_R) |>
  print()

cat("\n=== SPLIT-SIGNAL DISTRICTS (new maps) ===\n")
cat("Districts where 2020 and 2024 point different directions:\n")
analysis |>
  filter(!elections_agree_new) |>
  select(state, ID, margin_20_new, margin_24_new, margin_avg_new, 
         party_20_new, party_24_new, party_avg_new, rating_avg_new) |>
  arrange(abs(margin_avg_new)) |>
  print(n = Inf)

cat("\n=== D→R FLIPS (by 2024) WHERE BIDEN WON IN 2020 ===\n")
analysis |>
  filter(flip_dir_24 == "D→R") |>
  select(state, ID, margin_20_new, margin_24_new, margin_avg_new, 
         party_20_new, rating_avg_new) |>
  print(n = Inf)

cat("\n=== SEAT COUNTS ON NEW MAPS ===\n")
tibble(
  metric = c("2024 Pres", "2020 Pres", "Average"),
  D = c(
    sum(analysis$party_24_new == "D"),
    sum(analysis$party_20_new == "D"),
    sum(analysis$party_avg_new == "D")
  ),
  R = c(
    sum(analysis$party_24_new == "R"),
    sum(analysis$party_20_new == "R"),
    sum(analysis$party_avg_new == "R")
  )
) |> print()

cat("\n=== RATINGS ON NEW MAPS: 2024 vs 2020 vs AVG ===\n")
bind_rows(
  analysis |> count(rating = rating_24_new) |> mutate(metric = "2024"),
  analysis |> count(rating = rating_20_new) |> mutate(metric = "2020"),
  analysis |> count(rating = rating_avg_new) |> mutate(metric = "avg")
) |>
  pivot_wider(names_from = metric, values_from = n, values_fill = 0) |>
  print(n = Inf)

cat("\n=== STATE SUMMARY: NEW MAPS BY METRIC ===\n")
analysis |>
  group_by(state) |>
  summarize(
    districts = n(),
    D_by_24 = sum(party_24_new == "D"),
    D_by_20 = sum(party_20_new == "D"),
    D_by_avg = sum(party_avg_new == "D"),
    split_signal = sum(!elections_agree_new),
    .groups = "drop"
  ) |>
  print()

legislators <- read_csv(
  "https://unitedstates.github.io/congress-legislators/legislators-current.csv",
  show_col_types = FALSE
) |>
  filter(type == "rep") |>
  select(state, district, party, full_name = last_name, first_name) |>
  mutate(
    incumbent_party = case_when(
      party == "Democrat" ~ "D",
      party == "Republican" ~ "R",
      TRUE ~ "I"
    ),
    ID = as.numeric(district)
  )

analysis_with_incumbents <- analysis |>
  left_join(
    legislators |> select(state, ID, incumbent = full_name, first_name, incumbent_party),
    by = c("state", "ID")
  ) |>
  mutate(
    incumbent_name = paste(first_name, incumbent),
    mismatch_24 = incumbent_party != party_24_new,
    mismatch_20 = incumbent_party != party_20_new,
    mismatch_avg = incumbent_party != party_avg_new,
    # How vulnerable are they really?
    vulnerability = case_when(
      mismatch_24 & mismatch_20 ~ "Vulnerable (both elections)",
      mismatch_24 & !mismatch_20 ~ "At risk (2024 only, won 2020)",
      !mismatch_24 & mismatch_20 ~ "At risk (2020 only)",
      TRUE ~ "Aligned"
    )
  )

cat("\n=== INCUMBENT VULNERABILITY (NEW MAPS) ===\n")
analysis_with_incumbents |>
  filter(vulnerability != "Aligned") |>
  select(state, ID, incumbent_name, incumbent_party, 
         margin_24_new, margin_20_new, margin_avg_new, 
         rating_avg_new, vulnerability) |>
  arrange(vulnerability, abs(margin_avg_new)) |>
  print(n = Inf)

cat("\n=== VULNERABILITY SUMMARY ===\n")
analysis_with_incumbents |>
  count(vulnerability) |>
  print()

seat_counts <- analysis |>
  group_by(state) |>
  summarize(
    districts = n(),
    # Old maps
    trump_won_24_old = sum(party_24_old == "R"),
    trump_won_20_old = sum(party_20_old == "R"),
    trump_won_avg_old = sum(party_avg_old == "R"),
    harris_won_24_old = sum(party_24_old == "D"),
    biden_won_20_old = sum(party_20_old == "D"),
    dem_won_avg_old = sum(party_avg_old == "D"),
    # New maps
    trump_won_24_new = sum(party_24_new == "R"),
    trump_won_20_new = sum(party_20_new == "R"),
    trump_won_avg_new = sum(party_avg_new == "R"),
    harris_won_24_new = sum(party_24_new == "D"),
    biden_won_20_new = sum(party_20_new == "D"),
    dem_won_avg_new = sum(party_avg_new == "D"),
    .groups = "drop"
  )

seat_long <- seat_counts |>
  select(state, districts,
         trump_won_24_old, trump_won_24_new,
         trump_won_20_old, trump_won_20_new,
         trump_won_avg_old, trump_won_avg_new) |>
  pivot_longer(
    cols = -c(state, districts),
    names_to = c("metric", "era"),
    names_pattern = "trump_won_(\\w+)_(old|new)",
    values_to = "trump_seats"
  ) |>
  mutate(
    dem_seats = districts - trump_seats,
    metric = case_when(
      metric == "24" ~ "2024 results",
      metric == "20" ~ "2020 results",
      metric == "avg" ~ "Average"
    ),
    era = if_else(era == "old", "Pre-redistricting", "Post-redistricting"),
    metric = factor(metric, levels = c("2020 results", "Average", "2024 results")),
    era = factor(era, levels = c("Pre-redistricting", "Post-redistricting"))
  )

cat("=== TOTAL SEATS ACROSS 7 STATES ===\n")
seat_long |>
  group_by(metric, era) |>
  summarize(
    dem = sum(dem_seats),
    gop = sum(trump_seats),
    .groups = "drop"
  ) |>
  print()