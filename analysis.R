library(tidyverse)

files <- list.files("C:/Users/andre/Downloads/downloads/redistricting_analysis_04_22_26/", pattern = "\\.csv$", full.names = TRUE)

df <- map_dfr(files, \(f) {
  filename <- basename(f) |> str_remove("\\.csv$")
  parts <- str_split_fixed(filename, "_", 3)
  
  read_csv(f, show_col_types = FALSE) |>
    select(ID, Label, starts_with("E_24_PRES")) |>
    mutate(
      state = parts[1],
      map_year = parts[2],
      plan_type = parts[3],
      filename = basename(f)
    )
})

df <- df |>
  mutate(
    pres_margin = (E_24_PRES_Dem - E_24_PRES_Rep) / E_24_PRES_Total,
    dem_pct = E_24_PRES_Dem / E_24_PRES_Total,
    rep_pct = E_24_PRES_Rep / E_24_PRES_Total
  )

comparison <- df |>
  mutate(era = if_else(map_year == "2026", "new", "old")) |>
  select(state, era, ID, Label, pres_margin, dem_pct, rep_pct) |>
  pivot_wider(
    id_cols = c(state, ID),
    names_from = era,
    values_from = c(pres_margin, dem_pct, rep_pct, Label)
  ) |>
  mutate(
    margin_shift = pres_margin_new - pres_margin_old
  ) |>
  arrange(state, ID)

analysis <- comparison |>
  filter(ID != 0) |> 
  mutate(
    party_old = case_when(
      pres_margin_old > 0 ~ "D",
      pres_margin_old < 0 ~ "R",
      TRUE ~ "Tie"
    ),
    party_new = case_when(
      pres_margin_new > 0 ~ "D",
      pres_margin_new < 0 ~ "R",
      TRUE ~ "Tie"
    ),
    flipped = party_old != party_new,
    flip_direction = case_when(
      !flipped ~ "No flip",
      party_old == "R" & party_new == "D" ~ "R→D",
      party_old == "D" & party_new == "R" ~ "D→R"
    ),
    competitive_old = abs(pres_margin_old) < 0.05,
    competitive_new = abs(pres_margin_new) < 0.05,
    # bigger categories to capture tossup likely lean safe
    rating_old = case_when(
      abs(pres_margin_old) < 0.02 ~ "Toss-up",
      abs(pres_margin_old) < 0.05 ~ paste0("Lean ", party_old),
      abs(pres_margin_old) < 0.10 ~ paste0("Likely ", party_old),
      TRUE ~ paste0("Safe ", party_old)
    ),
    rating_new = case_when(
      abs(pres_margin_new) < 0.02 ~ "Toss-up",
      abs(pres_margin_new) < 0.05 ~ paste0("Lean ", party_new),
      abs(pres_margin_new) < 0.10 ~ paste0("Likely ", party_new),
      TRUE ~ paste0("Safe ", party_new)
    )
  )

# --- FLIPS ---
flips <- analysis |> filter(flipped)
cat("=== FLIPPED DISTRICTS ===\n")
flips |>
  select(state, ID, flip_direction, pres_margin_old, pres_margin_new, margin_shift) |>
  arrange(flip_direction, state, ID) |>
  print(n = Inf)

# --- FLIPS SUMMARY BY STATE ---
cat("\n=== FLIPS BY STATE ===\n")
flips |>
  count(state, flip_direction) |>
  pivot_wider(names_from = flip_direction, values_from = n, values_fill = 0) |>
  mutate(net = `R→D` - `D→R`) |>
  print()

# --- COMPETITIVENESS ---
cat("\n=== COMPETITIVENESS SHIFT ===\n")
tibble(
  metric = c("Competitive before", "Competitive after",
             "Became competitive", "Lost competitiveness"),
  n = c(
    sum(analysis$competitive_old),
    sum(analysis$competitive_new),
    sum(!analysis$competitive_old & analysis$competitive_new),
    sum(analysis$competitive_old & !analysis$competitive_new)
  )
) |> print()

# --- OVERALL SEAT COUNT ---
cat("\n=== SEAT COUNTS (by presidential lean) ===\n")
tibble(
  era = c("Old maps", "New maps"),
  D_seats = c(sum(analysis$party_old == "D"), sum(analysis$party_new == "D")),
  R_seats = c(sum(analysis$party_old == "R"), sum(analysis$party_new == "R"))
) |> print()

# --- RATING DISTRIBUTION ---
cat("\n=== RATING DISTRIBUTION ===\n")
bind_rows(
  analysis |> count(rating = rating_old) |> mutate(era = "old"),
  analysis |> count(rating = rating_new) |> mutate(era = "new")
) |>
  pivot_wider(names_from = era, values_from = n, values_fill = 0) |>
  print(n = Inf)

# --- STATE-LEVEL SUMMARY FOR DATAVIZ ---
state_summary <- analysis |>
  group_by(state) |>
  summarize(
    districts = n(),
    D_old = sum(party_old == "D"),
    R_old = sum(party_old == "R"),
    D_new = sum(party_new == "D"),
    R_new = sum(party_new == "R"),
    flips_to_D = sum(flip_direction == "R→D", na.rm = TRUE),
    flips_to_R = sum(flip_direction == "D→R", na.rm = TRUE),
    net_D = flips_to_D - flips_to_R,
    competitive_old = sum(competitive_old),
    competitive_new = sum(competitive_new),
    .groups = "drop"
  )

cat("\n=== STATE SUMMARY ===\n")
print(state_summary)