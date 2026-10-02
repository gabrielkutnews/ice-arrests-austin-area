library(tidyverse)
library(janitor)
library(lubridate)
library(readxl)
library(slider)

# Data Download and Cleaning=============

db_national<- read_csv("data/national-joined.csv")

db_texas <- read_csv("data/texas-joined.csv")

db_austin <- read_csv("data/joined-arrests-detention-stays-central-texas.csv")

glimpse(db_austin)


austin_area_city_values <- c(
  # Austin
  "Austin", "Astin", "Austi N", "Austin Texas", "Ausitn", "Austn", "Austi",
  
  # Bastrop
  "Bastrop", "Basrop", "Bastop", "Bastrio", "Batrop", "Fci Bastrop",
  
  # Blanco / Burnet and communities inside the western edge
  "Blanco", "Burnet", "Burent", "Brunet", "Burnett", "Bertram",
  "Granite Shoals", "Highland Haven", "Johnson City", "Marble Falls",
  "Round Mountain", "Spicewood",
  
  # Buda / Kyle / San Marcos (inclusive southern edge)
  "Buda", "Buba", "Near Buda", "Kyle", "Kyel", "San Marcos", "Sam Marcos",
  "San Marco", "Maxwell",
  
  # Cedar Park / Leander / Liberty Hill
  "Cedar Park", "Cedar Parl", "Leander", "Liberty Hill", "Liberty Hil",
  "Liberty Hills", "Andice",
  
  # Del Valle
  "Del Valle", "Del Vale", "De Valle", "Del Velle", "Del Ville",
  "Dell Valle", "Del Valley", "Del Vall", "Del Vallle", "Del Vallw",
  "Del Valletx",
  
  # Dripping Springs / Lakeway / Bee Cave
  "Dripping Springs", "Dripping Sprsings", "Driftwood", "Lakeway",
  "Bee Cave",
  
  # Elgin
  "Elgin", "Eglin",
  
  # Florence / Jarrell (inclusive northern edge)
  "Florence", "Flovence", "Florance", "Jarrell", "Jarell", "Jerrell",
  "Jarrel", "Jerell", "Jarrellï¿½",
  
  # Georgetown
  "Georgetown", "Gerogetown", "Gorgetown", "Georgetwon", "Georgertown",
  "Gerorgetown", "Georgtown", "Goergetown", "Grorgetown",
  
  # Lockhart (inclusive southern edge)
  "Lockhart", "Lockhard", "Lockart",
  
  # Manor / Pflugerville / Round Rock / Hutto
  "Manor", "Pflugerville", "Pfluggerville", "Pfgullerville", "Plugerville",
  "Pfluger", "Round Rock", "Hutto",
  
  # Taylor and nearby communities
  "Taylor", "Tayor", "Coupland", "Granger", "Thrall",
  
  # Eastern and southeastern portion, through Rockdale and La Grange
  "Rockdale", "La Grange", "Cedar Creek", "Ceder Creek", "Dale", "McDade",
  "Paige", "Red Rock", "Smithville", "Webberville",
  
  # Other communities inside the stated boundary
  "Lago Vista", "Manchaca", "Mustang Ridge", "Uhland"
)


db_austin <- db_austin |>
  dplyr::filter(
    trimws(as.character(apprehension_city_common_case)) %in%
      austin_area_city_values
  )

glimpse(db_austin)

db_austin <- db_austin |> mutate(apprehension_date_time = as.Date(apprehension_date_time))
db_texas <- db_texas |> mutate(apprehension_date_time = as.Date(apprehension_date_time))
db_national <- db_national |> mutate(apprehension_date_time = as.Date(apprehension_date_time))

glimpse(db_austin)

# Analysis=======


db_austin_monthly <- db_austin |>
  mutate(month = floor_date(apprehension_date_time, unit = "month")
  ) |>
  count(month, name = "apprehensions") |> 
  filter(month != "2026-08-01")

db_texas_monthly <- db_texas |>
  mutate(month = floor_date(apprehension_date_time, unit = "month")
  ) |>
  count(month, name = "apprehensions") |> 
  filter(month != "2026-08-01")

db_national_monthly <- db_national |>
  mutate(month = floor_date(apprehension_date_time, unit = "month")
  ) |>
  count(month, name = "apprehensions") |> 
  filter(month != "2026-08-01")

pct_change <- function(data) {
  first_value <- data |>
    filter(month == as.Date("2022-10-01")) |>
    pull(apprehensions)
  
  last_value <- data |>
    filter(month == as.Date("2026-07-01")) |>
    pull(apprehensions)
  
  (last_value - first_value) / first_value * 100
}

pct_change(db_austin_monthly)
# pct_change(db_austin_monthly)
# [1] 857.4257
pct_change(db_texas_monthly)
# [1] 206.3123
pct_change(db_national_monthly)
# [1] 195.4975


glimpse(db_austin_monthly)

ggplot(db_austin_monthly, aes(x = month, y = apprehensions)) +
  geom_col(fill = "steelblue") +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%b %Y"
  ) +
  labs(
    title = "Apprehensions by Month",
    x = "Month",
    y = "Number of apprehensions"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggplot(db_texas_monthly, aes(x = month, y = apprehensions)) +
  geom_col(fill = "steelblue") +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%b %Y"
  ) +
  labs(
    title = "Apprehensions by Month",
    x = "Month",
    y = "Number of apprehensions"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


write_csv(db_austin_monthly, "data/db_austin_monthly.csv")

# Seven Day

daily_seven_day_average <- function(data) {
  data |>
    filter(
      !is.na(apprehension_date_time),
      apprehension_date_time < as.Date("2026-08-01")
    ) |>
    count(
      apprehension_date_time,
      name = "daily_apprehensions"
    ) |>
    complete(
      apprehension_date_time = seq(
        min(apprehension_date_time),
        max(apprehension_date_time),
        by = "day"
      ),
      fill = list(daily_apprehensions = 0)
    ) |>
    arrange(apprehension_date_time) |>
    mutate(
      rolling_7_day_average = slide_dbl(
        daily_apprehensions,
        mean,
        .before = 6,
        .complete = TRUE
      )
    )
}

# Calculate daily arrests and 7-day averages
db_austin_daily <- daily_seven_day_average(db_austin)
write_csv(db_austin_daily, "data/db_austin_daily.csv")
db_texas_daily <- daily_seven_day_average(db_texas)
db_national_daily <- daily_seven_day_average(db_national)

ggplot(
  db_austin_daily,
  aes(
    x = apprehension_date_time,
    y = rolling_7_day_average
  )
) +
  geom_line(
    color = "steelblue",
    linewidth = 1
  ) +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%b %Y"
  ) +
  labs(
    title = "Seven-Day Rolling Average of Daily Apprehensions",
    x = "Date",
    y = "Average daily apprehensions"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#6-month data comparison

# Function to create monthly totals for January through July
make_jan_july_data <- function(data, geography) {
  
  data |>
    filter(
      !is.na(apprehension_date_time),
      month(apprehension_date_time) >= 1,
      month(apprehension_date_time) <= 7,
      year(apprehension_date_time) %in% c(2025, 2026)
    ) |>
    mutate(
      year = year(apprehension_date_time),
      month_number = month(apprehension_date_time),
      month = month(apprehension_date_time, label = TRUE, abbr = TRUE)
    ) |>
    count(
      geography = geography,
      year,
      month_number,
      month,
      name = "apprehensions"
    ) |>
    complete(
      geography,
      year = c(2025, 2026),
      month_number = 1:7,
      fill = list(apprehensions = 0)
    ) |>
    mutate(
      month = factor(
        month.abb[month_number],
        levels = month.abb[1:7]
      ),
      series = paste(geography, year)
    ) |>
    arrange(geography, year, month_number)
}

# Create monthly data for each geographic area
austin_jan_july <- make_jan_july_data(
  db_austin,
  "Austin area"
)

texas_jan_july <- make_jan_july_data(
  db_texas,
  "Texas"
)

national_jan_july <- make_jan_july_data(
  db_national,
  "National"
)

# Combine all three areas
jan_july_comparison <- bind_rows(
  austin_jan_july,
  texas_jan_july,
  national_jan_july
) |>
  mutate(
    month = as.character(month)
  ) |>
  arrange(month_number, geography, year)

# Export for Datawrapper
write_csv(
  jan_july_comparison,
  "data/jan_july_2025_2026_comparison.csv"
)

# Looking at the change in type of ppl arrested======

central_texas_crime <- db_austin |>
  mutate(
    apprehension_date_time = as.Date(apprehension_date_time),
    month = floor_date(apprehension_date_time, "month"),
    city_clean = trimws(as.character(apprehension_city_common_case)),
    criminality = recode(
      apprehension_criminality,
      "1 Convicted Criminal" = "Convicted criminal",
      "2 Pending Criminal Charges" = "Pending criminal charges",
      "3 Other Immigration Violator" = "Other immigration violator",
      .default = "Unknown",
      .missing = "Unknown"
    )
  ) |>
  filter(
    city_clean %in% austin_area_city_values,
    month >= as.Date("2022-10-01"),
    month <= as.Date("2026-07-01")
  )

criminality_monthly_detail <- central_texas_crime |>
  count(month, criminality, name = "apprehensions") |>
  group_by(month) |>
  mutate(
    total_apprehensions = sum(apprehensions),
    share = apprehensions / total_apprehensions,
    share_percent = share * 100
  ) |>
  ungroup() |>
  arrange(month, criminality)

write_csv(
  criminality_monthly_detail,
  "outputs/story_angles/datawrapper/datawrapper_central_texas_criminality_monthly_detail.csv"
)

# LOCAL ARRESTS====

# Deportation Data Project caution:
# These fields do not capture the full scope of local/state collaboration with ICE.
# Treat these as indicators of possible local/state involvement, not a complete count.

glimpse(db_austin)

local_landmark_terms <- paste(
  c(
    "police", "sheriff", "jail", "county jail", "detention center",
    "detention facility", "correctional", "corrections", "prison",
    "law enforcement", "public safety", "criminal justice",
    "\\bpd\\b", "\\bso\\b"
  ),
  collapse = "|"
)

local_arrests_classified <- db_austin |>
  mutate(
    month = floor_date(apprehension_date_time, "month"),
    apprehension_method_clean = str_squish(as.character(apprehension_method)),
    apprehension_method_simple_clean = str_squish(as.character(apprehension_method_simple)),
    event_landmark_clean = str_squish(as.character(event_landmark)),
    formal_local_collaboration_method = str_detect(
      apprehension_method_clean,
      regex("287\\(g\\)|CAP.*state|CAP.*local|state/local|local incarceration", ignore_case = TRUE)
    ),
    custodial_arrest_method = str_detect(
      apprehension_method_simple_clean,
      regex("^custodial arrest$", ignore_case = TRUE)
    ),
    local_landmark_indicator = str_detect(
      event_landmark_clean,
      regex(local_landmark_terms, ignore_case = TRUE)
    ),
    possible_local_or_state_involvement = case_when(
      formal_local_collaboration_method ~ TRUE,
      custodial_arrest_method ~ TRUE,
      local_landmark_indicator ~ TRUE,
      TRUE ~ FALSE
    ),
    local_arrest_category = case_when(
      formal_local_collaboration_method ~ "Formal local/state collaboration method",
      custodial_arrest_method ~ "Custodial arrest",
      local_landmark_indicator ~ "Local law enforcement/jail landmark",
      TRUE ~ "No local/state indicator in these fields"
    )
  ) |>
  filter(
    !is.na(apprehension_date_time),
    month >= as.Date("2022-10-01"),
    month <= as.Date("2026-07-01")
  )

local_arrests_classified |> 
  count(apprehension_method, sort = TRUE) %>%
  mutate(percent = n / sum(n) * 100)

# Overall comparison: arrests with a possible local/state indicator vs. all other ICE arrests.
local_arrests_summary <- local_arrests_classified |>
  count(
    possible_local_or_state_involvement,
    name = "apprehensions"
  ) |>
  mutate(
    category = if_else(
      possible_local_or_state_involvement,
      "Possible local/state involvement",
      "No local/state indicator in selected fields"
    ),
    share_of_apprehensions = apprehensions / sum(apprehensions)
  ) |>
  select(category, apprehensions, share_of_apprehensions)

# Mutually exclusive categories, useful for explaining what is driving the local/state proxy.
local_arrests_category_summary <- local_arrests_classified |>
  count(local_arrest_category, name = "apprehensions") |>
  mutate(
    share_of_apprehensions = apprehensions / sum(apprehensions)
  ) |>
  arrange(desc(apprehensions))

# Monthly trend, useful for a Datawrapper stacked bar or line chart.
local_arrests_monthly <- local_arrests_classified |>
  count(month, local_arrest_category, name = "apprehensions") |>
  group_by(month) |>
  mutate(
    monthly_total_apprehensions = sum(apprehensions),
    share_of_month = apprehensions / monthly_total_apprehensions
  ) |>
  ungroup() |>
  arrange(month, local_arrest_category)

# Original ICE method values, useful for checking exactly which method labels are included.
local_arrests_method_check <- local_arrests_classified |>
  count(
    apprehension_method,
    apprehension_method_simple,
    local_arrest_category,
    name = "apprehensions"
  ) |>
  arrange(desc(apprehensions))

# Landmark examples to review manually before using this as a reporting finding.
local_arrests_landmark_examples <- local_arrests_classified |>
  filter(local_landmark_indicator) |>
  count(event_landmark, apprehension_city_common_case, name = "apprehensions") |>
  arrange(desc(apprehensions))

# Nationality breakdown for a Datawrapper pie chart.
local_arrests_nationality_pie <- local_arrests_classified |>
  mutate(
    nationality = str_squish(as.character(citizenship_country)),
    nationality = if_else(
      is.na(nationality) | nationality == "" | str_to_upper(nationality) == "NA",
      "Unknown",
      nationality
    )
  ) |>
  count(nationality, name = "apprehensions") |>
  arrange(desc(apprehensions)) |>
  mutate(
    nationality = if_else(row_number() <= 10, nationality, "Other")
  ) |>
  group_by(nationality) |>
  summarise(apprehensions = sum(apprehensions), .groups = "drop") |>
  mutate(
    share = apprehensions / sum(apprehensions),
    percent = share * 100
  ) |>
  arrange(desc(apprehensions))

# Busiest individual arrest days.
top_twenty <- local_arrests_classified |>
  count(apprehension_date_time, name = "apprehensions") |>
  arrange(desc(apprehensions), apprehension_date_time) |>
  mutate(rank = min_rank(desc(apprehensions))) |>
  select(rank, date = apprehension_date_time, apprehensions) |> 
  slice_head(n = 20)

write_csv(top_twenty, "data/top_twenty.csv")
  

local_arrests_summary
local_arrests_category_summary

write_csv(local_arrests_classified, "data/local_arrests_classified.csv")
write_csv(
  local_arrests_nationality_pie,
  "outputs/story_angles/datawrapper/datawrapper_local_arrests_nationality_pie.csv"
)
write_csv(
  highest_arrest_days,
  "outputs/story_angles/tables/highest_arrest_days.csv"
)

# Detention stays=======

glimpse(db_austin)

db_austin |> filter(
  has_detention_stay == TRUE,
  !is.na(stay_length_days),
  stay_length_days >= 0
) |>
  summarise(
    detention_records = n(),
    shortest_days = min(stay_length_days),
    median_days = median(stay_length_days),
    longest_days = max(stay_length_days)
  )

view(db_austin)

# Other Immigration Violator pct?

imm_vio <- db_austin |>
  mutate(
    month = lubridate::floor_date(as.Date(apprehension_date_time), "month"),
    is_other_immigration_violator = stringr::str_detect(
      apprehension_criminality,
      stringr::regex("other immigration violator", ignore_case = TRUE)
    )
  ) |>
  group_by(month) |>
  summarise(
    total_arrests = n(),
    other_immigration_violator = sum(
      is_other_immigration_violator,
      na.rm = TRUE
    ),
    pct_share = other_immigration_violator / total_arrests * 100,
    .groups = "drop"
  ) |>
  arrange(month)

# Type of arrests (287(g))?=======

arrest_method_jan_july <- db_austin |>
  mutate(
    date = as.Date(apprehension_date_time),
    year = lubridate::year(date),
    month = lubridate::month(date),
    method = dplyr::coalesce(
      as.character(apprehension_method_simple),
      as.character(apprehension_method)
    )
  ) |>
  filter(month <= 7) |>
  group_by(year) |>
  summarise(
    total_arrests = n(),
    at_large_arrests = sum(
      stringr::str_detect(
        method,
        stringr::regex("at.?large", ignore_case = TRUE)
      ),
      na.rm = TRUE
    ),
    `287g_arrests` = sum(
      stringr::str_detect(
        method,
        stringr::regex("287\\s*\\(g\\)", ignore_case = TRUE)
      ),
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  mutate(
    at_large_pct = at_large_arrests / total_arrests * 100,
    `287g_pct` = `287g_arrests` / total_arrests * 100
  )

arrest_method_jan_july



