library(auk); library(dplyr); library(ggplot2); library(gridExtra)
library(lubridate); library(readr); library(sf)

f_sed <- "C:/Users/User/Desktop/eBird/ebd_US-NC_paibun_smp_relOct-2023/ebd_US-NC_paibun_smp_relOct-2023_sampling.txt"
checklists <- read_sampling(f_sed)
glimpse(checklists)


f_ebd <- "C:/Users/User/Desktop/eBird/ebd_US-NC_paibun_smp_relOct-2023/ebd_US-NC_paibun_smp_relOct-2023.txt"
observations <- read_ebd(f_ebd)
glimpse(observations)

###########

# filter the checklist data
checklists <- checklists %>% 
  filter(all_species_reported == TRUE, # defaults to TRUE if you don't put == TRUE
         protocol_type %in% c("Stationary", "Traveling"), # the %in% is kind of complicated... but this just keeps those two survey types.
         # re: %in% https://stackoverflow.com/questions/25647470/filter-multiple-values-on-a-string-column-in-dplyr
         year(observation_date) >= 2014, year(observation_date) <= 2023, 
         month(observation_date) >= 5, month(observation_date) <= 7)

# filter the observation data
observations <- observations %>% 
  filter(all_species_reported == TRUE, 
         protocol_type %in% c("Stationary", "Traveling"),
         year(observation_date) >= 2014, year(observation_date) <= 2023, 
         month(observation_date) >= 5, month(observation_date) <= 7)

#############

zf <- auk_zerofill(observations, checklists, collapse = TRUE)

#############

# function to convert time observation to hours since midnight
time_to_decimal <- function(x) {
  x <- hms(x, quiet = TRUE)
  hour(x) + minute(x) / 60 + second(x) / 3600
}

# clean up variables
zf <- zf %>% 
  mutate(
    # convert X to NA
    observation_count = if_else(observation_count == "X", 
                                NA_character_, observation_count),
    observation_count = as.integer(observation_count),
    # effort_distance_km to 0 for non-travelling counts
    effort_distance_km = if_else(protocol_type != "Traveling", 
                                 0, effort_distance_km),
    # convert duration to hours
    effort_hours = duration_minutes / 60,
    # speed km/h
    effort_speed_kmph = effort_distance_km / effort_hours,
    # convert time to decimal hours since midnight
    hours_of_day = time_to_decimal(time_observations_started),
    # split date into year and day of year
    year = year(observation_date),
    day_of_year = yday(observation_date)
  )

################ more recommended filtering

# additional filtering
zf_filtered <- zf %>% 
  filter(effort_hours <= 6,
         effort_distance_km <= 10,
         effort_speed_kmph <= 100,
         number_observers <= 10)

################ Splitting into 80% training, 20% testing
zf_filtered$type <- if_else(runif(nrow(zf_filtered)) <= 0.8, "train", "test")
# confirm the proportion in each set is correct
table(zf_filtered$type) / nrow(zf_filtered)

################ Remove redundant columns from database (e.g., state codes AND state names)
checklists <- zf_filtered |> 
  select(checklist_id, observer_id, type,
         observation_count, species_observed, 
         state_code, locality_id, latitude, longitude,
         protocol_type, all_species_reported,
         observation_date, year, day_of_year,
         hours_of_day, 
         effort_hours, effort_distance_km, effort_speed_kmph,
         number_observers)

############## save the resulting zero-filled observations for use in later chapters
write_csv(checklists, "data/checklists-zf_pabu_nc.csv", na = "")


############# Download raw data
# https://github.com/ebird/ebird-best-practices/blob/main/data-raw/ebird-best-practices-data.zip

############# mapping

# load gis data
ne_land <- read_sf("data/ebird-best-practices-data/data/gis-data.gpkg", "ne_land") |> 
  st_geometry()
ne_country_lines <- read_sf("data/ebird-best-practices-data/data/gis-data.gpkg", "ne_country_lines") |> 
  st_geometry()
ne_state_lines <- read_sf("data/ebird-best-practices-data/data/gis-data.gpkg", "ne_state_lines") |> 
  st_geometry()
study_region <- read_sf("data/ebird-best-practices-data/data/gis-data.gpkg", "ne_states") |> 
  filter(state_code == "US-NC") |> 
  st_geometry()

# prepare ebird data for mapping
checklists_sf <- checklists |> 
  # convert to spatial points
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
  select(species_observed)

# map
par(mar = c(0.25, 0.25, 4, 0.25))
# set up plot area
plot(st_geometry(checklists_sf), 
     main = "PABU eBird Observations\n",
     col = NA, border = NA)
# contextual gis data
plot(ne_land, col = "#cfcfcf", border = "#888888", lwd = 0.5, add = TRUE)
plot(study_region, col = "#e6e6e6", border = NA, add = TRUE)
plot(ne_state_lines, col = "#ffffff", lwd = 0.75, add = TRUE)
plot(ne_country_lines, col = "#ffffff", lwd = 1.5, add = TRUE)
# ebird observations
# not observed
plot(filter(checklists_sf, !species_observed),
     pch = 19, cex = 0.1, col = alpha("#555555", 0.25),
     add = TRUE)
# observed
plot(filter(checklists_sf, species_observed),
     pch = 19, cex = 0.3, col = alpha("#4daf4a", 1),
     add = TRUE)
# legend
legend("bottomright", bty = "n",
       col = c("#555555", "#4daf4a"),
       legend = c("eBird checklist", "Painted Bunting sighting"),
       pch = 19)
box()
