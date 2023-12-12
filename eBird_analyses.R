library(remotes)
remotes::install_github("ebird/ebird-best-practices")

#############

library(dplyr); library(rnaturalearth); library(sf)
gpkg_file <- "data/gis-data.gpkg"
dir.create(dirname(gpkg_file), showWarnings = FALSE, recursive = TRUE)

# political boundaries
# land border with lakes removed
ne_land <- ne_download(scale = 50, category = "cultural",
                       type = "admin_0_countries_lakes",
                       returnclass = "sf") %>%
  filter(CONTINENT == "North America") %>%
  st_set_precision(1e6) %>%
  st_union()
# state boundaries for united states
ne_states <- ne_download(scale = 50, category = "cultural",
                         type = "admin_1_states_provinces",
                         returnclass = "sf") %>% 
  filter(iso_a2 == "US") %>% 
  select(state = name, state_code = iso_3166_2)
# country lines
# downloaded globally then filtered to north america with st_intersect()
ne_country_lines <- ne_download(scale = 50, category = "cultural",
                                type = "admin_0_boundary_lines_land",
                                returnclass = "sf") %>% 
  st_geometry()
ne_country_lines <- st_intersects(ne_country_lines, ne_land, sparse = FALSE) %>%
  as.logical() %>%
  {ne_country_lines[.]}
# states, north america
ne_state_lines <- ne_download(scale = 50, category = "cultural",
                              type = "admin_1_states_provinces_lines",
                              returnclass = "sf") %>%
  filter(ADM0_A3 %in% c("USA", "CAN")) %>%
  mutate(iso_a2 = recode(ADM0_A3, USA = "US", CAN = "CAN")) %>% 
  select(country = ADM0_NAME, country_code = iso_a2)

# save all layers to a geopackage
unlink(gpkg_file)
write_sf(ne_land, gpkg_file, "ne_land")
write_sf(ne_states, gpkg_file, "ne_states")
write_sf(ne_country_lines, gpkg_file, "ne_country_lines")
write_sf(ne_state_lines, gpkg_file, "ne_state_lines")
