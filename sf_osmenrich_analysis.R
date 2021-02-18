# sf and osmdata analysis
# Last edited 2021-02-16 by @leonardovida
# CC-BY ODISSEI SoDa team

# Packages ----
# Data
library(tidyverse)
library(sf)
library(osmenrich) # remotes::install_github("sodascience/osmenrich")
library(osmdata)
library(ggmap)

# Plotting
library(firatheme) # remotes::install_github("vankesteren/firatheme")
library(ggplot2)

# Optional: use local version of osm
# osmdata::set_overpass_url("http://localhost:8888/api/interpreter")

################
# sf and osmdata
################

# Create data
# Extract Utrecht boundaries
utrecht_sf <- osmdata::opq(bbox = "utrecht nl")

# Retrieve trees using the boundaries of Utrecht
trees <- utrecht_sf %>%
  osmdata::add_osm_feature(key = "natural", value = "tree") %>%
  osmdata::osmdata_sf()

# Create plot of trees within the boundaries
plot_trees <- ggplot() +
  geom_sf(
    data=trees$osm_points,
    fill="darkgreen",
    color="darkgreen"
  ) +
  theme_fira()

# Plot trees
plot_trees

# Retrieve the map of Utrecht
utrecht_map <- ggmap::get_map(
  c(left = 5.0041822,
    bottom = 52.026282,
    right = 5.195155,
    top = 52.1356715
  ),
  maptype = "toner-background")

# Plot the Map
ggmap(utrecht_map)

# Create plot of the trees using the map of Utrecht
plot_trees_utrecht <- ggmap(utrecht_map) +
  geom_sf(
    data = trees$osm_points,
    inherit.aes = FALSE,
    fill = "darkgreen",
    color = "darkgreen"
  ) +
  theme_fira() +
  labs(title = "Trees in Utrecht", colour = "")

# Plot map with trees
plot_trees_utrecht

###########
# osmenrich
###########

# Create tibble sparrows
# and transform it to sf
sf_sparrows <-
  tribble(
    ~sparrow, ~lat, ~lon,
    "Sparrow_Alice", 52.12, 5.09,
    "Sparrow_Bob", 52.13, 5.08,
  ) %>%
  sf::st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326
  )

# Print sparrows sf
sf_sparrows

# Retrieve another map of Utrecht
# but moved over the sparrows
utrecht_map <- ggmap::get_map(
  c(left = 5.05,
    bottom = 52.10,
    right = 5.110,
    top = 52.14
  ),
  maptype = "toner-background")

# Plot the map
utrecht_map

# Create the plot of the positions of the sparrows
plot_sparrows <- ggmap(utrecht_map) +
  geom_sf(
    inherit.aes = FALSE,
    data = sf_sparrows,
    colour = "red",
    fill = "red",
    size = 5,
  ) +
  labs(title = "Sparrow position", colour = "") +
  theme_fira()

# Plot the sparrow positions
plot_sparrows

# Data augmentation of sparrows sf
# with trees around them within the
# boundaries of Utrecht
sf_sparrows_enriched <- sf_sparrows %>%
  osmenrich::enrich_osm(
    name = "n_trees",
    key = "natural",
    value = "tree",
    r = 1000
  )

# Plot the enriched data
plot_sparrows_enriched <- ggmap(utrecht_map) +
  geom_sf(
    data = trees$osm_points,
    inherit.aes = FALSE,
    fill = "darkgreen",
    color = "darkgreen"
  ) +
  geom_sf(
    inherit.aes = FALSE,
    data = sf_sparrows_enriched,
    colour = "red",
    fill = "red",
    size = 5,
  ) +
  labs(title = "Sparrow and trees position", colour = "") +
  theme_fira()


# Display the plot
plot_sparrows_enriched
