# Gierzwaluw (common swift) analysis using osmenrich
# Last edited 2021-02-16 by @vankesteren
# CC-BY ODISSEI SoDa team

# Packages ----
# Data
library(tidyverse)
library(sf)
library(osmenrich)

# Modeling
library(mgcv)

# Plotting
library(ggspatial)
library(firatheme) # remotes::install_github("vankesteren/firatheme")
library(ggeffects)

# Optional: use local version of osm
# osmdata::set_overpass_url("http://localhost:8888/api/interpreter")

# Data loading ----
data_url <- "https://ckan.dataplatform.nl/dataset/8ceaae10-fb90-4ec0-961c-ef02691bb861/resource/baae4cde-cf33-416b-aa4e-d0fba160eed9/download/gierzwaluwinventarisatie2014.csv"
bird_sf <-
  read_csv(data_url) %>%
  drop_na(latitude, longitude, `aantal nesten`) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  select(nestcount = "aantal nesten", geometry)

# Plot data
plot_bird <-
  ggplot(bird_sf, aes(colour = nestcount, size = nestcount)) +
  annotation_map_tile(zoomin = 0) +
  geom_sf() +
  theme_fira() +
  scale_colour_viridis_c() +
  scale_size(range = c(1, 3), guide = FALSE) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Common swift nests", colour = "")

plot_bird

# Data augmentation ----
# randomly sample 200 non-bird sites:
# centroids of grid cells where nests were not found
# ASSUMPTION: The common swift inventarisation is comprehensive over the grid.
set.seed(45)
bird_grid  <- st_make_grid(bird_sf, n = c(40, 60))
has_nests  <- apply(st_contains(bird_grid, bird_sf, sparse = FALSE), 1, any)
zerosample <- st_centroid(sample(bird_grid[!has_nests], 200))
nonbird_sf <- st_sf(geometry = zerosample) %>% mutate(nestcount = 0)
bird_sf    <- bind_rows(bird_sf, nonbird_sf)

# Plot augmented data
plot_bird +
  geom_sf(data = nonbird_sf, colour = "black", bg = "black", size = 1, shape = 22)

# Data enrichment ----
# using osmenrich for computing the following two features:
# The number of shops as a proxy of commercial activity
# The number of trees as a proxy of natural material availability
bird_sf <-
  bird_sf %>%
  enrich_osm(
    name = "commercial_activity_1km",
    key = "shop",
    kernel = "gaussian",
    r = 1000
  ) %>%
  enrich_osm(
    name = "tree_1km",
    key = "natural",
    value = "tree",
    kernel = "gaussian",
    r = 1000
  )

# We can do the same for a grid covering the bounding box of the data
grid_sf_c <-
  st_centroid(bird_grid) %>%
  st_sf() %>%
  enrich_osm(
    name = "commercial_activity_1km",
    key = "shop",
    kernel = "gaussian",
    r = 1000,
    .verbose = TRUE
  ) %>%
  enrich_osm(
    name = "tree_1km",
    key = "natural",
    value = "tree",
    kernel = "gaussian",
    r = 1000,
    .verbose = TRUE
  )

grid_sf_p <- grid_sf_c %>% st_set_geometry(bird_grid)

# Plot predictors on the grid
plot_commercial_activity <-
  ggplot() +
  annotation_map_tile(zoomin = 0) +
  geom_sf(data = grid_sf_p, aes(fill = commercial_activity_1km), colour = NA, alpha = 0.7) +
  geom_sf(data = bird_sf %>% filter(nestcount > 0), colour = "black") +
  theme_fira() +
  scale_fill_viridis_c() +
  labs(title = "Commercial activity", fill = "") +
  theme(axis.text.x = element_text(angle = 90))

plot_commercial_activity

plot_trees <-
  ggplot() +
  annotation_map_tile(zoomin = 0) +
  geom_sf(data = grid_sf_p, aes(fill = tree_1km), colour = NA, alpha = 0.7) +
  geom_sf(data = bird_sf %>% filter(nestcount > 0), colour = "black") +
  theme_fira() +
  scale_fill_viridis_c() +
  labs(title = "Trees", fill = "") +
  theme(axis.text.x = element_text(angle = 90))

plot_trees


# Modeling ----
# zero-inflated poisson spatial generalized additive model
# We need a function to turn sf with coordinates into df with x and y
sf_to_df <- function(sf) {
  bind_cols(
    as_tibble(sf) %>% select(-geometry),
    st_coordinates(sf) %>% as_tibble() %>% set_names(c("x", "y"))
  )
}

# generalized additive model with mgcv
fit <- gam(
  nestcount ~
    te(x, y, bs = "ts") +              # bivariate regularized thin plate spline
    poly(commercial_activity_1km, 2) + # quadratic effect of commercial activity
    tree_1km,                          # natural material availability proxy
  family = "ziP",                      # zero-inflated poisson
  data = sf_to_df(bird_sf)
)

# Interpretation and prediction ----
# effects of spline, commercial activity, and natural material availability
summary(fit)

# plot the marginal effect of commercial activity
values <- seq(0, 0.10, 0.01)
plot_marginal <-
  ggpredict(fit, terms = "commercial_activity_1km [values]",
            condition = c(x = 5.10, y = 52.125)) %>%
  ggplot(aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high)) +
  geom_ribbon(alpha = 0.4, fill = firaCols[3]) +
  geom_line(colour = firaCols[3], size = 1) +
  theme_fira() +
  labs(y = "Predicted nests", x = "Commercial activity",
       title = "Marginal effect of commercial activity")

plot_marginal

# create predictions over previously defined grid
pred <- predict(fit, newdata = sf_to_df(grid_sf_c), type = "response", se = TRUE)

# plot the predictions over the grid
grid_sf_p$pred <- pred$fit
grid_sf_p$se   <- pred$se.fit

plot_pred <-
  ggplot() +
  annotation_map_tile(zoomin = 0) +
  geom_sf(data = grid_sf_p, aes(fill = pred), colour = NA, alpha = 0.7) +
  geom_sf(data = bird_sf %>% filter(nestcount > 0), colour = "black") +
  theme_fira() +
  scale_fill_viridis_c() +
  labs(title = "Nest count", fill = "") +
  theme(axis.text.x = element_text(angle = 90))

plot_pred


# Plot the spline only
grid_sf_c2 <-
  grid_sf_c %>%
  mutate(
    commercial_activity_1km = 0,
    grass_300m = 0,
    tree_300m = 0
  )

pred2 <- predict(fit, newdata = sf_to_df(grid_sf_c2), type = "response", se = TRUE)
grid_sf_p$pred2 <- pred2$fit

plot_pred_smooth_only <-
  ggplot() +
  annotation_map_tile(zoomin = 0) +
  geom_sf(data = grid_sf_p, aes(fill = pred2), colour = NA, alpha = 0.7) +
  geom_sf(data = bird_sf %>% filter(nestcount > 0), colour = "black") +
  theme_fira() +
  scale_fill_viridis_c() +
  labs(title = "Spline-only predictions", fill = "") +
  theme(axis.text.x = element_text(angle = 90))

plot_pred_smooth_only

