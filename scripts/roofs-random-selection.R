#------------------------------------------------------------------------------*
# Randomly select roofs from roof census
#------------------------------------------------------------------------------*


#------------------------------------------------------------------------------*
# Prepare analysis environment ----
#------------------------------------------------------------------------------*

# Load used packages
library(package = "sf")
library(package = "tidyverse")




#------------------------------------------------------------------------------*
# Load roof census data ----
#------------------------------------------------------------------------------*

# List available shapefiles
shapefiles <- list.files("data/roofs/census", full.names = TRUE, pattern = "shp")

# Get all roofs
all_roofs <- shapefiles %>%
  # Read each file
  map(read_sf) %>%
  # Standardize field names
  map(set_names, nm = c("id", "cluster", "observacio", "geometry")) %>%
  # Standardize CRS
  map(st_set_crs, value = 4326) %>%
  # Bind all
  do.call(rbind, .)




#------------------------------------------------------------------------------*
# Sample roofs ----
#------------------------------------------------------------------------------*

# Reproducible sample
set.seed(2017-06-01)

# Sample parameters
roofs_by_cluster <- 20

# Sample roofs
sampled_roofs <- all_roofs %>%
  # By cluster
  split(.$cluster) %>%
  # Sample a subset
  map(
    ~subset(
      .x,
      subset = 1:nrow(.x) %in% sample(1:nrow(.x), roofs_by_cluster, replace = FALSE)
    )
  ) %>%
  # Add house id
  map(
    cbind,
    name = as.character(1:roofs_by_cluster)
  ) %>%
  # Add left/right pick
  map(
    ~cbind(
      .x,
      elegir = sample(c("izquierda", "derecha"), size = nrow(.x), replace = TRUE)
    )
  ) %>%
  # Bind all
  do.call(rbind, .)

# Write sampled roofs
write_sf(sampled_roofs, dsn = "data/roofs/sampled/sampled.shp")
write_csv(sampled_roofs, path = "data/roofs/sampled/casas-elegidas.csv")

# Export as individual GPX files for use in the gps receivers
sampled_roofs %>%
  mutate(
    cluster = stringr::str_pad(cluster, width = 3, side = "left", pad = "0"),
    name = stringr::str_pad(name, width = 2, side = "left", pad = "0"),
    name = paste(cluster, name, sep = "-")
  ) %>%
  split(sampled_roofs$cluster) %>%
  walk(
    ~write_sf(
      obj = select(.x, name, geometry),
      dsn = paste0("output/garmin/casas", first(.x$cluster), ".gpx"),
      driver = "GPX",
      delete_dsn = TRUE
    )
  )


# End of script
