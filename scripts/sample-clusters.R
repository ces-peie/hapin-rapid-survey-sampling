#------------------------------------------------------------------------------*
# Prepare shapefiles for sampling ----
#------------------------------------------------------------------------------*

# Load used packages
# library(package = "raster")
library(package = "raster")
library(package = "rgeos")
library(package = "maptools")
library(package = "tidyverse")




#------------------------------------------------------------------------------*
# Define study parameters ----
#------------------------------------------------------------------------------*

# Population parameters
household_size <- 6                                # people per household

# Sampling parameters
sample_size <- 400                                 # households
cluster_size <- 10                                 # households per cluster
n_clusters <- ceiling(sample_size / cluster_size)  # enough clusters for ss

# Inefficiency
response <- 2/3                                # proportion contacted included
suitable <- 4/5                                # proportion of suitable clusters
adj_clusters <- ceiling(n_clusters / suitable)




#------------------------------------------------------------------------------*
# Get limits data ----
#------------------------------------------------------------------------------*

# Set included municipalities
included <- c("Jalapa", "San Pedro Pinula")

# Read in limits
municipalities <- readShapePoly(
  fn = "data/shapefiles/borders/municipalities.shp"
)

# Keep only Jalapa
jalapa <- municipalities %>%
  subset(as.character(COD_MUNI) >= 2100 & as.character(COD_MUNI) < 2200)

# Keep only study municipalities
study_munis <- jalapa %>%
  subset(MUNICIPIO %in% included)

# Read exluded areas
excluded <- readShapePoly(
  fn = "data/shapefiles/exclusions/exclusions.shp"
)

# Subtract excluded areas
study_area <- gDifference(study_munis, excluded)




#------------------------------------------------------------------------------*
# Get density data ----
#------------------------------------------------------------------------------*

# Read in rasters
pop_density <- raster(x = "data/jalapa_density.gri")

# Limit to study area
study_density <- rasterize(study_area, pop_density, mask = TRUE)

# Median cell density
median_density <- median(study_density[], na.rm = TRUE)

# Minimum cluster (area) population for household_size
min_cluster_pop <- household_size * (cluster_size / response)

# Inflate cluster size to ensure enough households
adj_cluster_pop <- min_cluster_pop * 2 / median_density

# Calculate cells per (square) cluster
cluster_side <- ceiling(sqrt(adj_cluster_pop))

# Aggregate population cells
cluster_cells <- aggregate(study_density, fact = cluster_side, fun = sum)




#------------------------------------------------------------------------------*
# Sample clusters ----
#------------------------------------------------------------------------------*

# Set seed for reproducibility
set.seed(2017-05)

# Half width
hs <- res(cluster_cells)/2

# Calculate weights
weights <- cluster_cells
weights[is.na(weights[])] <- 0         # set NAs to 0 for sampling
weights <- weights / max(weights[])    # scale to probabilities

# First stage of sampling: sample clusters
sampled_clusters <- sample(
  x = seq_len(length.out = length(weights[])),
  size = adj_clusters,
  replace = FALSE,
  prob = weights[]
)

# Get locations of sample clusters
centers <- xyFromCell(cluster_cells, sampled_clusters)

# Get polygons for each cluster
cluster_cells[is.na(cluster_cells)] <- 0
cluster_polygons <- rasterToPolygons(
  x = cluster_cells,
  function(x) seq_along(x) %in% sampled_clusters
)


# Collect data for sample clusters
clusters <- cluster_polygons@data %>%
  rownames_to_column(var = "correlative") %>%
  mutate(
    correlative = order(sampled_clusters),
    cell_id = sampled_clusters,
    households = layer / household_size
  ) %>%
  select(correlative, cell_id, population = layer, households) %>%
  bind_cols(as_tibble(centers[order(sampled_clusters),])) %>%
  rename(lat = y, long = x)


# Edit clusters data
cluster_polygons <- SpatialPolygonsDataFrame(
  Sr = cluster_polygons,
  data = clusters,
  match.ID = TRUE
)




#------------------------------------------------------------------------------*
# Write sampling data ----
#------------------------------------------------------------------------------*

# Write cluster data
write_csv(clusters, path = "output/clusters.csv")

# Write cluster polygons
writeSpatialShape(cluster_polygons, fn = "output/clusters/clusters")

# Function to get cluster tiles
get_tile <- function(cluster, save_path = NULL, zoom = 16, format = "GTiff"){
  # cell center
  center <- cluster[c("lat", "long")] %>% unlist()
  
  # Get tile
  cell <- RgoogleMaps::GetMap(
    center = center[c("lat", "long")], zoom=16, maptype = "satellite"
  )
  
  # Convert to raster
  bb <- cell$BBOX
  cell <- stack(file.path(tempdir(), "mapTile.png"))
  extent(cell) <- extent(bb$ll[,2],bb$ur[,2],bb$ll[,1],bb$ur[,1])
  
  if(!is.null(save_path)){
    # file path
    save_path <- file.path(
      save_path,
      stringr::str_pad(
        string = cluster$correlative, width = 2, side = "left", pad = "0"
      )
    )
    writeRaster(
      x = cell, filename = save_path, format = format, overwrite = TRUE
    )
    file.copy(file.path(tempdir(), "mapTile.png"), paste0(save_path, ".png"))
  }
  
  cell <- stack(paste0(save_path, ".png"))
  extent(cell) <- extent(bb$ll[,2],bb$ur[,2],bb$ll[,1],bb$ur[,1])
  
  return(cell)
}

# Get all cells
clusters <- clusters %>%
  # group_by(correlative, population, households, lat, long) %>%
  rowwise() %>%
  do(
    data = .data,
    cell = get_tile(cluster = ., save_path = "output/cluster_satellite")
  )


#------------------------------------------------------------------------------*
# Plot everything ----
#------------------------------------------------------------------------------*

# Plot raw data
png(filename = "output/map_areas.png", width = 5, height = 5, units = "in", res = 600)
plot(study_density)
title("Raw population density\nand study areas")
plot(jalapa, add = TRUE, asp = 1)
plot(excluded, add = TRUE, col = "grey20", asp = 1)
lines(study_munis, col = "red")
dev.off()


# Plot aggregated density and sampled clusters
png(filename = "output/map_clusters.png", width = 5, height = 5, units = "in", res = 600)
plot(cluster_cells)
title("Aggregated population density\nand selected clusters")
plot(jalapa, add = TRUE, asp = 1)
plot(cluster_polygons, add = TRUE, asp = 1)
points(centers, pch = ".")
dev.off()



# End of script

