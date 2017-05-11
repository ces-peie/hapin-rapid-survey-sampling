

library(package = "raster")

# Read in rasters
pop_density <- raster(x = "data/jalapa_density.gri")
fine_dem <- raster(x = "data/jalapa_dem.gri")


# Aggregate finer (fine_dem) raster into the coarser (pop_density) resolution
coarse_dem <- aggregate(x = fine_dem, fact = 3, fun = mean)

# Check if rasters match
compareRaster(coarse_dem, pop_density)

# Resample dem to align to population density raster
aligned_dem <- resample(x = coarse_dem, y = pop_density)

# Check if rasters match
compareRaster(aligned_dem, pop_density)

# Check out values
par(mfrow = c(1, 2))
plot(aligned_dem, main = "Jalapa DEM")
plot(pop_density, main = "Jalapa population density")
par(mfrow = c(1, 1))

# Save rasters
writeRaster(
  x = aligned_dem, filename = "data/jalapa_aligned_dem.asc", format = "ascii"
)

writeRaster(
  x = pop_density, filename = "data/jalapa_aligned_density.asc", format = "ascii"
)


# End of script
