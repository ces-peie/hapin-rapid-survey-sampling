
requested_clusters <- c(11, 34, 46)

communities <- readShapePoints("data/shapefiles/communities/communities_ine_2002.shp")

in_clusters <- cluster_polygons %>%
  subset(correlative %in% requested_clusters) %>%
  gIntersection(spgeom1 = communities, spgeom2 = ., byid = TRUE)

# Bounding box in data frame
bbox_df <- . %>%
  bbox() %>%
  as.data.frame %>%
  rownames_to_column(var = "direction") %>%
  mutate(direction = recode(direction, x = "long", y = "lat")) %>%
  gather(key = variable, value = value, -direction) %>%
  unite(var, direction, variable) %>%
  spread(var, value) %>%
  as_tibble()


# Clusters data
cluster_data <- requested_clusters %>%
  data_frame(cluster_id = .) %>%
  # Cluster information
  mutate(
    cluster_data = map(cluster_id, ~subset(x = cluster_polygons, subset = correlative %in% .x)),
    cluster_row = map_chr(cluster_data, ~rownames(.x@data)),
    bbox = map(cluster_data, bbox_df)
  ) %>%
  unnest(bbox) %>%
  select(-cluster_data)


# Communities data
communities_data <- in_clusters@coords %>%
  as.data.frame() %>% 
  rownames_to_column(var = "ids") %>%
  separate(ids, into = c("cummunity_row", "cluster_row")) %>%
  as_tibble() %>%
  # Add community data
  left_join(rownames_to_column(communities@data, var = "cummunity_row")) %>%
  select(
    cluster_row,
    community_long = x, community_lat = y, community_code = NUEVO_COD,
    community_type = CATEGORIA, community_name = LUGAR_POBL
  )


# Combined
validation_data <- cluster_data %>%
  left_join(communities_data) %>%
  select(-cluster_row)


write_csv(validation_data, path = "output/cluster_validation_data.csv")

