#------------------------------------------------------------------------------*
# Evaluate rapid cluster assessment
#------------------------------------------------------------------------------*


# Load used packages
library(package = "readxl")
library(package = "tidyverse")




#------------------------------------------------------------------------------*
# Prepare data
#------------------------------------------------------------------------------*

# Load visual assessment data
jrivera <- read_csv("data/roofs/cluster_rapid_jrivera.csv")
aramirez <- read_excel("data/roofs/cluster_rapid_aramirez.xlsx")


# Standardize input
st_jrivera <- jrivera %>%
  select(
    correlative, cell_id,
    jr_type = tipo, jr_assesment = suficientes, jr_comment = comentario
  ) %>%
  mutate(
    jr_type = case_when(
      grepl("Rural", jr_type, ignore.case = TRUE) ~ "rural",
      grepl("Urb", jr_type, ignore.case = TRUE) ~ "urban",
      is.na(jr_type) ~ NA_character_,
      TRUE ~ "error"
    )
  )

st_aramirez <- aramirez %>%
  select(
    correlative, cell_id,
    ar_type = tipo, ar_assesment = suficientes, ar_comment = comentario
  ) %>%
  mutate(
    ar_type = case_when(
      grepl("R", ar_type, ignore.case = TRUE) ~ "rural",
      grepl("U", ar_type, ignore.case = TRUE) ~ "urban",
      is.na(ar_type) ~ NA_character_,
      TRUE ~ "error"
    )
  )




#------------------------------------------------------------------------------*
# Review assessment
#------------------------------------------------------------------------------*

# Join assessments
assessment <- st_aramirez %>%
  left_join(st_jrivera)


# Compare assesment

# Evaluated by jrivera
assessment %>% filter(jr_type %in% c("urban", "rural")) %>% nrow()

# Evaluated by aramirez
assessment %>% filter(ar_type %in% c("urban", "rural")) %>% nrow()

# Concordance among evaluated
assessment %>% filter(!is.na(jr_type)) %>% count(ar_type, jr_type)

# Review discordances in cluster type (rural vs urban)
assessment %>%
  filter(
    !is.na(jr_type),
    jr_type != ar_type
  )

# Review discordances in cluster suitability
assessment %>%
  filter(
    !is.na(jr_type),
    jr_assesment != ar_assesment,
    # Remove cluster 60 (cell id 1127)
    correlative != 60
  ) %>%
  select(-ar_comment, -jr_comment)



# Get list of clusters evaluated as suitable
suitable <- assessment %>%
  filter(ar_assesment == 1)


# Copy images from suitable clusters
suitable %>%
  pull(correlative) %>%
  stringr::str_pad(string = ., width = 2, side = "left", pad = "0") %>%
  file.copy(
    from = paste0("output/cluster_satellite/img/", .,".png"),
    to = paste0("output/cluster_satellite/suitable/", .,".png"),
    copy.date = .
  )


# Generate query to filter shapefile
suitable %>%
  pull(correlative) %>%
  paste(
    collapse = ", "
  ) %>%
  paste0(
    "\"correlativ\" IN (",
    .,
    ")"
  ) %>%
  cat

# End of script
