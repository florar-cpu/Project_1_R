setwd("/Users/florarobertson/Documents/Project_1_R/arbos_2025")

library(sf); library(dplyr); library(magrittr); library(ggplot2)
library(viridis); library(RColorBrewer); library(wesanderson)

# brazilian shapefile with harmonised ID code to disease data
shp = sf::st_read("./data/shapefiles/shp_harm/Brazil_shp_harm_2022.shp") %>%
  dplyr::filter(!name_mn %in% c("Lagoa Mirim", "Lagoa Dos Patos")) # remove lake borders
shp$IBGE6 = as.numeric(shp$IBGE6)

# extra info about immediate regions and intermediate regions (higher level admin units)
lk = read.csv("./data/lookup_tables/subregions_lookup.csv")
shp = shp %>% dplyr::left_join(lk)

# ----------- read dengue data -------------

# dengue data - monthly case counts for all municipalities (2001-2024)
# IBGE6 = unique municipality code linking disease data to shapefile
dd = read.csv("./data/disease_timeseries/dengue_monthlyTS_2001_2024.csv") %>%
  dplyr::mutate(Date = as.Date(Date))

# use geographic lookup file to get name, state and region of municipality
geo = read.csv("./data/lookup_tables/geo_lookup_table.csv")
dd = dplyr::left_join(dd, geo)

# add info about immediate regions from shapefile
# removes sf geometry before joining
dd = dd %>%
  dplyr::left_join(
    sf::st_drop_geometry( 
      shp[ , c("IBGE6", "code_immr", "name_immr") ] 
    )
  )

# add municipality populations per year, and calculate incidence 
pop = read.csv("./data/population/population_municipalities.csv")
dd = dplyr::left_join(dd, pop)

# calculate incidence per 100,000 persons
dd$Incidence = dd$NumCases / (dd$Population/100000)


# --------- summarise to a higher level (immediate regions; approx 500 in Brazil) --------------

# combine data from all the municipalities within each imm reg
# (often helps smooth over the noise in each individual municipality)
dd_imm = dd %>%
  dplyr::group_by(code_immr, Date) %>% # for each imm region and date
  dplyr::summarise(
    population = sum(Population), # total population in imm reg
    numcases = sum(NumCases) # total cases in imm reg
  ) %>%
  dplyr::mutate(
    incidence = numcases / (population/100000) # cases per 100,000 persons
  )

# Read in momentFM embeddings and PCA-reduced gaussian clusters from VS csv
emb = read.csv("/Users/florarobertson/Documents/Project 1/momentfm_embeddings_clusters.csv")

# Renaming code_immr column
emb = emb %>%
  rename(code_immr = emb_code_immr)

# Plot shaded by cluster mapping PCA reduced GMM clusters
# from momentFM embeddings
shp %>%
  dplyr::left_join(emb, by = join_by(code_immr)) %>%
  ggplot() + 
  geom_sf(aes(fill=as.factor(emb_cluster)), color=NA) +
  theme_minimal() + 
  scale_fill_viridis(option="magma", discrete=TRUE)


