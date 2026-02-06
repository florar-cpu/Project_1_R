setwd("/Users/florarobertson/Documents/Project_1_R")

library(RcppCNPy); library(FactoMineR); library(factoextra)

# Reading in csv of embeddings with attached location codes
emb = read.csv("/Users/florarobertson/Documents/Project_1_R/momentfm_embeddings.csv")

# renaming columns
# Renaming code_immr column
emb = emb %>%
  rename(code_immr = emb_code_immr)

# Removing location codes
embeddings_only <- emb %>%
                    select(-code_immr)

# Scaling embeddings
scaled_embeddings <- scale(embeddings_only)

# Computing PCA
data.pca <- prcomp(scaled_embeddings)
summary

# Scree plot to visualise importance of PCs

fviz_eig(data.pca, addlabels = TRUE, ncp = 50)
