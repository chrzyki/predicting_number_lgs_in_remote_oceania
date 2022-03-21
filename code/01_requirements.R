# Please run this script first to make sure you have all the necessary packages 
# installed for running the rest of the scripts in this R project

if (!suppressPackageStartupMessages(require("pacman"))) { install.packages("pacman") }

pacman::p_load(
  tidyverse,
  missForest,
  reshape2,
  modEvA,
  MuMIn,
  rsq,
  cluster,
  viridis,
  rlang,
  Amelia,
  readODS,
  devtools,
  matrixcalc,
  forcats,
  knitr, 
  lazyeval,
  gplots,
  igraph,
  geosphere,
  diagram,
  foreign, 
  MASS, 
  colorspace,
  RColorBrewer,
  wesanderson,
  randomcoloR,
  ggplot2,
  ggthemes,
  tidytree ,
  sandwich, 
  msm,
  readxl,
  glue,
  broom, 
  pscl,
  ggrepel,
  ggpubr,
  cowplot,
  fuzzyjoin,
  infotheo,
  rlist,
  data.table,
  #making maps
  mapdata,
  maptools,
  maps,
  mapproj,
  ggmap,
  qgraph,
  glue,
  stringi,
  Rarity,
  ape, 
  castor,
  naniar, 
  fields,
  adephylo,
  phytools,
  diversitree,
  phylobase, 
  phangorn, 
  treeman, 
  devtools,
  xtable,
  broom, 
  sp, 
  raster, 
  scales
)

unlist_entire_df <- function(data) {
  temp1 <- sapply(data, is.list)
  temp2 <- do.call(
    cbind, lapply(data[temp1], function(x) 
      data.frame(do.call(rbind, x), check.names=FALSE)))
  cbind(data[!temp1], temp2)
}

#quieting down tidyverse
options(tidyverse.quiet = TRUE)