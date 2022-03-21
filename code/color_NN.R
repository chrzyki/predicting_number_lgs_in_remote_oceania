if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse)

p_load_gh("abrozzi/microbio")



Glottolog <- read_tsv("data/Glottolog_lookup_table_Heti_edition.tsv") %>%
  dplyr::select(glottocode, Language_level_ID, Name = Name_stripped_no_spaces)


subregions <- read_tsv("data/oceania_subregions.tsv") %>% 
  dplyr::select(glottocode = Glottocode, Boxplot_groups, boxplot_color, Abberancy_group_color) %>% 
  left_join(Glottolog)

##Aberrant
raw_data <- readLines("output/Vizualisations/distance_based/Neighbournet/splitstree_saves/GB_NN_Aberrant.nex") %>%
  tibble(
    line = 1:length(.),
    data = .
  )

inputfile <- "output/Vizualisations/distance_based/Neighbournet/splitstree_saves/GB_NN_Aberrant.nex"

outfile <- "output/Vizualisations/distance_based/Neighbournet/coloured/GB_NN_Aberrant.nex"

#the vlabels chunk is the only chunk where all lines have "x="
raw_data %>% 
  filter(str_detect(data, " x=")) -> vlabels_raw


vlabels_raw %>% 
  separate(col = data, into = c("index", "glottocode", "formatting"), sep = fixed("'")) %>% 
  dplyr::select(-formatting, -line) %>%   
  left_join(subregions )  %>% 
  distinct(glottocode, .keep_all = T)-> vlabels

vlabels$color_lc <-vlabels$Abberancy_group_color

vlabels <- vlabels %>% 
  mutate(font = "Arial-BOLD-9") %>% 
  mutate(color_background = "white") %>% 
  mutate(color_tip = color_lc) %>% 
  mutate(color_fg = color_lc)

#Abrozz'is Microbio



tips <- vlabels$glottocode
vlabel_argument <- vlabels$Name
col <- vlabels$color_tip
fg <- vlabels$color_fg
lc <- vlabels$color_lc
s <- "o"
w <- 4
h <- 4
edgecolor <- "black"
lk <- vlabels$color_background
edgecolor <- "black"
f <- vlabels$font


custom_nexus(nexus.file=inputfile, tips=tips, colors=col, vlabels=vlabel_argument ,f = "Dialog-12", lc=lc, lk=lk, fg= fg, s = s, w = w, h = h, edgecolor=edgecolor, outfile= outfile, plot = F)