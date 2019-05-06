library(tidyverse)
library(readxl)

df <- read_excel("input/DataDownload - Copy.xlsx", 2)
worksheet <- df %>% distinct(`Category Code`) %>% pull(`Category Code`)


for (sheets in worksheet){
  df_name <- paste('df',sheets,sep='_')
  print(df_name)
  assign(paste(df_name), read_excel("input/DataDownload - Copy.xlsx", sheet = sheets))
}

