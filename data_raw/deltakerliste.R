library(readxl)
library(dplyr)

# 0 Lese inn data
deltakerliste <- read_excel(paste0(devtools::as.package(".")$path,'/deltakere/ØKB1115-Makroøkonomi-Studenter-Vår-2018.xlsx'))

# 1. Rense data
deltakerlistefil <- deltakerliste

# 2. Lagre data i Rda-format
devtools::use_data(deltakerlistefil, overwrite = TRUE)
