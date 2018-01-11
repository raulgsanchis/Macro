library(readxl)
library(dplyr)

# 0 Lese inn data
deltakerliste <- read_excel(paste0(devtools::as.package(".")$path,'/deltakere/ØKB1115-Makroøkonomi-Studenter-Vår-2018.xlsx'))

# 1. Rense data
names(deltakerliste) <- c("gendato", "X1", "Fornavn", "Etternavn", "epost", "mobil", "studieprogram", "status")

deltakerlistefil <- deltakerliste %>% dplyr::mutate(passord = substring(epost, 1, 6)) %>% dplyr::select(-X1)

# 2. Lagre data i Rda-format
devtools::use_data(deltakerlistefil, overwrite = TRUE)

load("~/gitclones/teaching/MakroOEKB1115/data/  .rda")


