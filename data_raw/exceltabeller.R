mappe <- paste0(devtools::as.package(".")$path)
source(paste0(mappe,'/data_raw/freddatanorway.R'))
source(paste0(mappe,'/data_raw/freddatatyskland.R'))
source(paste0(mappe,'/data_raw/freddatahellas.R'))

excobj <- list(norge = lmoltmacronor, tyskland = lmoltmacroger, hellas = lmoltmacrohel)

openxlsx::write.xlsx(excobj, file=paste0(mappe,'/inst/webside/excel/dataoppgaveseminar1.xlsx'))
