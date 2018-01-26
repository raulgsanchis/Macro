inst <- function(){
  website <- c('')[1]
  pkg <- devtools::as.package(".")
  # rmarkdown::render("test.Rmd", "html_document")
  rmarkdown::render(paste0(devtools::as.package(".")$path,'/inst/webside/presentasjoner/pres_innledning.Rmd'))
  rmarkdown::render(paste0(devtools::as.package(".")$path,'/inst/webside/presentasjoner/pres_del1.Rmd'))
  rmarkdown::render(paste0(devtools::as.package(".")$path,'/inst/webside/presentasjoner/pres_del1.Rmd'))
  rmarkdown::render(paste0(devtools::as.package(".")$path,'/inst/webside/seminar/seminar1.Rmd'))
  rmarkdown::render(paste0(devtools::as.package(".")$path,'/inst/webside/seminar/seminar2.Rmd'))
  rmarkdown::render_site(paste0(devtools::as.package(".")$path,'/inst/webside/'))
  #system('cd ..')
  #system('pwd')
  #system('R CMD INSTALL --no-multiarch --with-keep.source MakroOEKB1115')
}


# system('/home/joernih/gitclones/teaching/ R CMD INSTALL --no-multiarch --with-keep.source MakroOEKB1115')

# Bygg nettside
#
# rmarkdown::render("test.Rmd", "html_document")
# R CMD INSTALL --no-multiarch --with-keep.source MakroOEKB1115
