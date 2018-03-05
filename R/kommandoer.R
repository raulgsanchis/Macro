inst <- function(){
  website <- c('')[1]
  pkg <- devtools::as.package(".")
  # rmarkdown::render("test.Rmd", "html_document")
  rmarkdown::render(paste0(devtools::as.package(".")$path,'/inst/webside/presentasjoner/pres_innledning.Rmd'))
  rmarkdown::render(paste0(devtools::as.package(".")$path,'/inst/webside/presentasjoner/pres_del1_lukket.Rmd'))
  rmarkdown::render(paste0(devtools::as.package(".")$path,'/inst/webside/presentasjoner/pres_del1_open.Rmd'))
  rmarkdown::render(paste0(devtools::as.package(".")$path,'/inst/webside/presentasjoner/pres_del2.Rmd'))
  rmarkdown::render(paste0(devtools::as.package(".")$path,'/inst/webside/seminar/seminar1.Rmd'))
  rmarkdown::render(paste0(devtools::as.package(".")$path,'/inst/webside/seminar/seminar2.Rmd'))
  rmarkdown::render(paste0(devtools::as.package(".")$path,'/inst/webside/seminar/seminar3.Rmd'))
  rmarkdown::render(paste0(devtools::as.package(".")$path,'/inst/webside/seminar/seminar4AV.Rmd'))
  rmarkdown::render(paste0(devtools::as.package(".")$path,'/inst/webside/seminar/seminar4PAA.Rmd'))
  rmarkdown::render_site(paste0(devtools::as.package(".")$path,'/inst/webside/'))
  #system('cd ..')
  #system('pwd')
  #system('R CMD INSTALL --no-multiarch --with-keep.source MakroOEKB1115')
}

publisering <- function(){
#  scp -r /home/joernih/R/x86_64-pc-linux-gnu-library/3.4/MakroOEKB1115/webside/_site adasextended@login.domeneshop.no:www/
  #attom-46-langt-Lagge-anode#
  #ssh adasextended@login.domeneshop.no
}


# system('/home/joernih/gitclones/teaching/ R CMD INSTALL --no-multiarch --with-keep.source MakroOEKB1115')

# Bygg nettside
# rmarkdown::render("test.Rmd", "html_document")
# R CMD INSTALL --no-multiarch --with-keep.source MakroOEKB1115


inst2 <- function(){
  website <- c('')[1]
  pkg <- devtools::as.package(".")
  # rmarkdown::render("test.Rmd", "html_document")
  rmarkdown::render(paste0(devtools::as.package(".")$path,'/inst/websidebygger/presentasjoner/pres_innledning.Rmd'))
  rmarkdown::render(paste0(devtools::as.package(".")$path,'/inst/websidebygger/presentasjoner/pres_del1.Rmd'))
  #rmarkdown::render(paste0(devtools::as.package(".")$path,'/inst/websidebygger/presentasjoner/pres_del1.Rmd'))
  rmarkdown::render(paste0(devtools::as.package(".")$path,'/inst/websidebygger/seminar/seminar1.Rmd'))
  rmarkdown::render(paste0(devtools::as.package(".")$path,'/inst/websidebygger/seminar/seminar2.Rmd'))
  rmarkdown::render_site(paste0(devtools::as.package(".")$path,'/inst/websidebygger/'))
  #system('cd ..')
  #system('pwd')
  #system('R CMD INSTALL --no-multiarch --with-keep.source MakroOEKB1115')
}
