#' @export
buildweb <- function(webnr=1){

  website <- c('')[webnr]

  pkg <- devtools::as.package(".")
  rmarkdown::render_site(paste0(devtools::as.package(".")$path,'/inst/webside/'))
}

gitcomit <- function(message=Sys.Date()){
  system("git add --all", TRUE)
  system("git commit -m 'ABC'", TRUE)
  system("git push 'ABC'", TRUE)
}

gitconfig <- function(){
  system("git config --global user.email 'jorn.halvorsen@gmail.com'")
  system("git config --global user.name 'joernih'")
}
