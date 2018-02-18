library(ggplot2)
library(dplyr)

Makrofigur <- setRefClass("Makrofigur", fields = list(modell='vector', modellatr ='list',
                                                      plotvectorend='list', dfmodellres='list',
                                                      ggtyper='list'))

Makrofigur$methods(initialize=function(navn){

  keynes <- rjson::fromJSON(file=paste0(devtools::as.package(".")$path,'/inst/webside/jupyter/keynesequ.json'))
  islml <- rjson::fromJSON(file=paste0(devtools::as.package(".")$path,'/inst/webside/jupyter/islmequ.json'))
  islmo <- rjson::fromJSON(file=paste0(devtools::as.package(".")$path,'/inst/webside/jupyter/islmequ.json'))
  adasl <- rjson::fromJSON(file=paste0(devtools::as.package(".")$path,'/inst/webside/jupyter/adascequ.json'))
  adaso <- rjson::fromJSON(file=paste0(devtools::as.package(".")$path,'/inst/webside/jupyter/adasoequ.json'))

  modellatr <<- list(keynes=keynes, islml=islml, islmo=islmo, adasl=adasl, adaso=adaso)

  modell <<- c(navn)

})

Makrofigur$methods(numerisk=function(endvar=NULL ,lukketpar=NULL, openpar=NULL, endrvar = NULL, kat=NULL){

  exoparval <- c(lukketpar, openpar, endrvar)

  for(mvar in endvar){
    endv <- list(eval(parse(text=  modellatr[[modell]][[mvar]]), exoparval))
    plotvectorend <<- append(plotvectorend, endv)
  }

  names(plotvectorend) <<- endvar

  DFmodellres <- data.frame(Iv= endrvar[[1]], plotvectorend) %>% reshape2::melt(id.vars = c("Iv"))

  dfmodellres <<- append(dfmodellres, list(DFmodellres))

  names(dfmodellres)[length(dfmodellres)]  <<- c(kat)

  #browser()

})


Makrofigur$methods(grafisknum=function(samlikv=list(x=NULL, y=NULL), manuell=1){

  browser()

  ggobjnum <- ggplot() + geom_line(data = dfmodellres, aes(x = Iv, y = value, color = factor(variable))) +
    geom_point(aes(x=samlikv$x, y=samlikv$x)) +
    geom_segment(aes(x = samlikv$x, y = samlikv$y ,
                      xend = samlikv$x, yend = 0), lty = 2) +
    geom_segment(aes(x = samlikv$x, y = samlikv$y ,
                     xend = 0, yend = samlikv$y), lty = 2)

  ggtyper <<- list(ggobjnum)

})

## Samtidig likevekt
Yv = list(Y=1:600)
keypar <- list(c_1 = 0.6, oC = 25, oG= 50, b = 30, i = 0.03, oI = 50, T = 10)
ekeypar <- list(c_1 = 0.6, oC = 25, oG= 50, b = 30, i = 0.03, oI = 50, T = 10)
keynes <- Makrofigur('keynes')
#names(keynes$modellatr$keynes)
keynes$numerisk(endvar=c('ID', 'IDPGD', 'IDPGDPCD' ,'YS'), lukketpar=keypar, endrvar=Yv, kat='init')
keynes$numerisk(endvar=c('ID', 'IDPGD', 'IDPGDPCD' ,'YS'), lukketpar=ekeypar, endrvar=Yv, kat='endringG')
names(keynes$dfmodellres)

#
# keynes$grafisknum(samlikv=list(x=200, y=200))
#
# keynes$ggtyper[[1]]
#
# #keynes$grafiskstyle()
#
#
#
#
# Makrofigur$methods(enumerisk=function(lukketpar=NULL, openpar=NULL, endrpar=NULL){
# })
#
# Makrofigur$methods(egrafisk=function(){
#
# })
#
#
# Makrofigur$methods(grafiskstyle=function(){
#
#   ggobjsty <- ggtyper[[1]] + labs(title = 'Samtidig likevekt', x = NULL, y = NULL) #+
#
#   ggtyper <<- append(ggtyper,list(ggobjsty))
#
# })
#
# # library(R6)
# #
# #
# # Makrofigur <- R6Class("Makrofigur",
# #                       public = list(
# #                         label = NULL
# #                       ),
# #                       initialize = function(label= NA){
# #                         self$label <- label
# #                       }
# #                       )
# #
# # keynes <- Makrofigur$new
# #
# #
# # keynes
