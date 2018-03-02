Genfigur <- setRefClass("Genfigur", fields = list(modell='character',
                                                  exoparval='vector',
                                                  modellatr='vector',
                                                  plotvectorend='list',
                                                  dfmodellres='list',
                                                  optimeringv='list'))

Genfigur$methods(initialize=function(modellnavn='solow'){

  solowd <- list(sy=expression('savr*k^(alpha)'), y=expression(k^(alpha)), depk=expression(k*(n+gamma)))

  modellatr <<- list(solow=solowd)

  modell <<- c(modellnavn)

})

Genfigur$methods(numerisk=function(vartegne=c('sy','y','depk'), par=list(savr=0.3, alpha=0.5, n=0, gamma=0.03), endvar=list(k=1:200),kat='solow'){

  exoparval <<- c(par,endvar)

  plotvectorend <<- list()
  for(mvar in vardraw){
    # mvar <- vardraw[3]
    print(paste0(mvar))
    endv <- list(eval(parse(text=  modellatr[[modell]][[mvar]]), exoparval))
    plotvectorend <<- append(plotvectorend, endv)
  }

  names(plotvectorend) <<- vartegne

  DFmodellres <- data.frame(Iv= endvar[[1]], plotvectorend) %>% reshape2::melt(id.vars = c("Iv"))

  dfmodellres[[kat]] <<- DFmodellres

})

Genfigur$methods(optimering=function(tovectorlabel=c('sy', 'y'), startv=c(10,10)){

  browser()

  rearexoparval <- rev(exoparval)[-1]
  y <- c(startv[1], startv[2])
  optolign <- function(y){

    sokevar <- setNames(as.list(c(y[1])), c(names(rev(exoparval)[1])))

    c(Y1 = y[2] - eval(parse(text=modellatr[[modell]][tovectorlabel[1]]), c(rearexoparval, sokevar)),
      Y2 = y[2] - eval(parse(text=modellatr[[modell]][tovectorlabel[2]]), c(rearexoparval, sokevar)))
  }

  yeae <- c(rootSolve::multiroot(f = optolign, start = y, positive = TRUE)$root)

  optimeringv <<- append(optimeringv,list(yeae))

})

Genfigur$methods(grafisknumappend=function(samlikve=list(x=0, y=0),  dftekst=NULL, manuell=1,
                                             tilstand=NULL){

  browser()

  samlikvedf <- data.frame(x=samlikve$x, y=samlikve$y)

  # ggobjnumapp <- ggtyper[[length(ggtyper)]] +
  #   geom_point(data=samlikvedf,aes(x=x, y=y)) +
  #   geom_segment(data=samlikvedf,aes(x = x, y = y ,xend = x, yend =  dftekst$xlim[1]), lty = 2) +
  #   geom_segment(data=samlikvedf, aes(x = x, y = y ,xend = dftekst$ylim[1], yend = y), lty = 2) +
  #   geom_line(data = dfmodellres[[tilstand]],
  #             aes(x = Iv, y = value, color = factor(variable))) +
  #   geom_text(data=dftekst, aes(x, y, label=kurve), color=dftekst$farge)
  #
  # ggtyper <<- append(ggtyper,list(ggobjnumapp))

})

Genfigur$methods(grafiskstyle=function(labs=list(title=NULL, x=NULL, y=NULL),
                                         skaleringx=NULL,
                                         skaleringy=NULL,
                                         fargelinje=c('black','black'),
                                         figurnr = NULL){

  # #browser()
  # nrfigur <- c(length(ggtyper), figurnr)[ifelse(is.null(figurnr)==TRUE,1,2)]
  #
  # ggobjsty <- ggtyper[[nrfigur]] + labs(title = labs$title, x = labs$x, y = labs$y) +
  #   scale_x_continuous(labels = skaleringx$label, breaks=skaleringx$breaks, limits=skaleringx$limits) +
  #   scale_y_continuous(labels = skaleringy$label, breaks=skaleringy$breaks, limits=skaleringy$limits) +
  #   scale_colour_manual(values =fargelinje) +
  #   theme_classic() + theme(legend.position="none")
  #
  # ggtyper <<- append(ggtyper,list(ggobjsty))

})






#' A class description
#'
#' @import methods
#' @export Makrofigur
#' @exportClass Makrofigur
Makrofigur <- setRefClass("Makrofigur", fields = list(modell='vector', modellatr ='list',
                                                      plotvectorend='list', dfmodellres='list',
                                                      ggtyper='list',
                                                      exoparval='vector',
                                                      optimeringv='list'))

Makrofigur$methods(initialize=function(modellnavn=NULL){

  keynes <- rjson::fromJSON(file=paste0(devtools::as.package(".")$path,'/inst/webside/jupyter/keynesequ.json'))
  islml <- rjson::fromJSON(file=paste0(devtools::as.package(".")$path,'/inst/webside/jupyter/islmequ.json'))
  islmo <- rjson::fromJSON(file=paste0(devtools::as.package(".")$path,'/inst/webside/jupyter/islmocequ.json'))
  adasl <- rjson::fromJSON(file=paste0(devtools::as.package(".")$path,'/inst/webside/jupyter/adascequ.json'))
  adaso <- rjson::fromJSON(file=paste0(devtools::as.package(".")$path,'/inst/webside/jupyter/adasoequ.json'))

  modellatr <<- list(keynes=keynes, islml=islml, islmo=islmo, adasl=adasl, adaso=adaso)

  modell <<- c(modellnavn)

})


Makrofigur$methods(numerisk=function(endvar=NULL ,lukketpar=NULL, openpar=NULL, endrvar = NULL, kat=NULL){

  exoparval <<- c(lukketpar, openpar, endrvar)

  #browser()

  plotvectorend <<- list()
  for(mvar in endvar){
    # mvar <- endvar[1]
    endv <- list(eval(parse(text=  modellatr[[modell]][[mvar]]), exoparval))
    plotvectorend <<- append(plotvectorend, endv)
  }

  names(plotvectorend) <<- endvar

  DFmodellres <- data.frame(Iv= endrvar[[1]], plotvectorend) %>% reshape2::melt(id.vars = c("Iv"))

  dfmodellres[[kat]] <<- DFmodellres

})

Makrofigur$methods(grafisknum=function(samlikv=list(x=NULL, y=NULL), dftekst=NULL, manuell=1){

  #browser()

  ggobjnum <- ggplot() + geom_line(data = dfmodellres[[1]], aes(x = Iv, y = value, color = factor(variable))) +
  geom_point(aes(x=samlikv$x, y=samlikv$y)) +
  geom_segment(aes(x = samlikv$x, y = samlikv$y ,
                   xend = samlikv$x, yend = dftekst$xlim[1]), lty = 2) +
  geom_segment(aes(x = samlikv$x, y = samlikv$y ,
                   xend = dftekst$ylim[1], yend = samlikv$y), lty = 2) +
  geom_text(data=dftekst, aes(x, y, label=kurve), color=dftekst$farge)

  #browser()

  ggtyper <<- list(ggobjnum)

})

Makrofigur$methods(grafisknumappend=function(samlikve=list(x=0, y=0),  dftekst=NULL, manuell=1,
                                             tilstand=NULL){

  #browser()

  samlikvedf <- data.frame(x=samlikve$x, y=samlikve$y)

  ggobjnumapp <- ggtyper[[length(ggtyper)]] +
      geom_point(data=samlikvedf,aes(x=x, y=y)) +
      geom_segment(data=samlikvedf,aes(x = x, y = y ,xend = x, yend =  dftekst$xlim[1]), lty = 2) +
      geom_segment(data=samlikvedf, aes(x = x, y = y ,xend = dftekst$ylim[1], yend = y), lty = 2) +
    geom_line(data = dfmodellres[[tilstand]],
              aes(x = Iv, y = value, color = factor(variable))) +
    geom_text(data=dftekst, aes(x, y, label=kurve), color=dftekst$farge)

  ggtyper <<- append(ggtyper,list(ggobjnumapp))

})

Makrofigur$methods(grafiskstyle=function(labs=list(title=NULL, x=NULL, y=NULL),
                                         skaleringx=NULL,
                                         skaleringy=NULL,
                                         fargelinje=c('black','black'),
                                         figurnr = NULL){

  #browser()
  nrfigur <- c(length(ggtyper), figurnr)[ifelse(is.null(figurnr)==TRUE,1,2)]

  ggobjsty <- ggtyper[[nrfigur]] + labs(title = labs$title, x = labs$x, y = labs$y) +
    scale_x_continuous(labels = skaleringx$label, breaks=skaleringx$breaks, limits=skaleringx$limits) +
    scale_y_continuous(labels = skaleringy$label, breaks=skaleringy$breaks, limits=skaleringy$limits) +
    scale_colour_manual(values =fargelinje) +
    theme_classic() + theme(legend.position="none")

  ggtyper <<- append(ggtyper,list(ggobjsty))

})

Makrofigur$methods(optimering=function(tovectorlabel=c('SEQFastY', 'IAS'), startv=c(1,100)){

  #browser()

  rearexoparval <- rev(exoparval)[-1]
  y <- c(startv[1], startv[2])
  optolign <- function(y){

    sokevar <- setNames(as.list(c(y[1])), c(names(rev(exoparval)[1])))

    c(Y1 = y[2] - eval(parse(text=modellatr[[modell]][tovectorlabel[1]]), c(rearexoparval, sokevar)),
      Y2 = y[2] - eval(parse(text=modellatr[[modell]][tovectorlabel[2]]), c(rearexoparval, sokevar)))
  }

  yeae <- c(rootSolve::multiroot(f = optolign, start = y, positive = TRUE)$root)

  optimeringv <<- append(optimeringv,list(yeae))

})

