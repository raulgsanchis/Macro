#' @export genmakrofigure
genmakrofigure <- function(dfnumeric = NULL,
                           variables = NULL,
                           labt = labelsadas,
                           scalejust = list(x=0, y=0),
                           punktvelger = list(x=1,y=2),
                           limits= list(x=NULL, y=NULL)){
  # Henter dataene
  datainp <- dplyr::filter(dfnumeric$dfmodell, variable %in% variables) %>% dplyr::mutate(kat='naa')

  # Grafikk
  ggplot() +
    labs(title = labt$title, x = labt$x, y = labt$y) +
    geom_line(data = datainp, aes(x = Iv, y = value, color = factor(variable))) +
    geom_text(data = labt$kurver, aes(x = x, y = y, label = kurve), color = labt$kurver$fargel) +
    geom_point(aes(x=dfnumeric$yeae[punktvelger$x], y=dfnumeric$yeae[punktvelger$y])) +
    geom_segment(aes(x = dfnumeric$yeae[punktvelger$x], y = dfnumeric$yeae[punktvelger$y] ,
                    xend = dfnumeric$yeae[punktvelger$x], yend = scalejust$y), lty = 2) +
    geom_segment(aes(x = scalejust$x, y = dfnumeric$yeae[punktvelger$y], xend = dfnumeric$yeae[punktvelger$x],
                     yend = dfnumeric$yeae[punktvelger$y]), lty = 2) +
    scale_x_continuous(breaks = dfnumeric$yeae[c(1:length(labt$x0))], limits = limits$x, labels = labt$x0) +
    scale_y_continuous(breaks = dfnumeric$yeae[c(punktvelger$y)], limits = limits$y, labels = labt$y0) +
    scale_colour_manual(values = labt$kurver$fargek) +
    theme_classic() +
    theme(legend.position="none")

}

#' @export cgenmakrofigure
cgenmakrofigure <- function(dfnumeric=NULL,
                            edfnumeric=NULL,
                            variables = NULL,
                            labt = NULL,
                            elabt=NULL,
                            scalejust = list(x=0, y=0),
                            limits= list(x=NULL, y=NULL)){

  # Henter dataene
  datainp <- dplyr::filter(dfnumeric$dfmodell, variable %in% variables) %>% dplyr::mutate(kat='naa')
  odatainp <- dplyr::filter(edfnumeric$dfmodell, variable %in% variables) %>% dplyr::mutate(kat='naa')

  # Scalering
  #xscal <- c(dfnumeric$yeae[1], edfnumeric$yeae[1])
  #yscal <- c(dfnumeric$yeae[2], edfnumeric$yeae[2])

  # Grafikk
  ggplot() +
    labs(title = labt$title, x = labt$x, y = labt$y) +
    geom_line(data = datainp, aes(x = Iv, y = value, color = factor(variable))) +
    geom_line(data = odatainp, aes(x = Iv, y = value, color = factor(variable))) +
    geom_text(data = labt$kurver, aes(x = x, y = y, label = kurve), color = labt$kurver$fargel) +
    geom_text(data = elabt$kurver, aes(x = x, y = y, label = kurve), color = elabt$kurver$fargel) +
    geom_point(aes(x=dfnumeric$yeae[1], y=dfnumeric$yeae[2])) +
    geom_point(aes(x=edfnumeric$yeae[1], y=edfnumeric$yeae[2])) +
    geom_segment(aes(x =dfnumeric$yeae[1], y = dfnumeric$yeae[2] ,
                     xend = dfnumeric$yeae[1], yend = scalejust$y), lty = 2) +
    geom_segment(aes(x = scalejust$x, y = dfnumeric$yeae[2], xend = dfnumeric$yeae[1],
                     yend = dfnumeric$yeae[2]), lty = 2) +
    geom_segment(aes(x =edfnumeric$yeae[1], y = edfnumeric$yeae[2] ,
                     xend = edfnumeric$yeae[1], yend = scalejust$y), lty = 2) +
    geom_segment(aes(x = scalejust$x, y = edfnumeric$yeae[2], xend = edfnumeric$yeae[1],
                     yend = edfnumeric$yeae[2]), lty = 2) +
    scale_x_continuous(breaks = c(dfnumeric$yeae[1], edfnumeric$yeae[1]), limits = limits$x, labels = c(labt$x0, elabt$x0)) +
    scale_y_continuous(breaks = c(dfnumeric$yeae[2], edfnumeric$yeae[2]), limits = limits$y, labels = c(labt$y0, elabt$y0)) +
    scale_colour_manual(values = labt$kurver$fargek) +
    theme_classic() +
    theme(legend.position="none")
}

#' @export dfgeneric
dfgeneric <- function(modell='adasl',labels = NULL, exoparval=NULL, eqsel = c(1,2)){


  Iv <- as.vector(unlist(rev(exoparval)[1]))

  if (modell =='keynes'){
    # Keynes
    keynesequ <- rjson::fromJSON(file=paste0(devtools::as.package(".")$path,'/inst/webside/jupyter/keynesequ.json'))
    grad45v <- 0:rev(Iv)[1]
    cdv <- eval(parse(text=keynesequ$CD), exoparval)
    idv <- eval(parse(text=keynesequ$ID), exoparval)
    gdv <- eval(parse(text=keynesequ$GD), exoparval)

    # Linjer
    dfmodellres <- data.frame(Iv, grad45v, cdv, idv, gdv) %>%
      dplyr::mutate(gdvpidv = gdv + idv) %>%
      dplyr::mutate(cdvpidvgdv = cdv + idv + gdv) %>%
      reshape2::melt(id.vars = c("Iv"))

    # Likevekt
    yeae <- c(eval(parse(text=keynesequ$AD), exoparval), eval(parse(text=keynesequ$AD), exoparval))

  } else if (modell =='islm'){
    # Leser inn modellen
    modellequ <- rjson::fromJSON(file=paste0(devtools::as.package(".")$path,'/inst/webside/jupyter/islmequ.json'))
    # Selekterte modellligninger
    ## Enkeltligninger
    ldv <- eval(parse(text=modellequ$LD), exoparval)
    msv <- eval(parse(text=modellequ$MS), exoparval)
    isv <- eval(parse(text=modellequ$ISC), exoparval)
    lmv <- eval(parse(text=modellequ$LMC), exoparval)
    #exoparval$m <- 1
    #
    rrm <- eval(parse(text=modellequ$RREG), exoparval)
    rry <-  eval(parse(text=modellequ$RREG), list(m=exoparval$m, Y=isv))

    ## Samtidig likevekt
    yss <- exoparval$Y
    iss <- median(exoparval$i)
    y <- c(yss,iss)
    exoparvalvd <- exoparval[1:length(exoparval)-1]
    # #y <- c(yss, pss)
    optadas <- function(y){
      c(Y1 = y[2] - eval(parse(text=modellequ$ISC), c(exoparvalvd, list(i=y[1]))),
        Y2 = y[2] - eval(parse(text=modellequ$LMC), c(exoparvalvd, list(i=y[1]))))}

    yeae <- c(rootSolve::multiroot(f = optadas, start = c(iss, yss), positive = TRUE)$root, exoparvalvd$M)

    # Linjer
    dfmodellres <- data.frame(Iv, ldv, msv, isv, lmv, rry) %>%
      reshape2::melt(id.vars = c("Iv"))


  } else if (modell =='adasl'){
    # Leser inn modellen
    modellequ <- rjson::fromJSON(file=paste0(devtools::as.package(".")$path,'/inst/webside/jupyter/adascequ.json'))
    # Selekterte modellligninger
    ## Enkeltligninger
    adv <- eval(parse(text=modellequ$AD), exoparval)
    asv <- eval(parse(text=modellequ$AS), exoparval)

    # Melted
    dfmodellres <- data.frame(Iv, adv, asv) %>%
      reshape2::melt(id.vars = c("Iv"))

    # Samtidig likevekt
    yss <- median(exoparval$Y)
    pss <- exoparval$P
    exoparvalvd <- exoparval[1:length(exoparval)-1]
    #y <- c(yss, pss)
    optadas <- function(y){
      c(Y1 = y[2] - eval(parse(text=modellequ$AD), c(exoparvalvd, list(Y=y[1]))),
        Y2 = y[2] - eval(parse(text=modellequ$AS), c(exoparvalvd, list(Y=y[1]))))}

    yeae <- rootSolve::multiroot(f = optadas, start = c(yss, pss))$root

  } else {
    print('Modell ikke funnet!')
  }

  #browser()

  varnavn <- as.character(unique(dfmodellres$variable))

  varnavnmaksverdi <- subset(dfmodellres, Iv ==rev(Iv)[1])
  varnavnminverdi <- subset(dfmodellres, Iv ==Iv[1])


  list(dfmodell=dfmodellres, yeae = yeae[eqsel], varnavn = varnavn,
       varnavnmaksverdi = varnavnmaksverdi, varnavnminverdi= varnavnminverdi)
}

#' @export dfgpmakro
dfgpmakro <- function(Iv=NULL, exoparval=NULL, modell='keynes', endr=0){

  keynesequ <- rjson::fromJSON(file=paste0(devtools::as.package(".")$path,'/inst/webside/jupyter/keynesequ.json'))
  grad45v <- 0:rev(Iv)[1]
  cdv <- eval(parse(text=keynesequ$CD),exoparval)
  idv <- eval(parse(text=keynesequ$ID),exoparval)
  gdv <- eval(parse(text=keynesequ$GD),exoparval)

  # Linjer
  dfkeykryss <- data.frame(Iv, grad45v, cdv, idv, gdv) %>%
    dplyr::mutate(gdvpidv = gdv + idv) %>%
    dplyr::mutate(cdvpidvgdv = cdv + idv + gdv) %>%
    reshape2::melt(id.vars = c("Iv"))

  # Likevekt
  yeae <-eval(parse(text=keynesequ$AD), exoparval)
  xeae <-eval(parse(text=keynesequ$AD), exoparval)

  # Før eller etter
  scx <- NULL#list(breaksvx = c(yeae), labels = c(TeX(paste0("$Y_{",endr,"}$"))))
  scy <- NULL# list(breaksvy = c(yeae), labels = c(TeX(paste0("$X_{",endr,"}$"))))

  list(dfmodell=dfkeykryss, yeae=yeae, xeae=xeae, scx=scx, scy=scy)
}

#IS-LM
#' @export dfgpmakro2
dfgpmakro2 <- function(Iv=NULL, exoparval=exoparvalv, modell='is-lm', endr=0){

  #browser()

  # Leser inn modellen
  modellequ <- rjson::fromJSON(file=paste0(devtools::as.package(".")$path,'/inst/webside/jupyter/islmequ.json'))
  # Selekterte modellligninger
  ## Enkeltligninger
  ldv <- eval(parse(text=modellequ$LD), exoparval)
  msv <- eval(parse(text=modellequ$MS), exoparval)
  isv <- eval(parse(text=modellequ$ISC), exoparval)
  lmv <- eval(parse(text=modellequ$LMC), exoparval)

  ## Samtidig likevekt
  iss <- 3
  yss <- 300
  #y <- c(yss,iss)
  exoparvalvd <- exoparval[1:length(exoparval)-1]
  #y <- c(yss, pss)
  optadas <- function(y){
    c(Y1 = y[1] - eval(parse(text=modellequ$ISC), c(exoparvalvd, list(i=y[2]))),
      Y2 = y[1] - eval(parse(text=modellequ$LMC), c(exoparvalvd, list(i=y[2]))))}

  yeae <- nmtaggmodela <- rootSolve::multiroot(f = optadas, start = c(yss, iss))

  # Linjer
  dfmodellres <- data.frame(Iv, ldv, msv, isv, lmv) %>%
    reshape2::melt(id.vars = c("Iv"))

  # Før eller etter
  scx <- NULL#list(breaksvy = c(yeae), labels = c(TeX(paste0("$Y_{",endr,"}$"))))
  scy <- NULL#list(breaksvx = c(yeae), labels = c(TeX(paste0("$X_{",endr,"}$"))))

  list(dfmodell=dfmodellres, yeae = yeae, scx = NULL, scy = NULL)
}

#' AD-AS
#' @export dfgpmakro3
dfgpmakro3 <- function(Iv=NULL, exoparval=NULL, modell='ad-asc', endr=0){

  # Leser inn modellen
  modellequ <- rjson::fromJSON(file=paste0(devtools::as.package(".")$path,'/inst/webside/jupyter/adascequ.json'))
  # Selekterte modellligninger
  ## Enkeltligninger
  adv <- eval(parse(text=modellequ$AD), exoparval)
  asv <- eval(parse(text=modellequ$AS), exoparval)
  # Linjer
  dfmodellres <- data.frame(Iv, adv, asv) %>%
    reshape2::melt(id.vars = c("Iv"))


  yss <- median(exoparval$Y)
  pss <- exoparval$P
  exoparvalvd <- exoparval[1:length(exoparval)-1]
  #y <- c(yss, pss)
  optadas <- function(y){
    c(Y1 = y[2] - eval(parse(text=modellequ$AD), c(exoparvalvd, list(Y=y[1]))),
      Y2 = y[2] - eval(parse(text=modellequ$AS), c(exoparvalvd, list(Y=y[1]))))}

  yeae <- rootSolve::multiroot(f = optadas, start = c(yss, pss))$root


  # Før eller etter
  scx <- NULL#list(breaksvy = c(yeae), labels = c(TeX(paste0("$Y_{",endr,"}$"))))
  scy <- NULL#list(breaksvx = c(yeae), labels = c(TeX(paste0("$X_{",endr,"}$"))))

  list(dfmodell=dfmodellres, yeae = yeae, scx = NULL, scy = NULL)
}

#' @export makrofigure
makrofigure <- function(ndata = datakeynes,
                        labt = list(title= 'Testplot', x='x-variabel', y='y-variabel'),
                        variables = c('grad45','id'),
                        labplassmon = data.frame(labeling=c('45 grader'), x=c(60), y = c(20)),
                        equisol = list(x = c(299.75, 310), y = c(299.75, 345)),
                        scalebreaksx = list(breaksvx = c(1,10), labels = c('xxx',TeX('$X_{0}$'))),
                        scalebreaksy = list(breaksvy = c(1,10), labels = c('yyy',TeX('$Y_{0}$'))),
                        colorl = NULL,
                        starts = list(x=0,y=0)){


  # Henter dataene
  datainp <- dplyr::filter(ndata, variable %in% variables) %>% dplyr::mutate(kat='naa')

  # Plotte dataene
  ggplot() +
    geom_line(data = datainp, aes(x = Iv, y = value, color = factor(variable))) +
    geom_point(aes(x=equisol$x, y=equisol$y)) +
    geom_text(data = labplassmon, aes(x = x, y = y, label = labeling), color = labplassmon$col) +
    labs(title = labt$title, x = labt$x, y = labt$y) +
    geom_segment(aes(x = equisol$x, y = 0, xend = equisol$x , yend = equisol$y), lty = 2) +
    geom_segment(aes(x = 0, y = equisol$y, xend = equisol$x , yend = equisol$y), lty = 2) +
    scale_x_continuous(breaks = starts$x, labels = scalebreaksx$labels) +
    scale_y_continuous(breaks = starts$y, labels = scalebreaksy$labels) +
    scale_colour_manual(values = colorl) +
    theme_classic() +
    theme(legend.position="none")
}

#' Komparativ
#' @export gpdiamdoell3
makrofigurechange <- function(ndata = datakeynes,
                              labt = list(title= 'Testplot', x='x-variabel', y='y-variabel'),
                              variables = c('grad45','id'),
                              labplassmon = data.frame(labeling=c('45 grader'), x=c(60), y = c(20)),
                              equisol = list(x = c(299.75, 310), y = c(299.75, 345)),
                              scalebreaksx = list(breaksvx = c(1,10), labels = c('xxx',TeX('$X_{0}$'))),
                              scalebreaksy = list(breaksvy = c(1,10), labels = c('yyy',TeX('$Y_{0}$'))),
                              colorl = NULL,
                              odata = NULL,
                              ovariables = NULL,
                              starts = list(x=0,y=0)){

  # Henter dataene
  datainp <- dplyr::filter(ndata, variable %in% variables) %>% dplyr::mutate(kat='naa')
  odatainp <- dplyr::filter(odata, variable %in% c(ovariables)) %>% dplyr::mutate(kat='foer')

  #browser()

  # Plotte dataene
 ggplot() +
    labs(title = labt$title, x = labt$x, y = labt$y) +
    geom_line(data = datainp, aes(x = Iv, y = value, color = factor(variable))) +
    geom_line(data = odatainp, aes(x = Iv, y = value, color = factor(variable))) +
    geom_text(data = labplassmon, aes(x = x, y = y, label = labeling), color = labplassmon$col) +
    geom_point(aes(x=equisol$x, y=equisol$y)) +
    geom_segment(aes(x = equisol$x, y = starts$y, xend = equisol$x , yend = equisol$y), lty = 2) +
    geom_segment(aes(x = starts$x, y = equisol$y, xend = equisol$x , yend = equisol$y), lty = 2) +
    scale_x_continuous(breaks = scalebreaksx$breaksvx, labels = scalebreaksx$labels)  +
    scale_y_continuous(breaks = scalebreaksy$breaksvy, labels = scalebreaksy$labels) +
    scale_colour_manual(values = colorl) +
    theme_classic() +
    theme(legend.position="none")

}



#' Tidsserier
#' @export gpmakro
gpmakro <- function(data = moltmacrousa, variables = c('lngdp', 'hptrendg'), katv = c('gdp'),
                    startp="1929-04-01", endp =" 2017-12-12",
                    variabeln = c('var1', 'var2'),
                    labt = list(x='x-variabel', y='y-variabel')){
  # Filtrere datasettet
  datainp <- dplyr::filter(data, variable %in% variables, kat %in% katv, date >= startp & date <= endp)
  # Lager plot
  ggplot(data = datainp, aes(x = date, y = value, color = factor (variable, labels = variabeln))) +   geom_line() + theme_classic() +
    labs(color = "Variabel:",x = labt$x, y = labt$y) +
    labs(x = labt$x, y = labt$y)
}


#' Grafter
#' @export gpdiamdoell
gpdiamdoell <- function(data = datakeynes,
                        variables = c('grad45','id'),
                        labt = list(title= 'Keyneskrysset', x='x-variabel', y='y-variabel'),
                        equisol = list(x = 299.75, y = 299.75),
                        color = NULL,
                        labplassmon = data.frame(labeling=c('45 grader'), x=c(60), y = c(20)),
                        scalebreaksx = list(breaksvx = c(1,10), labels = c('xxx',TeX('$X_{0}$'))),
                        scalebreaksy = list(breaksvy = c(1,10), labels = c('yyy',TeX('$Y_{0}$'))),
                        odata = NULL,
                        ovariables = NULL){

  # Henter dataene
  datainp <- dplyr::filter(data, variable %in% variables)
  odatainp <- dplyr::filter(datainp, variable %in% c(ovariables))

  # Plotte dataene
  ggplot() +
    geom_text(data = labplassmon, aes(x = x, y = y, label = labeling), color = as.character(labplassmon$col)) +
    geom_line(data = datainp, aes(x = Iv, y = value, color = factor(variable))) +
    geom_line(data = odatainp, aes(x = Iv, y = value, color = factor(variable))) +
    geom_point(aes(x=equisol$x, y=equisol$y)) +
    labs(title = labt$title, x = labt$x, y = labt$y) +
    scale_colour_manual(values = color) +
    theme_classic() +
    geom_segment(aes(x = equisol$x, y = 0, xend = equisol$x , yend = equisol$y), lty = 2) +
    geom_segment(aes(x = 0, y = equisol$y, xend = equisol$x , yend = equisol$y), lty = 2) +
    scale_x_continuous(breaks =scalebreaksx$breaksvx, labels = scalebreaksx$labels) +
    scale_y_continuous(breaks = scalebreaksy$breaksvy, labels = scalebreaksx$labels) +
    theme(legend.position="none") +
    coord_cartesian()
  # Old
}




#' Grafter
#' @export gpdiamdoell2
gpdiamdoell2 <- function(data = NULL,
                         variables = NULL,
                         labt = list(title= NULL, x=NULL, y=NULL),
                         equisol = list(x = NULL, y = NULL),
                         color = NULL,
                         labplassmon = data.frame(labeling=c(''), x=c(), y = c()),
                         scalebreaksx = list(breaksvx = c(1,10), labels = c('xxx',TeX('$X_{0}$'))),
                         scalebreaksy = list(breaksvy = c(1,10), labels = c('yyy',TeX('$Y_{0}$')))){

  # Henter dataene
  datainp <- dplyr::filter(data, variable %in% variables)

  # Plotte dataene
  ggplot() +
    geom_line(data = datainp, aes(y = Iv, x = value, color = factor(variable))) +
    geom_point(aes(x=equisol$x, y=equisol$y)) +
    geom_text(data = labplassmon, aes(x = x, y = y, label = labeling), color = as.character(labplassmon$col)) +
    labs(title = labt$title, x = labt$x, y = labt$y) +
    scale_colour_manual(values = color) +
    theme_classic() +
    geom_segment(aes(x = equisol$x, y = 0, xend = equisol$x , yend = equisol$y), lty = 2) +
    geom_segment(aes(x = 0, y = equisol$y, xend = equisol$x , yend = equisol$y), lty = 2) +
    scale_x_continuous(breaks =scalebreaksx$breaksvx, labels = scalebreaksx$labels) +
    scale_y_continuous(breaks = scalebreaksy$breaksvy, labels = scalebreaksx$labels) +
    theme(legend.position="none") +
    coord_cartesian()
}

