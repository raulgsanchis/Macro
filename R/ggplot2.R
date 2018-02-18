#' @export dfgeneric
dfgeneric <- function(modell='adasl',labels = NULL, exoparval=NULL, eqsel = c(1,2)){

  #browser()
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

  } else if (modell =='islml'){
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

  }
  else if (modell =='islmo'){

    #browser()
    modellequ <- rjson::fromJSON(file=paste0(devtools::as.package(".")$path,'/inst/webside/jupyter/islmocequ.json'))

    # Fast kurs
    iisv <- eval(parse(text=modellequ$FastISC), exoparval)
    ilmv <- eval(parse(text=modellequ$FastLMC), exoparval)
    ibpv <- eval(parse(text=modellequ$FastBoP), exoparval)
    elmv <- eval(parse(text=modellequ$FlytLMC), exoparval)
    eisv <- eval(parse(text=modellequ$FlytISC), exoparval)
    eisbpv <- eval(parse(text=modellequ$FlytISCBoP), exoparval)
    ebpv <- eval(parse(text=modellequ$FastBoP), exoparval)

    # Melted
    dfmodellres <- data.frame(Iv, iisv, ilmv, ibpv, eisv, elmv, ebpv, eisbpv) %>%
      reshape2::melt(id.vars = c("Iv"))


    # Samtidig likevekt
    ## Samtidig likevekt
    # yss <- exoparval$Y
    # iss <- median(exoparval$i)
    # y <- c(yss,iss)
    exoparvalvd <- exoparval[1:length(exoparval)-1]
    # # #y <- c(yss, pss)
    # optadas <- function(y){
    #   c(Y1 = y[2] - eval(parse(text=modellequ$ISC), c(exoparvalvd, list(i=y[1]))),
    #     Y2 = y[2] - eval(parse(text=modellequ$LMC), c(exoparvalvd, list(i=y[1]))))}
    #
    #yeae <- c(1,1) #c(rootSolve::multiroot(f = optadas, start = c(iss, yss), positive = TRUE)$root, exoparvalvd$M)
    #exoparvalvd

    yeae <- c(eval(parse(text=modellequ$SEQFasti), exoparval), eval(parse(text=modellequ$SEQFastY), exoparval),
              eval(parse(text=modellequ$SEQFlytY), exoparval), eval(parse(text=modellequ$SEQFlytY), exoparval))

  }
  else if (modell =='adaso'){

    modellequ <- rjson::fromJSON(file=paste0(devtools::as.package(".")$path,'/inst/webside/jupyter/adasoequ.json'))

    # Selekterte modellligninger
    ## Enkeltligninger
    # adv <- eval(parse(text=modellequ$AD), exoparval)
    # asv <- eval(parse(text=modellequ$AS), exoparval)
    #
    # # Melted
    # dfmodellres <- data.frame(Iv, adv, asv) %>%
    #   reshape2::melt(id.vars = c("Iv"))
    #
    # # Samtidig likevekt
    # yss <- median(exoparval$Y)
    # pss <- exoparval$P
    # exoparvalvd <- exoparval[1:length(exoparval)-1]
    # #y <- c(yss, pss)
    # optadas <- function(y){
    #   c(Y1 = y[2] - eval(parse(text=modellequ$AD), c(exoparvalvd, list(Y=y[1]))),
    #     Y2 = y[2] - eval(parse(text=modellequ$AS), c(exoparvalvd, list(Y=y[1]))))}
    #
    # yeae <- rootSolve::multiroot(f = optadas, start = c(yss, pss))$root
  }
  else {
    print('Modell ikke funnet!')
  }

  #browser()

  varnavn <- as.character(unique(dfmodellres$variable))

  varnavnmaksverdi <- subset(dfmodellres, Iv ==rev(Iv)[1])
  varnavnminverdi <- subset(dfmodellres, Iv ==Iv[1])


  list(dfmodell=dfmodellres, yeae = yeae[eqsel], varnavn = varnavn,
       varnavnmaksverdi = varnavnmaksverdi, varnavnminverdi= varnavnminverdi)
}

#' @export genmakrofigure
genmakrofigure <- function(dfnumeric = NULL,
                           variables = NULL,
                           labt = labelsadas,
                           scalejust = list(x=0, y=0),
                           punktvelger = list(x=1,y=2),
                           limits= list(x=NULL, y=NULL),
                           linjetype = NULL){
  # Henter dataene
  datainp <- dplyr::filter(dfnumeric$dfmodell, variable %in% variables) #%>% dplyr::mutate(kat='naa')

  #browser()

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
    scale_y_continuous(breaks = c(dfnumeric$yeae[2], edfnumeric$yeae[2]), limits = limits$y, labels = c(labt$y0, elabt$y0)) +
    scale_x_continuous(breaks = c(dfnumeric$yeae[1], edfnumeric$yeae[1]), limits = limits$x, labels = c(labt$x0, elabt$x0)) +
    scale_colour_manual(values = labt$kurver$fargek) +
    theme_classic() +
    theme(legend.position="none")
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
