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
                     scalebreaksy = list(breaksvy = c(1,10), labels = c('yyy',TeX('$Y_{0}$')))){

    browser()

    # Henter dataene
    datainp <- dplyr::filter(data, variable %in% variables)

    # Plotte dataene
    ggplot() +
    geom_line(data = datainp, aes(x = Yv, y = value, color = factor(variable))) +
    geom_text(data = labplassmon, aes(x = x, y = y, label = labeling), color = labldf$col) +
    geom_point(aes(x=equisol$x, y=equisol$y)) +
    labs(title = labt$title, x = labt$x, y = labt$y) +
    scale_colour_manual(values = color) +
    theme_classic() +
    geom_segment(aes(x = equisol$x, y = 0, xend = equisol$x , yend = equisol$y), lty = 2) +
    geom_segment(aes(x = 0, y = equisol$y, xend = equisol$x , yend = equisol$y), lty = 2) +
    scale_x_continuous(name=labt$x, breaks = scalebreaksx$breaksvx, labels = scalebreaksx$labels ) +
    scale_y_continuous(name=labt$y, breaks = scalebreaksy$breaksvy, labels = scalebreaksy$labels ) +
    theme(legend.position="none") +
    coord_cartesian()

}

#' IS-LM
dfgpmakro <- function(Yv=NULL, exoparval=NULL, modell='keynes', endr=0){

  keynesequ <- rjson::fromJSON(file=paste0(devtools::as.package(".")$path,'/inst/webside/jupyter/keynesequ.json'))
  grad45v <- 0:rev(Yv)[1]
  cdv <- eval(parse(text=keynesequ$CD),exoparval)
  idv <- eval(parse(text=keynesequ$ID),exoparval)
  gdv <- eval(parse(text=keynesequ$GD),exoparval)

  # Linjer
  dfkeykryss <- data.frame(Yv, grad45v, cdv, idv, gdv) %>%
    dplyr::mutate(gdvpidv = gdv + idv) %>%
    dplyr::mutate(cdvpidvgdv = cdv + idv + gdv) %>%
    reshape2::melt(id.vars = c("Yv"))

  # Likevekt
  yeae <-eval(parse(text=keynesequ$AD), exoparval)

  # Før eller etter
  scx <- list(breaksvy = c(yeae), labels = c(TeX(paste0("$Y_{",endr,"}$"))))
  scy <- list(breaksvx = c(yeae), labels = c(TeX(paste0("$X_{",endr,"}$"))))

  list(dfmodell=dfkeykryss, yeae=yeae, scx=scx, scy=scy)
}






