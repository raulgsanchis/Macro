# Keyneskrysset
mlabels <- list(title= 'Keyneskrysset', x='produksjon, inntekt (Y)', y='aggregert etterspørsel (AE)')
# Parameterverdier (Keyneskrysset)
Iv <- 0:600
exoparval <- c(list(c_1 = 0.6, oC = 25, oG= 50, a = 30, i = 0.03, oI = 50, T = 10), list(Y=c(Iv)))
cexoparval <- c(list(c_1 = 0.6, oC = 25, oG= 50, a = 30, i = 0.03, oI = 90, T = 10), list(Y=c(Iv)))
dfkeynes <- dfgpmakro(Iv, exoparval, endr=0)
cdfkeynes <- dfgpmakro(Iv, cexoparval, endr=1)
eqsol <- list(x= c(dfkeynes$yeae), y = c(dfkeynes$yeae))
ceqsol <- list(x= c(dfkeynes$yeae, cdfkeynes$yeae), y = c(dfkeynes$yeae, cdfkeynes$yeae))
eqlinjey <- list(breaksvy = eqsol$y, labels = c(TeX('$AE_{0}$')))
eqlinjex <- list(breaksvx = eqsol$x, labels = c(TeX('$Y_{0}$')))
unique(dfkeynes$dfmodell$variable)

rev(subset(dfkeynes$dfmodell, variable == 'gdv')$value)[1]
rev(subset(dfkeynes$dfmodell, variable == 'gdvpidv')$value)[1]
rev(subset(dfkeynes$dfmodell, variable == 'cdvpidvgdv')$value)[1]
rev(subset(dfkeynes$dfmodell, variable == 'grad45v')$value)[1]
labldf <- data.frame(labeling=c("45 grader", "I", "C + I", "C + I + G", "Y = AE"),
                     x = c(105, 560, 560, 560, 560),
                     y = c(20, 50+10, 100+10, 480+10, 600+10),
                     col = c('black', rep(c('red'),1,4)))

keynesc <- makrofigure(ndata = dfkeynes$dfmodell,
                       labt = mlabels,
                       variables = c('grad45v', 'gdv', 'gdvpidv','cdvpidvgdv'),
                       labplassmon = labldf,
                       equisol = eqsol,
                       scalebreaksx = eqlinjex,
                       scalebreaksy = eqlinjey,
                       colorl = c(rep('black',4)))

ggsave(paste0(devtools::as.package(".")$path,'/inst/webside/figurer/keynesc.png'))


###
rev(subset(dfkeynes$dfmodell, variable == 'cdvpidvgdv')$value)[1]
ceqlinjey <- list(breaksvy = ceqsol$y, labels = c(TeX('$AE_{0}$'), TeX('$AE_{1}$')))
ceqlinjex <- list(breaksvx = ceqsol$x, labels = c(TeX('$Y_{0}$'), TeX('$Y_{i}$')))
rev(subset(cdfkeynes$dfmodell, variable == 'cdvpidvgdv')$value)[1]
rev(subset(cdfkeynes$dfmodell, variable == 'grad45v')$value)[1]
labldf <- data.frame(labeling=c("45 grader", "C + I + G", "C+ I' + G", "Y=AE"),
                     x = c(105, 560, 560, 560),
                     y = c(20, 480+5, 520+5, 600+5),
                     col = c('black', rep(c('red'),1,3)))


keynesccinv <- makrofigurechange(ndata = dfkeynes$dfmodell,
                                 labt = mlabels,
                                 variables = c('grad45v','cdvpidvgdv'),
                                 labplassmon = labldf,
                                 equisol = ceqsol,
                                 scalebreaksx = ceqlinjex,
                                 scalebreaksy = ceqlinjey,
                                 colorl = c(rep('black',3)),
                                 odata = cdfkeynes$dfmodell,
                                 ovariables = c('cdvpidvgdv'))


ggsave(paste0(devtools::as.package(".")$path,'/inst/webside/figurer/keynesccinv.png'))
