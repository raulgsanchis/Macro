iv <- 0:7.5
exoparvalv <- c(list(c_1 = 0.6, oC = 25, oG= 75, b = 15, oI = 75, T = 10, M= 200, P=1, h = 80, k =2, Y = 300), list(i=c(iv)))
cexoparvalv <- c(list(c_1 = 0.6, oC = 25, oG= 75, b = 15, oI = 75, T = 10, M= 275, P=1, h = 80, k =2, Y = 300), list(i=c(iv)))
dfislm <- dfgpmakro2(Iv=iv, exoparval = exoparvalv, endr=0)
cdfislm <- dfgpmakro2(Iv=iv, exoparval = cexoparvalv, endr=0)

# Pengemarkedet
mlabels <- list(title= 'Pengemarkedet', y='Pengetilbudet (M/P)', x='rentenivå (i)')
labldf <- labldf <- data.frame(labeling=c("Ms", "Ld"),
                               x = c(7.5, 1),
                               y = c(200+30, 600),
                               col = c(rep(c('red'),1,2)))
eqsol <- list(x= c(5), y = c(200))
eqlinjey <- list(breaksvy = eqsol$y, labels = c(TeX('$M_{0}/P_{0}$')))
eqlinjex <- list(breaksvx = eqsol$x, labels = c(TeX('$i_{0}$')))

figpengem <- makrofigure(ndata = dfislm$dfmodell,
                         variables = c("ldv", "msv"),
                         labt = mlabels,
                         labplassmon = labldf,
                         equisol = eqsol,
                         scalebreaksx = eqlinjex,
                         scalebreaksy = eqlinjey,
                         colorl = c(rep('black',2))) + coord_flip()

ggsave(paste0(devtools::as.package(".")$path,'/inst/webside/figurer/figpengem.png'))


## Endring
ceqsol <- list(x= c(5, 4), y = c(200, 275))
ceqlinjey <- list(breaksvy = ceqsol$y, labels = c(TeX('$M/P_{0}$'), TeX('$M/P_{1}$')))
ceqlinjex <- list(breaksvx = ceqsol$x, labels = c(TeX('$i_{0}$'), TeX('$i_{1}$')))
labldf <- data.frame(labeling=c("Ms", "Ms'", "Ld"),
                     x = c(7.5, 7.5, 1),
                     y = c(200, 200+80, 600),
                     col = c(rep('red',3)))
eqlinjey <- list(breaksvy = eqsol$y, labels = c(TeX('$M_{0}/P_{0}$')))
eqlinjex <- list(breaksvx = eqsol$x, labels = c(TeX('$i_{0}$')))

figpengemchm <- makrofigurechange(ndata = dfislm$dfmodell,
                                  labt = mlabels,
                                  variables = c("ldv", "msv"),
                                  labplassmon = labldf,
                                  equisol = ceqsol,
                                  scalebreaksx = ceqlinjex,
                                  scalebreaksy = ceqlinjey,
                                  colorl = c(rep('black',3)),
                                  odata = cdfislm$dfmodell,
                                  ovariables = c("ldv", "msv")) + coord_flip()

ggsave(paste0(devtools::as.package(".")$path,'/inst/webside/figurer/figpengemchm.png'))

## Merkelapper
islmlabels <- list(title= 'IS-LM modellen', x='rentenivå (i)', y='produksjon, inntekt (Y)')
lableg <- list(title= 'IS-LM modellen', x='Y', y='i')
eqsol <- list(x= c(4.2), y = c(268))
rev(subset(dfislm$dfmodell, variable == 'isv')$value)[1]
rev(subset(dfislm$dfmodell, variable == 'lmv')$value)[1]
labldfislm <- labldf <- data.frame(labeling=c("IS-kurven", "LM-kurven"),
                                   x = c(7, 2),
                                   y = c(160+40, 380+20),
                                   col = c(rep(c('red'),1,2)))

eqlinjey <- list(breaksvy = eqsol$y, labels = c(TeX('$Y_{0}$')))
eqlinjex <- list(breaksvx = eqsol$x, labels = c(TeX('$i_{0}$')))

figislm <- makrofigure(ndata = dfislm$dfmodell,
                       variables = c("isv", "lmv"),
                       labt = islmlabels,
                       labplassmon = labldfislm,
                       equisol = eqsol,
                       scalebreaksx = eqlinjex,
                       scalebreaksy = eqlinjey,
                       color = rep(c('black'),2)) + coord_flip()

ggsave(paste0(devtools::as.package(".")$path,'/inst/webside/figurer/figislm.png'))

## Endring
ceqsol <- list(x= c(3.7, 4.15), y = c(284, 264))
equisol = ceqsol
ceqlinjey <- list(breaksvy = ceqsol$y, labels = c(TeX('$Y_{1}$'), TeX('$Y_{0}$')))
ceqlinjex <- list(breaksvx = ceqsol$x, labels = c(TeX('$i_{1}$'), TeX('$i_{0}$')))

labldfislm <- labldf <- data.frame(labeling=c("LM-kurven'", "IS-kurven"),
                                   x = c(7.5, 2),
                                   y = c(400, 380+20),
                                   col = c(rep(c('red'),1,2)))

figislmxchm <- makrofigurechange(ndata = dfislm$dfmodell,
                                 variables = c("isv", "lmv"),
                                 labt = islmlabels,
                                 labplassmon = labldfislm,
                                 equisol = ceqsol,
                                 scalebreaksx = ceqlinjex,
                                 scalebreaksy = ceqlinjey,
                                 odata = cdfislm$dfmodell,
                                 ovariables = c("lsv", "lmv"),
                                 color = rep('black',4)) + coord_flip()

grid.arrange(figpengemchm, figislmxchm , ncol= 2)

ggsave(paste0(devtools::as.package(".")$path,'/inst/webside/figurer/figislmxchm.png'))
