library(MakroOEKB1115)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(latex2exp)
#######################################################################################################################
iv <- 0:7.5
islmexoparvalv <- c(list(c_1 = 0.6, oC = 25, oG= 75, b = 15, oI = 75, T = 10, M= 200, P=1, h = 80, k =2, Y = 300), list(i=c(iv)))
eislmexoparvalv <- c(list(c_1 = 0.6, oC = 25, oG= 75, b = 15, oI = 75, T = 10, M= 275, P=1, h = 80, k =2, Y = 300), list(i=c(iv)))
edfislm <- dfgeneric(modell='islm', exoparval = islmexoparvalv)
dfislm <- dfgeneric(modell='islm', exoparval = eislmexoparvalv)

# Pengemarkedet
dfkurver <- data.frame(kurve=c("Ld", "Ms"),
                       fargel = c('red', 'red'),
                       fargek = c('red', 'red'),
                       y = dfislm$varnavnminverdi$value[c(1,2)],
                       x = c(dfislm$varnavnminverdi$Iv[c(1,2)]))


labelsislm <- list(title = 'Pengemarkedet',
                   y = 'produksjon, inntekt (Y)',
                   x = 'rentenivå (i)',
                   x0 = c(TeX('$i_{0}}$')),
                   y0 = c(TeX('$M_{0}/P_{0}$')),
                   kurver = dfkurver)

figpengem <- genmakrofigure(dfnumeric=dfislm,
                            variables = c(dfislm$varnavn)[c(1,2)],
                            labt = labelsislm,
                            scalejust = list(x=0, y=0))  + coord_flip()

emmdfkurver = data.frame(kurve=c("LM"),
                         fargel = c('red'),
                         fargek = c('red'),
                         x = edfislm$varnavnmaksverdi$Iv[c(2)],
                         y = edfislm$varnavnmaksverdi$value[c(2)])

elabelsislm <- list(title = 'Pengemarkedet',
                    y = 'produksjon, inntekt (Y)',
                    x = 'rentenivå (i)',
                    x0 = c(TeX('$i_{1}}$')),
                    y0 = c(TeX('$M/P_{1}$')),
                    kurver = emmdfkurver)


monpolichange <- cgenmakrofigure(dfnumeric=dfislm,
                                 edfnumeric=edfislm,
                                 variables = c(dfislm$varnavn)[c(1,2)],
                                 labt = labelsislm,
                                 elabt = elabelsislm,
                                 scalejust = list(x=0, y=0)) + coord_flip()


# IS-LM Modellen
### Likevekt ###
#!: endogenisere gjetteverdier
dfkurver <- data.frame(kurve=c("IS", "LM"),
                       fargel = c('red', 'red'),
                       fargek = c('red', 'red'),
                       y = c(dfislm$varnavnminverdi$value[c(3)],dfislm$varnavnmaksverdi$value[c(4)]),
                       x = c(dfislm$varnavnminverdi$Iv[c(3)],dfislm$varnavnmaksverdi$Iv[c(4)]))

labelsislm <- list(title = 'IS-LM modellen',
                   y = 'produksjon, inntekt (Y)',
                   x = 'rentenivå (i)',
                   x0 = c(TeX('$i_{0}}$')),
                   y0 = c(TeX('$Y_{0}$')),
                   kurver = dfkurver)

islmlikevekt <- genmakrofigure(dfnumeric=dfislm,
                               variables = c(dfislm$varnavn)[c(3,4)],
                               labt = labelsislm,
                               scalejust = list(x=0, y=75))  + coord_flip()
islmlikevekt

### Komparativ statikk ###
dfkurverislm <- data.frame(kurve=c("LM"),
                       fargel = c('red'),
                       fargek = c('red'),
                       y = c(dfislm$varnavnminverdi$value[c(3)]),
                       x = c(dfislm$varnavnminverdi$Iv[c(3)]))

elabelsislm <- list(title = 'IS-LM modellen',
                   y = 'produksjon, inntekt (Y)',
                   x = 'rentenivå (i)',
                   x0 = c(TeX('$i_{1}}$')),
                   y0 = c(TeX('$Y_{1}$')),
                   kurver = dfkurverislm)

emmdfkurver = data.frame(kurve=c("LM"),
                         fargel = c('red'),
                         fargek = c('red'),
                         x = edfislm$varnavnmaksverdi$Iv[c(2)],
                         y = edfislm$varnavnmaksverdi$value[c(2)])

elabelsislm <- list(title = 'Pengemarkedet',
                    y = 'produksjon, inntekt (Y)',
                    x = 'rentenivå (i)',
                    x0 = c(TeX('$i_{1}}$')),
                    y0 = c(TeX('$M/P_{1}$')),
                    kurver = emmdfkurver)

monpolichange <- cgenmakrofigure(dfnumeric=dfislm,
                                 edfnumeric=edfislm,
                                 variables = c(dfislm$varnavn)[c(1,2)],
                                 labt = labelsislm,
                                 elabt = elabelsislm,
                                 scalejust = list(x=0, y=0)) + coord_flip()

eislmmon <- cgenmakrofigure(dfnumeric=dfislm,
                                 edfnumeric=edfislm,
                                 variables = c(dfislm$varnavn)[c(3,4)],
                                 labt = labelsislm,
                                 elabt = elabelsislm,
                                 scalejust = list(x=0, y=50)) + coord_flip()

eislmmon
