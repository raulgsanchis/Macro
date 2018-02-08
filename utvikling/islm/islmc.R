library(MakroOEKB1115)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(latex2exp)
#######################################################################################################################
#keynesexoparvalv <- c(list(c_1 = 0.6, oC = 50, oG= 50, b = 10, i = 2, oI = 10, T = 50), list(Y=c(Iv)))
iv <- 0:7.5
islmexoparvalv <- c(list(c_1 = 0.6, oC = 50, oG= 50, b = 10, oI = 10, T = 50, M= 210,
                         P=1, h = 10, k =1, Y = 250), list(i=c(iv)))

eislmexoparvalv <- c(list(c_1 = 0.6, oC = 50, oG= 50, b = 10, oI = 10, T = 50, M= 220,
                          P=1, h = 10, k =1, Y = 250), list(i=c(iv)))

dfislm <- dfgeneric(modell='islm', exoparval = islmexoparvalv, eqsel = c(2,3))
edfislm <- dfgeneric(modell='islm', exoparval = eislmexoparvalv, eqsel = c(2,3))

#!A
dfislm$yeae <- c(4,210)

# Pengemarkedet
dfkurver <- data.frame(kurve=c("Ld", "Ms"),
                       fargel = c('red', 'red'),
                       fargek = c('red', 'red'),
                       y = dfislm$varnavnminverdi$value[c(1,2)]*(1+0.00),
                       x = c(dfislm$varnavnminverdi$Iv[c(2)], dfislm$varnavnmaksverdi$Iv[c(1)]))


labelslm <- list(title = 'Pengemarkedet',
                   y = 'produksjon, inntekt (Y)',
                   x = 'rentenivå (i)',
                   x0 = c(TeX('$i_{0}}$')),
                   y0 = c(TeX('$M_{0}/P_{0}$')),
                   kurver = dfkurver)

lmlikevekt <- genmakrofigure(dfnumeric = dfislm,
                            variables = c(dfislm$varnavn)[c(1,2)],
                            labt = labelslm,
                            scalejust = list(x=0, y=175))  + coord_flip()

emlmlikevekt <- data.frame(kurve=c("Ms'"),
                         fargel = c('red'),
                         fargek = c('red'),
                         x = edfislm$varnavnmaksverdi$Iv[c(2)],
                         y = edfislm$varnavnmaksverdi$value[c(2)])

elabelslm <- list(title = 'Pengemarkedet',
                    y = 'produksjon, inntekt (Y)',
                    x = 'rentenivå (i)',
                    x0 = c(TeX('$i_{1}}$')),
                    y0 = c(TeX('$M/P_{1}$')),
                    kurver = emlmlikevekt)

edfislm$yeae <- c(3,220)

lmchangemoney <- cgenmakrofigure(dfnumeric=dfislm,
                                 edfnumeric=edfislm,
                                 variables = c(dfislm$varnavn)[c(1,2)],
                                 labt = labelslm,
                                 elabt = elabelslm,
                                 scalejust = list(x=0, y=175)) + coord_flip()

# IS-LM Modellen
### Likevekt ###
dfislm <- dfgeneric(modell='islm', exoparval = islmexoparvalv, eqsel = c(1,2))
edfislm <- dfgeneric(modell='islm', exoparval = eislmexoparvalv, eqsel = c(1,2))

#!: endogenisere gjetteverdier
dfkurver <- data.frame(kurve=c("IS", "LM"),
                       fargel = c('red', 'red'),
                       fargek = c('red', 'red'),
                       y = c(dfislm$varnavnminverdi$value[c(3)], dfislm$varnavnmaksverdi$value[c(4)]),
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


### Komparativ statikk ###
dfkurverislm <- data.frame(kurve=c("LM'"),
                       fargel = c('red'),
                       fargek = c('red'),
                       y = c(edfislm$varnavnmaksverdi$value[c(4)]),
                       x = c(edfislm$varnavnmaksverdi$Iv[c(4)]))

elabelsislm <- list(title = 'IS-LM modellen',
                   y = 'produksjon, inntekt (Y)',
                   x = 'rentenivå (i)',
                   x0 = c(TeX('$i_{1}}$')),
                   y0 = c(TeX('$Y_{1}$')),
                   kurver = dfkurverislm)


islmchangemoney <- cgenmakrofigure(dfnumeric=dfislm,
                                 edfnumeric=edfislm,
                                 variables = c(dfislm$varnavn)[c(3,4)],
                                 labt = labelsislm,
                                 elabt = elabelsislm,
                                 scalejust = list(x=0, y=50)) + coord_flip()

