library(MakroOEKB1115)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(latex2exp)
#######################################################################################################################
iv <- 0:7.5
islmexoparvalv <- c(list(c_1 = 0.6, oC = 25, oG= 75, b = 15, oI = 75, T = 10, M= 200, P=1, h = 80, k =2, Y = 300), list(i=c(iv)))
dfislm <- dfgeneric(modell='islm', exoparval = islmexoparvalv)


# Pengemarkedet
dfkurver <- data.frame(kurve=c("IS", "LM"),
                       fargel = c('red', 'red'),
                       fargek = c('red', 'red'),
                       y = dfislm$varnavnminverdi$value[c(1,2)],
                       x = c(dfislm$varnavnminverdi$Iv[c(1)],dfislm$varnavnmaksverdi$Iv[c(2)]))


labelsislm <- list(title = 'Pengemarkedet',
                   y = 'produksjon, inntekt (Y)',
                   x = 'rentenivå (i)',
                   x0 = c(TeX('$i_{0}}$')),
                   y0 = c(TeX('$Y_{0}$')),
                   kurver = dfkurver)

dfislm$yeae <- c(5, 200)

figpengem <- genmakrofigure(dfnumeric=dfislm,
                            variables = c(dfislm$varnavn)[c(1,2)],
                            labt = labelsislm,
                            scalejust = list(x=0, y=0))  + coord_flip()
figpengem
#######################################################################################################################
# IS-LM Modellen
### Likevekt ###
iv <- 0:7.5
islmexoparvalv <- c(list(c_1 = 0.6, oC = 25, oG= 75, b = 15, oI = 75, T = 10, M= 200, P=1, h = 80, k =2, Y = 300), list(i=c(iv)))

#!: endogenisere gjetteverdier
dfislm <- dfgeneric(modell='islm', exoparval = islmexoparvalv)

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
iv <- 0:7.5
eexoparvalv <- c(list(c_1 = 0.6, oC = 25, oG= 75, b = 15, oI = 75, T = 10, M= 275, P=1, h = 80, k =2, Y = 300), list(i=c(iv)))

edfislm <- dfgeneric(modell='islm', exoparval = eexoparvalv)

edfkurver = data.frame(kurve=c("AD'", "AS"),
                       fargel = c('red', 'red'),
                       fargek = c('red', 'red'),
                       x = edfislm$varnavnmaksverdi$Iv,
                       y = edfislm$varnavnmaksverdi$value)

elabelsislm <- list(title = 'IS-LM modellen',
                    x = 'produksjon, inntekt (Y)',
                    y = 'rentenivå (i)',
                    x0 = c(TeX('$Y_{1}$')),
                    y0 = c(TeX('$P_{1}$')),
                    kurver = edfkurver)

eksadaslikevekt <- cgenmakrofigure(dfnumeric=dfislm,
                                   edfnumeric=edfislm,
                                   variables = c(dfislm$varnavn)[c(3,4)],
                                   labt = labelsislm,
                                   elabt = elabelsislm,
                                   scalejust = list(x=200, y=0))
