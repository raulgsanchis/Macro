library(MakroOEKB1115)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(latex2exp)
#######################################################################################################################
edfkeynes <- dfgeneric(modell='keynes', exoparval = ekeynesexoparvalv)

edfkurver <- data.frame(kurve=c("I'+G+C'"),
                        fargel = c('red'),
                        fargek = c('black'),
                        x = c(edfkeynes$varnavnmaksverdi$Iv[c(6)]),
                        y = c(edfkeynes$varnavnmaksverdi$value[c(6)]))

dfkurver = data.frame(kurve=c("45 grader","I+G+C"),
                      fargel = c('black','red'),
                      fargek = c('black','black'),
                      x = c(50, dfkeynes$varnavnmaksverdi$Iv[c(6)]),
                      y = c(0, dfkeynes$varnavnmaksverdi$value[c(6)]))

labelskeynes <- list(title = 'Keyneskrysset',
                     x = 'produksjon, inntekt (Y)',
                     y = 'Aggregert etterspørsel (AE)',
                     x0 = c(TeX('$Y_{0}')),
                     y0 = c(TeX('$AE_{0}$')),
                     kurver = dfkurver)


elabelskeynes <- list(title = 'Keyneskrysset',
                     x = 'produksjon, inntekt (Y)',
                     y = 'Aggregert etterspørsel (AE)',
                     x0 = c(TeX('$Y_{1}')),
                     y0 = c(TeX('$AE_{1}$')),
                     kurver = edfkurver)

ekeynesclikevekt <- cgenmakrofigure(dfnumeric=dfkeynes,
                edfnumeric=edfkeynes,
                variables = c(dfkeynes$varnavn)[c(1,6)],
                labt = labelskeynes,
                elabt = elabelskeynes,
                scalejust = list(x=0, y=0))

ekeynesclikevekt
###########################################################################################################
###########################################################################################################
iv <- 0:5
islmexoparvalv <- c(list(c_1 = 0.6, oC = 50, oG= 50, b = 10, oI = 10, T = 50, M= 100,
                         P=1, h = 10, k =1, Y = 130, m=2), list(i=c(iv)))

dfislm <- dfgeneric(modell='islm', exoparval = islmexoparvalv, eqsel = c(1,2))

dplyr::filter(dfislm$dfmodell, variable %in% c(dfislm$varnavn)[c(3,5)])

# Pengemarkedet
dfkurver <- data.frame(kurve=c("Ld", "i-renteregel"),
                       fargel = c('red', 'red'),
                       fargek = c('red', 'red'),
                       y = c(dfislm$varnavnminverdi$value[c(1)],225),
                       x = c(dfislm$varnavnminverdi$Iv[c(2)], 3))

labelslm <- list(title = 'Pengemarkedet',
                 y = 'Realpengemengde (M/P)',
                 x = 'rentenivå (i)',
                 x0 = c(TeX('$i_{0}}$')),
                 y0 = c(TeX('$')),
                 kurver = dfkurver)

dfislm$yeae <-  c(2.857143, 100)

lmlikevekt <- genmakrofigure(dfnumeric = dfislm,
                             variables = c(dfislm$varnavn)[c(1)],
                             labt = labelslm,
                             scalejust = list(x=0, y=60))  + coord_flip() + geom_line(data=data.frame(y=10:250, x=2.85), aes(x,y))


# IS-LM Modellen
#!: endogenisere gjetteverdier
dfkurver <- data.frame(kurve=c("IS", "i-renteregel"),
                       fargel = c('red', 'red'),
                       fargek = c('red', 'red'),
                       y = c(dfislm$varnavnminverdi$value[c(3)], 235),
                       x = c(dfislm$varnavnminverdi$Iv[c(3)], 2.75))

labelsislm <- list(title = 'IS-LM modellen',
                   y = 'produksjon, inntekt (Y)',
                   x = 'rentenivå (i)',
                   x0 = c(TeX('$i_{0}}$')),
                   y0 = c(TeX('$Y_{0}$')),
                   kurver = dfkurver)


dfislm$yeae <- c(1.6,160)

islmlikevekt <- genmakrofigure(dfnumeric=dfislm,
                               variables = c(dfislm$varnavn)[c(3)],
                               labt = labelsislm,
                               scalejust = list(x=0, y=75))  + coord_flip() + geom_line(data=data.frame(y=75:250, x=0.0100*(75:250)), aes(x,y))

islmlikevekt


grid.arrange(lmlikevekt,islmlikevekt, ncol=2)
