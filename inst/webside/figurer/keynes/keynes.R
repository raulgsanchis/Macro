library(MakroOEKB1115)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(latex2exp)
#######################################################################################################################
Iv <- 0:600
keynesexoparvalv <- c(list(c_1 = 0.6, oC = 25, oG= 50, a = 30, i = 0.03, oI = 50, T = 10), list(Y=c(Iv)))

dfkeynes <- dfgeneric(modell='keynes', exoparval = keynesexoparvalv)

dfkurver = data.frame(kurve=c("45 grader ", "I", "I+G", "I+G+C"),
                      fargel = c('black', 'red', 'red', 'red'),
                      fargek = c('black', 'black', 'black', 'black'),
                      x = c(50, dfkeynes$varnavnmaksverdi$Iv[c(3,5,6)]),
                      y = c(0, dfkeynes$varnavnmaksverdi$value[c(3,5,6)]))

labelskeynes <- list(title = 'Keyneskrysset',
                   x = 'produksjon, inntekt (Y)',
                   y = 'Aggregert etterspørsel (AE)',
                   x0 = c(TeX('$Y_{0}')),
                   y0 = c(TeX('$AE_{0}$')),
                   kurver = dfkurver)

keynesclikevekt <- genmakrofigure(dfnumeric=dfkeynes,
                               variables = c(dfkeynes$varnavn)[c(1,3,5,6)],
                               labt = labelskeynes,
                               scalejust = list(x=0, y=0))

keynesclikevekt
## Keynes komparativ statikk (skiftanalyse)
###########################################################################################################
Iv <- 0:600
ekeynesexoparvalv <- c(list(c_1 = 0.6, oC = 25, oG= 50, a = 30, i = 0.03, oI = 100, T = 10), list(Y=c(Iv)))

edfkeynes <- dfgeneric(modell='keynes', exoparval = ekeynesexoparvalv)

edfkurver <- data.frame(kurve=c("I+G+C'"),
                        fargel = c( 'red'),
                        fargek = c('black'),
                        x = c(edfkeynes$varnavnmaksverdi$Iv[c(6)]),
                        y = c(edfkeynes$varnavnmaksverdi$value[c(6)]))

dfkurver = data.frame(kurve=c("45 grader","I+G+C"),
                      fargel = c('red','red'),
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
