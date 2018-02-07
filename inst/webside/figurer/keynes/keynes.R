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

dfkurver = data.frame(kurve=c("", "", "", "", "", "A"),
                      fargel = c('red', 'red'),
                      fargek = c('red', 'red'),
                      x = dfkeynes$varnavnmaksverdi$Iv,
                      y = dfadas$varnavnmaksverdi$value)

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

## Keynes komparativ statikk (skiftanalyse)
###########################################################################################################
Iv <- 0:600
ekeynesexoparvalv <- c(list(c_1 = 0.6, oC = 25, oG= 50, a = 30, i = 0.03, oI = 75, T = 10), list(Y=c(Iv)))

edfkeynes <- dfgeneric(modell='keynes', exoparval = keynesexoparvalv)

edfkurver <- data.frame(kurve=c("Y", "AE"),
                      fargel = c('red', 'red'),
                      fargek = c('red', 'red'),
                      x = edfkeynes$varnavnmaksverdi$Iv,
                      y = edfkeynes$varnavnmaksverdi$value)

elabelskeynes <- list(title = 'Keyneskrysset',
                     x = 'produksjon, inntekt (Y)',
                     y = 'Aggregert etterspørsel (AE)',
                     x0 = c(TeX('$Y_{0}')),
                     y0 = c(TeX('$AE_{0}$')),
                     kurver = edfkurver)

ekeynesclikevekt <- cgenmakrofigure(dfnumeric=dfkeynes,
                edfnumeric=edfkeynes,
                variables = c(dfkeynes$varnavn),
                labt = elabelskeynes,
                elabt = elabelskeynes,
                scalejust = list(x=0, y=0))
###########################################################################################################
###########################################################################################################
