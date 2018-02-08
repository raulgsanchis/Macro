library(MakroOEKB1115)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(latex2exp)

# AD-AS likevekt
###########################################################################################################
Yv <- 200:250 # Guess
adasexoparvalv <- c(list(c_1 = 0.6, oC = 25, oG= 75, b = 100, oI = 75,
                           T = 75, M= 2000, P=1, h = 2, k =2, Pe=1, mu = 0.1,
                           l_1=-20,l_2=1, z=1, A=0.5, N=400, alpha = 1, Ac = 2), list(Y=c(Yv)))

dfadas <- dfgeneric(modell='adasl', exoparval = adasexoparvalv)

dfkurver = data.frame(kurve=c("AD", "AS"),
                    fargel = c('red', 'red'),
                    fargek = c('red', 'red'),
                    x = dfadas$varnavnmaksverdi$Iv,
                    y = dfadas$varnavnmaksverdi$value)

labelsadas <- list(title = 'AD-AS modellen',
                   x = 'produksjon, inntekt (Y)',
                   y = 'prisnivå (P)',
                   x0 = c(TeX('$Y_{0}=Y^{N}$')),
                   y0 = c(TeX('$P_{0}$')),
                   kurver = dfkurver)

adaslikevekt <- genmakrofigure(dfnumeric=dfadas,
                      variables = c(dfadas$varnavn),
                      labt = labelsadas,
                      scalejust = list(x=200, y=0))

adaslaslikevekt <- adaslikevekt + geom_line(data=data.frame(x=dfadas$yeae[1], y=0:10), aes(x,y), color ='black', size=0.5) + geom_text(aes(x=221, y= 10,label='LAS'), color = 'red')

## AD-AS komparativ statikk (skiftanalyse)
###########################################################################################################
### Kort sikt
eksYv <- 200:250 # Guess
eksadasexoparvalv <- c(list(c_1 = 0.6, oC = 25, oG= 75, b = 100, oI = 75,
                            T = 75, M= 2300, P=1, h = 2, k =2, Pe=1, mu = 0.1,
                            l_1=-20,l_2=1, z=1, A=0.5, N=400, alpha = 1, Ac = 2), list(Y=c(Yv)))

eksdfadas <- dfgeneric(modell='adasl', exoparval = eksadasexoparvalv)

edfkurver = data.frame(kurve=c("AD'", "AS"),
                       fargel = c('red', 'red'),
                       fargek = c('red', 'red'),
                       x = eksdfadas$varnavnmaksverdi$Iv,
                       y = eksdfadas$varnavnmaksverdi$value)

elabelsadas <- list(title = 'AD-AS modellen',
                   x = 'produksjon, inntekt (Y)',
                   y = 'prisnivå (P)',
                   x0 = c(TeX('$Y_{1}$')),
                   y0 = c(TeX('$P_{1}$')),
                   kurver = edfkurver)


eksadaslikevekt <- cgenmakrofigure(dfnumeric=dfadas,
                                   edfnumeric=eksdfadas,
                                   variables = c(dfadas$varnavn),
                                   labt = labelsadas,
                                   elabt = elabelsadas,
                                   scalejust = list(x=200, y=0))

eksadaslikevekt
### Mellomlang sikt ###
emsYv <- 200:250 # Guess
emsadasexoparvalv <- c(list(c_1 = 0.6, oC = 25, oG= 75, b = 100, oI = 75,
                            T = 75, M= 2300, P=1, h = 2, k =2, Pe=1.15, mu = 0.1,
                            l_1=-20,l_2=1, z=1, A=0.5, N=400, alpha = 1, Ac = 2), list(Y=c(Yv)))

emsdfadas <- dfgeneric(modell='adasl', exoparval = emsadasexoparvalv)

emdfkurver = data.frame(kurve=c("AD'", "AS''"),
                       fargel = c('red', 'red'),
                       fargek = c('red', 'red'),
                       x = emsdfadas$varnavnmaksverdi$Iv,
                       y = emsdfadas$varnavnmaksverdi$value)

elabelsadas <- list(title = 'AD-AS modellen',
                    x = 'produksjon, inntekt (Y)',
                    y = 'prisnivå (P)',
                    x0 = c(TeX('$$')),
                    y0 = c(TeX('$P_{2}$')),
                    kurver = emdfkurver)

labelsadas <- list(title = 'AD-AS modellen',
                   x = 'produksjon, inntekt (Y)',
                   y = 'prisnivå (P)',
                   x0 = c(TeX('$Y_{0}=$Y_{2}=Y^{N}$')),
                   y0 = c(TeX('$P_{0}$')),
                   kurver = dfkurver)

adaslikevekt <- genmakrofigure(dfnumeric=dfadas,
                               variables = c(dfadas$varnavn),
                               labt = labelsadas,
                               scalejust = list(x=200, y=0))


emsadaslikevekt <- cgenmakrofigure(dfnumeric=dfadas,
                                   edfnumeric=emsdfadas,
                                   variables = c(dfadas$varnavn),
                                   labt = labelsadas,
                                   elabt = elabelsadas,
                                   scalejust = list(x=200, y=0)) + geom_line(data=data.frame(x=dfadas$yeae[1], y=0:10), aes(x,y), color ='black', size=0.5) + geom_text(aes(x=221, y= 10,label='LAS'), color = 'red')

emsadaslikevekt
## AD-AS stabiliseringspolitikk
###########################################################################################################
stYv <- 200:250 # Guess
stadasexoparvalv <- c(list(c_1 = 0.6, oC = 25, oG= 75, b = 100, oI = 75,
                            T = 75, M= 2300, P=1, h = 2, k =2, Pe=1, mu = 0.1,
                            l_1=-20,l_2=1, z=1, A=0.5, N=400, alpha = 1, Ac = 2), list(Y=c(Yv)))

stdfadas <- dfgeneric(modell='adasl', exoparval = stadasexoparvalv)

sdfkurver = data.frame(kurve=c("AD'", "AS"),
                       fargel = c('red', 'red'),
                       fargek = c('red', 'red'),
                       x = stdfadas$varnavnmaksverdi$Iv,
                       y = stdfadas$varnavnmaksverdi$value)

ldfkurver = data.frame(kurve=c("AD=AD''", "AS"),
                      fargel = c('red', 'red'),
                      fargek = c('red', 'red'),
                      x = dfadas$varnavnmaksverdi$Iv,
                      y = dfadas$varnavnmaksverdi$value)

llabelsadas <- list(title = 'AD-AS modellen',
                   x = 'produksjon, inntekt (Y)',
                   y = 'prisnivå (P)',
                   x0 = c(TeX('$Y_{0}=Y_{2}=Y^{N}$')),
                   y0 = c(TeX('$P_{0}=$P_{2}$')),
                   kurver = ldfkurver)


slabelsadas <- list(title = 'AD-AS modellen',
                    x = 'produksjon, inntekt (Y)',
                    y = 'prisnivå (P)',
                    x0 = c(TeX('$Y_{1}$')),
                    y0 = c(TeX('$P_{1}$')),
                    kurver = sdfkurver)

stadaslikevekt <- cgenmakrofigure(dfnumeric=dfadas,
                                   edfnumeric=stdfadas,
                                   variables = c(dfadas$varnavn),
                                   labt = llabelsadas,
                                   elabt = slabelsadas,
                                   scalejust = list(x=200, y=0))
stadaslikevekt
