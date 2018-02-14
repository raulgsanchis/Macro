library(MakroOEKB1115)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(latex2exp)
# AD-AS likevekt
###########################################################################################################
Yv <- 105:125 # Guess
adasexoparvalv <- c(list(c_1 = 0.6, oC = 50, oG= 50, b = 10, oI = 10, T = 50, M= 100, P=1, h = 10,
                         k =1),list(Pe=1, mu = 2, l_1=-5,l_2=1, z=1, A= 1,
                                    N=400, alpha = 1, Ac = 4), list(Y=c(Yv)))

dfadas <- dfgeneric(modell='adasl', exoparval = adasexoparvalv)
subset(dfadas$dfmodell, variable = 'asv')

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
                      scalejust = list(x=100, y=0.5))

adaslikevekt


## AD-AS komparativ statikk (skiftanalyse)
# ###########################################################################################################
# ### Kort sikt
eYv <- 105:125 # Guess
eiadasexoparvalv <- c(list(c_1 = 0.6, oC = 50, oG= 50, b = 10, oI = 30, T = 50, M= 100, P=1, h = 10,
                         k =1),list(Pe=1, mu = 2, l_1=-5,l_2=1, z=1, A= 1,
                                    N=400, alpha = 1, Ac = 4), list(Y=c(eYv)))
eksdfadas <- dfgeneric(modell='adasl', exoparval = eiadasexoparvalv)

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
                                   scalejust = list(x=110, y=0))

#### Mellomlang sikt ###
Yv <- 105:125 # Guess
eiadasexoparvalv <- c(list(c_1 = 0.6, oC = 50, oG= 50, b = 10, oI = 30, T = 50, M= 100, P=1, h = 10,
                           k =1),list(Pe=1.3425, mu = 2, l_1=-5,l_2=1, z=1, A= 1,
                                      N=400, alpha = 1, Ac = 4), list(Y=c(eYv)))

emsdfadas <- dfgeneric(modell='adasl', exoparval = eiadasexoparvalv)

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

emsadaslikevekt <- cgenmakrofigure(dfnumeric=dfadas,
                                   edfnumeric=emsdfadas,
                                   variables = c(dfadas$varnavn),
                                   labt = labelsadas,
                                   elabt = elabelsadas,
                                   scalejust = list(x=100, y=0)) + geom_line(data=data.frame(x=dfadas$yeae[1], y=0:3), aes(x,y), color ='black', size=0.5) + geom_text(aes(x=110, y= 3,label='LAS'), color = 'red')

emsadaslikevekt

# ## AD-AS stabiliseringspolitikk
# ###########################################################################################################
Yv <- 105:125 # Guess
stadasexoparvalv <- c(list(c_1 = 0.6, oC = 50, oG= 50, b = 10, oI = 30, T = 50, M= 100, P=1, h = 10,
                           k =1),list(Pe=1, mu = 2, l_1=-5,l_2=1, z=1, A= 1,
                                      N=400, alpha = 1, Ac = 4), list(Y=c(eYv)))

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
                                  scalejust = list(x=105, y=0))
stadaslikevekt
