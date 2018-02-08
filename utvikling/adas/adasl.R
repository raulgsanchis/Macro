library(MakroOEKB1115)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(latex2exp)
###########################################################################################################
# IS-LM likevekt
iv <- 0:7.5
islmexoparvalv <- c(list(c_1 = 0.6, oC = 25, oG= 75, b = 15, oI = 75, T = 10, M= 200, P=1, h = 80, k =2, Y = 300), list(i=c(iv)))
eislmexoparvalv <- c(list(c_1 = 0.6, oC = 25, oG= 75, b = 15, oI = 75, T = 10, M= 275, P=0.75, h = 80, k =2, Y = 300), list(i=c(iv)))


dfislm <- dfgeneric(modell='islm', exoparval = islmexoparvalv)
edfislm <- dfgeneric(modell='islm', exoparval = eislmexoparvalv)


### Likevekt ###
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
                                   scalejust = list(x=0, y=50),
                                   limits= list(x=NULL, y=c(50,450))) + coord_flip()

Yv <- 50:410 # Guess
adasexoparvalv <- c(list(c_1 = 0.6, oC = 25, oG= 75, b = 100, oI = 75,
                         T = 75, M= 2000, P=1, h = 2, k =2, Pe=1, mu = 0.1,
                         l_1=-20,l_2=1, z=1, A=0.5, N=400, alpha = 1, Ac = 2), list(Y=c(Yv)))

dfadas <- dfgeneric(modell='adasl', exoparval = adasexoparvalv)

dfkurver = data.frame(kurve=c("AD"),
                      fargel = c('red'),
                      fargek = c('red'),
                      x = dfadas$varnavnmaksverdi$Iv[1],
                      y = dfadas$varnavnmaksverdi$value[1])

labelsadas <- list(title = 'AD-kurven',
                   x = 'produksjon, inntekt (Y)',
                   y = 'prisnivå (P)',
                   x0 = c(TeX('$Y_{0$'), TeX('$Y_{1}$')),
                   y0 = c(TeX('$P_{0}$'), TeX('$P_{1}$')),
                   kurver = dfkurver)

####B
#P=1
#P=0.75

dfadas$yeae <- c(dfislm$yeae[2], edfislm$yeae[2], dfislm$yeae[1]-0.3, edfislm$yeae[1]+0.1)

adaslikevekt <- genmakrofigure(dfnumeric = dfadas,
                               variables = c(dfadas$varnavn[1]),
                               labt = labelsadas,
                               scalejust = list(x=50, y=0),
                               punktvelger = list(x=c(1,2), y=c(3,4)),
                               limits= list(x=c(50,450), y=NULL))

grid.arrange(islmchangemoney, adaslikevekt, nrow=2)


# AD-AS likevekt
###########################################################################################################
Yv <- 100:450 # Guess
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
