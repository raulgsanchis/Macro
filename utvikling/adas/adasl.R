library(MakroOEKB1115)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(latex2exp)
###########################################################################################################
# AD-Kurven
iv <- 0:4
islmexoparvalv <- c(list(c_1 = 0.6, oC = 50, oG= 50, b = 10, oI = 10, T = 50, M= 100,
                         P=1, h = 10, k =1, Y = 130), list(i=c(iv)))

eislmexoparvalv <- c(list(c_1 = 0.6, oC = 50, oG= 50, b = 10, oI = 10, T = 50, M= 100,
                         P=0.75, h = 10, k =1, Y = 130), list(i=c(iv)))


dfislm <- dfgeneric(modell='islm', exoparval = islmexoparvalv)
edfislm <- dfgeneric(modell='islm', exoparval = eislmexoparvalv)

dfkurverislm <- data.frame(kurve=c("IS", "LM"),
                       fargel = c('red', 'red'),
                       fargek = c('red', 'red'),
                       y = c(dfislm$varnavnminverdi$value[c(3)], dfislm$varnavnmaksverdi$value[c(4)]),
                       x = c(dfislm$varnavnminverdi$Iv[c(3)],dfislm$varnavnmaksverdi$Iv[c(4)]))

epdfkurverislm <- data.frame(kurve=c("LM'"),
                           fargel = c('red'),
                           fargek = c('red'),
                           y = c(edfislm$varnavnmaksverdi$value[c(4)]),
                           x = c(edfislm$varnavnmaksverdi$Iv[c(4)]))

labelsislm <- list(title = 'IS-LM modellen',
                    y = 'produksjon, inntekt (Y)',
                    x = 'rentenivå (i)',
                    x0 = c(TeX('$i_{0}}$')),
                    y0 = c(TeX('$Y_{0}$')),
                    kurver = dfkurverislm)


elabelsislm <- list(title = 'IS-LM modellen',
                    y = 'produksjon, inntekt (Y)',
                    x = 'rentenivå (i)',
                    x0 = c(TeX('$i_{1}}$')),
                    y0 = c(TeX('$Y_{1}$')),
                    kurver = epdfkurverislm)


islmchangemoney <- cgenmakrofigure(dfnumeric=dfislm,
                                   edfnumeric=edfislm,
                                   variables = c(dfislm$varnavn)[c(3,4)],
                                   labt = labelsislm,
                                   elabt = elabelsislm,
                                   scalejust = list(x=0, y=100),
                                   limits= list(x=NULL, y=c(NULL,NULL))) + coord_flip()

###################################
Yv <- 100:200 # Guess
adasexoparvalv <- c(list(c_1 = 0.6, oC = 50, oG= 50, b = 10, oI = 10, T = 50, M= 100, P=1, h = 10,
                         k =1),list(Pe=1, mu = 0.1, l_1=-20,l_2=1, z=1, A= 5,
                                    N=400, alpha = 1, Ac = 2), list(Y=c(Yv)))

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


dfadas$yeae <- c(dfislm$yeae[2], edfislm$yeae[2], 1, 0.75)

adaslikevekt <- genmakrofigure(dfnumeric = dfadas,
                               variables = c(dfadas$varnavn[1]),
                               labt = labelsadas,
                               scalejust = list(x=100, y=0),
                               punktvelger = list(x=c(1,2), y=c(3,4)))

grid.arrange(islmchangemoney, adaslikevekt, nrow=2)
