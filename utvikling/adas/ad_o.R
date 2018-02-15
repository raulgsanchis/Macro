library(MakroOEKB1115)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(latex2exp)
###########################################################################################################
# AD-Kurven fast-kurs
iv <- 0:4
openpar <- list(i_s=1.5, rp=0.25, E=1, Ps=1, x1=20, x2=0.1, m1=15, m2=0.1, Ys=200, rp=0,Ee=1)
islmoexoparvalv <- c(list(c_1 = 0.6, oC = 50, oG= 50, b = 10, oI = 10, T = 50, M= 100,
                         P=1, h = 10, k =1, Y = 130, m=1, t=0.4), openpar, list(i=c(iv)))

eopenpar <- list(i_s=1.5, rp=0.25, E=1, Ps=1, x1=20, x2=0.1, m1=15, m2=0.1, Ys=200, rp=0,Ee=1)
eislmexoparvalv <- c(list(c_1 = 0.6, oC = 50, oG= 50, b = 10, oI = 10, T = 50, M= 100,
                         P=0.75, h = 10, k =1, Y = 130, m=1, t=0.4), eopenpar, list(i=c(iv)))


dfislmo <- dfgeneric(modell='islmo', exoparval = islmoexoparvalv)
edfislmo <- dfgeneric(modell='islmo', exoparval = eislmexoparvalv)

# Fast kurs
idfkurverislmo <- data.frame(kurve=c("", "IS"),
                           fargel = c('red', 'red'),
                           fargek = c('red', 'red'),
                           y = c(filter(dfislmo$varnavnminverdi, variable=='ibpv')[,3], filter(dfislmo$varnavnminverdi, variable=='iisv')[,3]),
                           x = c(filter(dfislmo$varnavnminverdi, variable=='ibpv')[,1], filter(dfislmo$varnavnminverdi, variable=='iisv')[,1]))


ilabelsislm <- list(title = 'Mundell-Fleming modellen - fast kurs',
                    y = 'produksjon, inntekt (Y)',
                    x = 'rentenivå (i)',
                    x0 = c(TeX('$i_{0}}$')),
                    y0 = c(TeX('$Y_{0}$')),
                    kurver = idfkurverislmo)

eidfkurverislmo <- data.frame(kurve=c("", "IS'"),
                             fargel = c('red', 'red'),
                             fargek = c('red', 'red'),
                             y = c(filter(edfislmo$varnavnminverdi, variable=='ibpv')[,3], filter(edfislmo$varnavnminverdi, variable=='iisv')[,3]),
                             x = c(filter(edfislmo$varnavnminverdi, variable=='ibpv')[,1], filter(edfislmo$varnavnminverdi, variable=='iisv')[,1]))


eilabelsislm <- list(title = 'Mundell-Fleming modellen - flytende kurs',
                    y = 'produksjon, inntekt (Y)',
                    x = 'rentenivå (i)',
                    x0 = c(TeX('$i_{1}}$')),
                    y0 = c(TeX('$Y_{1}$')),
                    kurver = eidfkurverislmo)


islmochangepricei <- cgenmakrofigure(dfnumeric=dfislmo,
                                   edfnumeric=edfislmo,
                                   variables = c('iisv'),
                                   labt = ilabelsislm,
                                   elabt = eilabelsislm,
                                   scalejust = list(x=0, y=0),
                                   limits= list(x=NULL, y=c(150,250))) + coord_flip() +
  geom_line(data=data.frame(x=dfislmo$yeae[1], y=75:230), aes(x,y), color ='black', size=0.5) +
  geom_text(aes(x=dfislmo$yeae[1], y=240 ,label='BoP'), color = 'red')

islmochangepricei

# Flytende kurs
iv <- 1.5:10
openpar <- list(i_s=1.5, rp=0.25, E=1, Ps=1, x1=20, x2=0.1, m1=15, m2=0.1, Ys=200, rp=0,Ee=1)
islmoexoparvalv <- c(list(c_1 = 0.6, oC = 50, oG= 50, b = 10, oI = 10, T = 50, M= 100,
                          P=1, h = 10, k =1, Y = 130, m=1, t=0.4), openpar, list(i=c(iv)))

eopenpar <- list(i_s=1.5, rp=0.25, E=1, Ps=1, x1=20, x2=0.1, m1=15, m2=0.1, Ys=200, rp=0,Ee=1)
eislmexoparvalv <- c(list(c_1 = 0.6, oC = 50, oG= 50, b = 10, oI = 10, T = 50, M= 100,
                          P=0.75, h = 10, k =1, Y = 130, m=1, t=0.4), eopenpar, list(i=c(iv)))

dfislmo <- dfgeneric(modell='islmo', exoparval = islmoexoparvalv)
edfislmo <- dfgeneric(modell='islmo', exoparval = eislmexoparvalv)

edfkurverislmo <- data.frame(kurve=c("LM", "IS-BoP"),
                             fargel = c('red', 'red'),
                             fargek = c('red', 'red'),
                             y = c(filter(dfislmo$varnavnmaksverdi, variable=='elmv')[,3], filter(dfislmo$varnavnminverdi, variable=='eisbpv')[,3]),
                             x = c(filter(dfislmo$varnavnmaksverdi, variable=='elmv')[,1], filter(dfislmo$varnavnminverdi, variable=='eisbpv')[,1]))


elabelsislm <- list(title = 'Mundell-Fleming modellen - fast kurs',
                    y = 'produksjon, inntekt (Y)',
                    x = 'rentenivå (i)',
                    x0 = c(TeX('$i_{0}}$')),
                    y0 = c(TeX('$Y_{0}$')),
                    kurver = edfkurverislmo)

eedfkurverislmo <- data.frame(kurve=c("LM'", "IS-BoP'"),
                              fargel = c('red', 'red'),
                              fargek = c('red', 'red'),
                              y = c(filter(edfislmo$varnavnmaksverdi, variable=='elmv')[,3], filter(edfislmo$varnavnminverdi, variable=='eisbpv')[,3]),
                              x = c(filter(edfislmo$varnavnmaksverdi, variable=='elmv')[,1], filter(edfislmo$varnavnminverdi, variable=='eisbpv')[,1]))


eelabelsislm <- list(title = 'Mundell-Fleming modellen - fast kurs',
                     y = 'produksjon, inntekt (Y)',
                     x = 'rentenivå (i)',
                     x0 = c(TeX('$i_{1}}$')),
                     y0 = c(TeX('$Y_{1}$')),
                     kurver = eedfkurverislmo)


dfislmo$yeae <- c(4.75, 143)
edfislmo$yeae <- c(3, 175)


islmochangepricee <- cgenmakrofigure(dfnumeric=dfislmo,
                                    edfnumeric=edfislmo,
                                    variables = c('eisbpv','elmv'),
                                    labt = elabelsislm,
                                    elabt = eelabelsislm,
                                    scalejust = list(x=0, y=75),
                                    limits= list(x=NULL, y=NULL)) + coord_flip()
islmochangepricee








###################################
# AD-Kurven flytende kurs
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

grid.arrange(islmochangepricei, islmochangepricee, adaslikevekt, adaslikevekt, nrow=2)
