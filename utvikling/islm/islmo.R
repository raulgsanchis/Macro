library(MakroOEKB1115)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(latex2exp)
#######################################################################################################################
iv <- 0:5
openpar <- list(i_s=1.5, rp=0.25, E=1, Ps=1, x1=20, x2=0.1, m1=15, m2=0.1, Ys=200, rp=0,Ee=1)
muflexoparvalv <- c(list(c_1 = 0.6, oC = 50, oG= 50, b = 10, oI = 10, T = 50, M= 100,
                         P=1, h = 10, k =1, Y = 130, m=1, t=0.4), openpar, list(i=c(iv)))

eopenpar <- list(i_s=1, rp=0, E=1, Ps=1, x1=20, x2=0.1, m1=15, m2=0.1, Ys=200, rp=0, Ee=1)
emuflexoparvalv <- c(list(c_1 = 0.6, oC = 50, oG= 50, b = 10, oI = 10, T = 50, M= 100,
                          P=1, h = 10, k =1, Y = 130, m=1, t=0.4), eopenpar, list(i=c(iv)))

seopenpar <- list(i_s=1.0, rp=0.25, E=1, Ps=1, x1=20, x2=0.1, m1=15, m2=0.1, Ys=200, rp=0, Ee=1)
semuflexoparvalv <- c(list(c_1 = 0.6, oC = 50, oG= 30, b = 10, oI = 10, T = 50, M= 110,
                          P=1, h = 10, k =1, Y = 130, m=1, t=0.4), seopenpar, list(i=c(iv)))


dfmufl <- dfgeneric(modell='islmo', exoparval = muflexoparvalv, eqsel = c(1,2))
edfmufl <- dfgeneric(modell='islmo', exoparval = emuflexoparvalv, eqsel = c(1,2))
sedfmufl <- dfgeneric(modell='islmo', exoparval = semuflexoparvalv, eqsel = c(1,2))

# Fast kurs
dfkurvermffast <- data.frame(kurve=c("IS", ""),
                             fargel = c('red', 'red'),
                             fargek = c('red', 'red'),
                             y = c(dfmufl$varnavnminverdi$value[c(1)], dfmufl$varnavnmaksverdi$value[c(2)]),
                             x = c(dfmufl$varnavnminverdi$Iv[c(1)],dfmufl$varnavnmaksverdi$Iv[c(2)]))

labelsmufl <- list(title = 'Mundell-Fleming modellen - fast kurs',
                   y = 'produksjon, inntekt (Y)',
                   x = 'rentenivå (i=i*+rp)',
                   x0 = c(TeX('$i_{0}}$')),
                   y0 = c(TeX('$Y_{0}$')),
                   kurver = dfkurvermffast)

muflfastlikevekt <- genmakrofigure(dfnumeric=dfmufl,
                                   variables = c(dfmufl$varnavn)[c(1)],
                                   labt = labelsmufl,
                                   scalejust = list(x=0, y=150))  + coord_flip() +
  geom_line(data=data.frame(x=dfmufl$yeae[1], y=150:230), aes(x,y), color ='black', size=0.5) +
  geom_text(aes(x=dfmufl$yeae[1], y=240 ,label='BoP'), color = 'red')

# Fast kurs, endring i utenlandsrenta
edfkurvermffast <- data.frame(kurve=c("IS", ""),
                              fargel = c('red', 'red'),
                              fargek = c('red', 'red'),
                              y = c(dfmufl$varnavnminverdi$value[c(1)], dfmufl$varnavnmaksverdi$value[c(2)]),
                              x = c(dfmufl$varnavnminverdi$Iv[c(1)],dfmufl$varnavnmaksverdi$Iv[c(2)]))

elabelsmufl <- list(title = 'Mundell-Fleming modellen - fast kurs',
                    y = 'produksjon, inntekt (Y)',
                    x = 'rentenivå (i=i*+rp)',
                    x0 = c(TeX('$i_{1}}$')),
                    y0 = c(TeX('$Y_{1}$')),
                    kurver = edfkurvermffast)

emuflfastlikevekt <- cgenmakrofigure(dfnumeric=dfmufl,
                                     edfnumeric=edfmufl,
                                     variables = c(dfmufl$varnavn)[c(1)],
                                     labt = labelsmufl,
                                     elabt = elabelsmufl,
                                     scalejust = list(x=0, y=75)) + coord_flip() +
  geom_line(data=data.frame(x=dfmufl$yeae[1], y=75:220), aes(x,y), color ='black', size=0.5) +
  geom_text(aes(x=dfmufl$yeae[1], y=220+3 ,label='BoP'), color = 'red') +
  geom_line(data=data.frame(x=edfmufl$yeae[1], y=75:240), aes(x,y), color ='black', size=0.5) +
  geom_text(aes(x=edfmufl$yeae[1], y=240+3 ,label="BoP'"), color = 'red')

emuflfastlikevekt

# Fast kurs, endring i stabiliseringspolitikk
sedfkurvermffast <- data.frame(kurve=c("IS'", ""),
                              fargel = c('red', 'red'),
                              fargek = c('red', 'red'),
                              y = c(sedfmufl$varnavnminverdi$value[c(1)], sedfmufl$varnavnmaksverdi$value[c(2)]),
                              x = c(sedfmufl$varnavnminverdi$Iv[c(1)], sedfmufl$varnavnmaksverdi$Iv[c(2)]))

selabelsmufl <- list(title = 'Mundell-Fleming modellen - fast kurs',
                    y = 'produksjon, inntekt (Y)',
                    x = 'rentenivå (i=i*+rp)',
                    x0 = c(TeX('$i_{2}}$')),
                    y0 = c(TeX('$Y_{2}$')),
                    kurver = sedfkurvermffast)


semuflfastlikevekt <- cgenmakrofigure(dfnumeric=dfmufl,
                                     edfnumeric=sedfmufl,
                                     variables = c(dfmufl$varnavn)[c(1)],
                                     labt = labelsmufl,
                                     elabt = selabelsmufl,
                                     scalejust = list(x=0, y=75)) + coord_flip() +
  geom_line(data=data.frame(x=dfmufl$yeae[1], y=75:220), aes(x,y), color ='black', size=0.5) +
  geom_text(aes(x=dfmufl$yeae[1], y=220+3 ,label='BoP'), color = 'red') +
  geom_line(data=data.frame(x=edfmufl$yeae[1], y=75:240), aes(x,y), color ='black', size=0.5) +
  geom_text(aes(x=edfmufl$yeae[1], y=240+3 ,label="BoP'"), color = 'red')

semuflfastlikevekt
#################################################################################
# Flytende kurs
iv <- 2:7.5
eopenpar <- list(i_s=1.5, rp=0.25, E=1, Ps=1, x1=20, x2=0.1, m1=15, m2=0.1, Ys=200, rp=0, Ee=1)
emuflexoparvalv <- c(list(c_1 = 0.6, oC = 50, oG= 50, b = 10, oI = 10, T = 50, M= 100,
                         P=1, h = 10, k =1, Y = 130, m=1, t=0.4), eopenpar, list(i=c(iv)))

iv <- 2:7.5
eeopenpar <- list(i_s=1.0, rp=0, E=1, Ps=1, x1=20, x2=0.1, m1=15, m2=0.1, Ys=200, rp=0, Ee=1)
eemuflexoparvalv <- c(list(c_1 = 0.6, oC = 50, oG= 75, b = 10, oI = 10, T = 50, M= 100,
                          P=1, h = 10, k =1, Y = 130, m=1, t=0.4), eeopenpar, list(i=c(iv)))

iv <- 2:7.5
eseopenpar <- list(i_s=1.0, rp=0, E=1, Ps=1, x1=20, x2=0.1, m1=15, m2=0.1, Ys=200, rp=0, Ee=1)
esemuflexoparvalv <- c(list(c_1 = 0.6, oC = 50, oG= 75, b = 10, oI = 10, T = 50, M= 80,
                            P=1, h = 10, k =1, Y = 130, m=1, t=0.4), eeopenpar, list(i=c(iv)))

edfmufl <- dfgeneric(modell='islmo', exoparval = emuflexoparvalv, eqsel = c(3,4))
eedfmufl <- dfgeneric(modell='islmo', exoparval = eemuflexoparvalv, eqsel = c(3,4))
sedfmufl <- dfgeneric(modell='islmo', exoparval = esemuflexoparvalv, eqsel = c(3,4))


edfkurvermffast <- data.frame(kurve=c("IS-BoP", "LM"),
                             fargel = c('red', 'red'),
                             fargek = c('red', 'red'),
                             y = c(filter(edfmufl$varnavnminverdi, variable=='eisbpv')[,3], filter(edfmufl$varnavnmaksverdi, variable=='elmv')[,3]),
                             x = c(filter(edfmufl$varnavnminverdi, variable=='eisbpv')[,1], filter(edfmufl$varnavnmaksverdi, variable=='elmv')[,1]))

elabelsmufl <- list(title = 'Mundell-Fleming modellen - flytende kurs',
                   y = 'produksjon, inntekt (Y)',
                   x = 'rentenivå (i)',
                   x0 = c(TeX('$i_{0}}$')),
                   y0 = c(TeX('$Y_{0}$')),
                   kurver = edfkurvermffast)

edfmufl$yeae <- c(4.4,144)

muflflytendelikevekt <- genmakrofigure(dfnumeric=edfmufl,
                                       variables = c('eisbpv','elmv'),
                                       labt = elabelsmufl,
                                       scalejust = list(x=0, y=100))  + coord_flip()

muflflytendelikevekt

## Endring
edfkurvermffast <- data.frame(kurve=c("IS-BoP'", "LM"),
                              fargel = c('red', 'red'),
                              fargek = c('red', 'red'),
                              y = c(filter(eedfmufl$varnavnminverdi, variable=='eisbpv')[,3], filter(eedfmufl$varnavnmaksverdi, variable=='elmv')[,3]),
                              x = c(filter(eedfmufl$varnavnminverdi, variable=='eisbpv')[,1], filter(eedfmufl$varnavnmaksverdi, variable=='elmv')[,1]))

eelabelsmufl <- list(title = 'Mundell-Fleming modellen - flytende kurs',
                    y = 'produksjon, inntekt (Y)',
                    x = 'rentenivå (i=i*+rp)',
                    x0 = c(TeX('$i_{1}}$')),
                    y0 = c(TeX('$Y_{1}$')),
                    kurver = edfkurvermffast)

edfmufl$yeae <- c(4.4,144)
eedfmufl$yeae <- c(5.7, 157)

emuflfastlikevekt <- cgenmakrofigure(dfnumeric=edfmufl,
                                     edfnumeric=eedfmufl,
                                     variables = c('eisbpv','elmv'),
                                     labt = elabelsmufl,
                                     elabt = eelabelsmufl,
                                     scalejust = list(x=0, y=75)) + coord_flip()

emuflfastlikevekt

## Stabiliseringspolitkk
sedfkurvermffast <- data.frame(kurve=c("IS-BoP'", "LM'"),
                              fargel = c('red', 'red'),
                              fargek = c('red', 'red'),
                              y = c(filter(sedfmufl$varnavnminverdi, variable=='eisbpv')[,3], filter(sedfmufl$varnavnmaksverdi, variable=='elmv')[,3]),
                              x = c(filter(sedfmufl$varnavnminverdi, variable=='eisbpv')[,1], filter(sedfmufl$varnavnmaksverdi, variable=='elmv')[,1]))

seelabelsmufl <- list(title = 'Mundell-Fleming modellen - fast flytende kurs',
                     y = 'produksjon, inntekt (Y)',
                     x = 'rentenivå (i=i*+rp)',
                     x0 = c(TeX('$i_{1}}$')),
                     y0 = c(TeX('$Y_{1}$')),
                     kurver = sedfkurvermffast)

sedfmufl$yeae <- c(6.5, 144)

semuflfastlikevekt <- cgenmakrofigure(dfnumeric=edfmufl,
                                     edfnumeric=sedfmufl,
                                     variables = c('eisbpv','elmv'),
                                     labt = elabelsmufl,
                                     elabt = seelabelsmufl,
                                     scalejust = list(x=0, y=75)) + coord_flip()

semuflfastlikevekt



# NX = EX - R*IM


################## Skisse objektorientert


