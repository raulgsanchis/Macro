library(MakroOEKB1115)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(latex2exp)
#######################################################################################################################
iv <- 0:5
openpar <- list(i_s=1.5, rp=0.25, E=1, Ps=1, x1=20, x2=0.1, m1=15, m2=0.1, Ys=200, rp=0)
muflexoparvalv <- c(list(c_1 = 0.6, oC = 50, oG= 50, b = 10, oI = 10, T = 50, M= 100,
                         P=1, h = 10, k =1, Y = 130, m=1, t=0.4), openpar, list(i=c(iv)))

eopenpar <- list(i_s=1, rp=0, E=1, Ps=1, x1=20, x2=0.1, m1=15, m2=0.1, Ys=200, rp=0)
emuflexoparvalv <- c(list(c_1 = 0.6, oC = 50, oG= 50, b = 10, oI = 10, T = 50, M= 100,
                          P=1, h = 10, k =1, Y = 130, m=1, t=0.4), eopenpar, list(i=c(iv)))

seopenpar <- list(i_s=1.0, rp=0.25, E=1, Ps=1, x1=20, x2=0.1, m1=15, m2=0.1, Ys=200, rp=0)
semuflexoparvalv <- c(list(c_1 = 0.6, oC = 50, oG= 30, b = 10, oI = 10, T = 50, M= 110,
                          P=1, h = 10, k =1, Y = 130, m=1, t=0.4), eopenpar, list(i=c(iv)))


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
#
# # Flytende kurs
# dfkurvermffast <- data.frame(kurve=c("IS", "LM"),
#                              fargel = c('red', 'red'),
#                              fargek = c('red', 'red'),
#                              y = c(dfmufl$varnavnminverdi$value[c(1)], dfmufl$varnavnmaksverdi$value[c(2)]),
#                              x = c(dfmufl$varnavnminverdi$Iv[c(1)],dfmufl$varnavnmaksverdi$Iv[c(2)]))
#
# labelsmufl <- list(title = 'Mundell-Fleming modellen - flytende kurs',
#                    y = 'produksjon, inntekt (Y)',
#                    x = 'rentenivå (i)',
#                    x0 = c(TeX('$i_{0}}$')),
#                    y0 = c(TeX('$Y_{0}$')),
#                    kurver = dfkurvermffast)
#
# muflflytenmuflfastlikevektdelikevekt <- genmakrofigure(dfnumeric=dfmufl,
#                                                        variables = c(dfmufl$varnavn)[c(1,2)],
#                                                        labt = labelsmufl,
#                                                        scalejust = list(x=0, y=75))  + coord_flip()
# muflflytenmuflfastlikevektdelikevekt
