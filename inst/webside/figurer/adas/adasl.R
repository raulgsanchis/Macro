library(MakroOEKB1115)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(latex2exp)

# AD-AS likevekt
Yv <- 200:250 # Guess
adasexoparvalv <- c(list(c_1 = 0.6, oC = 25, oG= 75, b = 100, oI = 75,
                           T = 75, M= 2000, P=1, h = 2, k =2, Pe=1, mu = 0.1,
                           l_1=-20,l_2=1, z=1, A=0.5, N=400, alpha = 1, Ac = 2), list(Y=c(Yv)))

dfadas <- dfgeneric(modell='adasl', exoparval = adasexoparvalv)

dfkurver = data.frame(kurve=c("AD", "AS"),
                    farge = c('black', 'black'),
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
                      scalejust = list(x=250, y=0))




# # ADAS-komparativ statikk (skiftanalyse)
# ## Kort sikt
# adaslabelsc <- list(title= 'AD-AS modellen', x='produksjon, inntekt (Y)', y='prisnivå (P)')
#
# Yv <- 200:250 # Guess
# cexoparvalvadas <- c(list(c_1 = 0.6, oC = 25, oG= 75, b = 100, oI = 75,
#                            T = 75, M= 2200, P=1, h = 2, k =2, Pe=1, mu = 0.1,
#                            l_1=-20,l_2=1, z=1, A=0.5, N=400, alpha = 1, Ac = 2), list(Y=c(Yv)))
#
# cdfadas <- dfgpmakro3(Iv=Yv, exoparval=cexoparvalvadas, modell='ad-asc', endr=0)
#
#
# csnatfigadas <- makrofigurechange(ndata = dfadas$dfmodell,
#                                  variables = c("adv", "asv"),
#                                  labt = adaslabels,
#                                  labplassmon = labldfislm,
#                                  equisol = ceqsol,
#                                  scalebreaksx = ceqlinjex,
#                                  scalebreaksy = ceqlinjey,
#                                  odata = cdfadas$dfmodell,
#                                  ovariables = c("adv", "asv"),
#                                  color = rep('black',4)) + coord_flip()
#
# csnatfigadas
#
# ## Mellomlang sikt
# Yv <- 200:250 # Guess
# ccexoparvalvadas <- c(list(c_1 = 0.6, oC = 25, oG= 75, b = 100, oI = 75,
#                           T = 75, M= 2200, P=1, h = 2, k =2, Pe=1.5, mu = 0.1,
#                           l_1=-20,l_2=1, z=1, A=0.5, N=400, alpha = 1, Ac = 2), list(Y=c(Yv)))
#
# ccdfadas <- dfgpmakro3(Iv=Yv, exoparval=cexoparvalvadas, modell='ad-asc', endr=0)
#
# ccsnatfigadas <- makrofigurechange(ndata = dfadas$dfmodell,
#                                   variables = c("adv", "asv"),
#                                   labt = adaslabels,
#                                   labplassmon = labldfislm,
#                                   equisol = ceqsol,
#                                   scalebreaksx = ceqlinjex,
#                                   scalebreaksy = ceqlinjey,
#                                   odata = ccdfadas$dfmodell,
#                                   ovariables = c("adv", "asv"),
#                                   color = rep('black',4)) + coord_flip()
#
# ccsnatfigadas
#
# # AD-AS stabiliseringspolitikk
# adaslabelstab <- list(title= 'AD-AS modellen', x='produksjon, inntekt (Y)', y='prisnivå (P)')
#
# Yv <- 200:250 # Guess
# sexoparvalvadas <- c(list(c_1 = 0.6, oC = 25, oG= 75, b = 100, oI = 75,
#                           T = 75, M= 2200, P=1, h = 2, k =2, Pe=1, mu = 0.1,
#                           l_1=-20,l_2=1, z=1, A=0.5, N=400, alpha = 1, Ac = 2), list(Y=c(Yv)))
#
# stabdfadas <- dfgpmakro3(Iv=Yv, exoparval=cexoparvalvadas, modell='ad-asc', endr=0)
#
# stabsnatfigadas <- makrofigurechange(ndata = dfadas$dfmodell,
#                                    variables = c("adv", "asv"),
#                                    labt = adaslabels,
#                                    labplassmon = labldfislm,
#                                    equisol = ceqsol,
#                                    scalebreaksx = ceqlinjex,
#                                    scalebreaksy = ceqlinjey,
#                                    odata = ccdfadas$dfmodell,
#                                    ovariables = c("adv", "asv"),
#                                    color = rep('black',4)) + coord_flip()
#
# stabsnatfigadas
