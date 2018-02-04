library(MakroOEKB1115)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(latex2exp)

# ADAS-equilibrium
adaslabels <- list(title= 'AD-AS modellen', x='produksjon, inntekt (Y)', y='prisnivå (P)')

Yv <- 200:250 # Guess
exoparvalvadas <- c(list(c_1 = 0.6, oC = 25, oG= 75, b = 100, oI = 75,
                           T = 75, M= 2000, P=1, h = 2, k =2, Pe=1, mu = 0.1,
                           l_1=-20,l_2=1, z=1, A=0.5, N=400, alpha = 1, Ac = 2), list(Y=c(Yv)))

dfadas <- dfgpmakro3(Iv=Yv, exoparval=exoparvalvadas, modell='ad-asc', endr=0)
eqsoladas <- list(y= c(dfadas$yeae[2]), x= c(dfadas$yeae[1]))
eqlinjexadas <- list(breaksvx = eqsoladas$x, labels = c(TeX('$Y_{0}=Y^{N}$')))
eqlinjeyadas <- list(breaksvy = eqsoladas$y, labels = c(TeX('$P_{0}$')))

xxadas <- c(rev(subset(dfadas$dfmodell, variable = 'adv')$Iv)[1], rev(subset(dfadas$dfmodell, variable = 'asv')$Iv)[1])

yyadas <- c(rev(subset(dfadas$dfmodell, variable == 'adv')$value)[1],
        rev(subset(dfadas$dfmodell, variable == 'asv')$value)[1])

labldf <- data.frame(labeling=c("AD", "AS"),
                     x = xxadas,
                     y = yyadas,
                     col = c('red','red'))

figadas <- makrofigure(ndata = dfadas$dfmodell,
                       variables = c("adv", "asv"),
                       labt = adaslabels,
                       labplassmon = labldf,
                       equisol = eqsoladas,
                       scalebreaksx = eqlinjexadas ,
                       scalebreaksy = eqlinjeyadas,
                       colorl = rep(c('black'),2),
                       starts = list(x=200,y=0))

natfigadas <- figadas + geom_line(data=data.frame(x=dfadas$yeae[1],y=0:10), aes(x,y),linetype="dotted")

# ADAS-komparativ statikk (skiftanalyse)
## Kort sikt
adaslabelsc <- list(title= 'AD-AS modellen', x='produksjon, inntekt (Y)', y='prisnivå (P)')

Yv <- 200:250 # Guess
cexoparvalvadas <- c(list(c_1 = 0.6, oC = 25, oG= 75, b = 100, oI = 75,
                           T = 75, M= 2200, P=1, h = 2, k =2, Pe=1, mu = 0.1,
                           l_1=-20,l_2=1, z=1, A=0.5, N=400, alpha = 1, Ac = 2), list(Y=c(Yv)))

cdfadas <- dfgpmakro3(Iv=Yv, exoparval=cexoparvalvadas, modell='ad-asc', endr=0)


csnatfigadas <- makrofigurechange(ndata = dfadas$dfmodell,
                                 variables = c("adv", "asv"),
                                 labt = adaslabels,
                                 labplassmon = labldfislm,
                                 equisol = ceqsol,
                                 scalebreaksx = ceqlinjex,
                                 scalebreaksy = ceqlinjey,
                                 odata = cdfadas$dfmodell,
                                 ovariables = c("adv", "asv"),
                                 color = rep('black',4)) + coord_flip()

csnatfigadas

## Mellomlang sikt
Yv <- 200:250 # Guess
ccexoparvalvadas <- c(list(c_1 = 0.6, oC = 25, oG= 75, b = 100, oI = 75,
                          T = 75, M= 2200, P=1, h = 2, k =2, Pe=1.5, mu = 0.1,
                          l_1=-20,l_2=1, z=1, A=0.5, N=400, alpha = 1, Ac = 2), list(Y=c(Yv)))

ccdfadas <- dfgpmakro3(Iv=Yv, exoparval=cexoparvalvadas, modell='ad-asc', endr=0)

ccsnatfigadas <- makrofigurechange(ndata = dfadas$dfmodell,
                                  variables = c("adv", "asv"),
                                  labt = adaslabels,
                                  labplassmon = labldfislm,
                                  equisol = ceqsol,
                                  scalebreaksx = ceqlinjex,
                                  scalebreaksy = ceqlinjey,
                                  odata = ccdfadas$dfmodell,
                                  ovariables = c("adv", "asv"),
                                  color = rep('black',4)) + coord_flip()

ccsnatfigadas

# AD-AS stabiliseringspolitikk
adaslabelstab <- list(title= 'AD-AS modellen', x='produksjon, inntekt (Y)', y='prisnivå (P)')

Yv <- 200:250 # Guess
sexoparvalvadas <- c(list(c_1 = 0.6, oC = 25, oG= 75, b = 100, oI = 75,
                          T = 75, M= 2200, P=1, h = 2, k =2, Pe=1, mu = 0.1,
                          l_1=-20,l_2=1, z=1, A=0.5, N=400, alpha = 1, Ac = 2), list(Y=c(Yv)))

stabdfadas <- dfgpmakro3(Iv=Yv, exoparval=cexoparvalvadas, modell='ad-asc', endr=0)

stabsnatfigadas <- makrofigurechange(ndata = dfadas$dfmodell,
                                   variables = c("adv", "asv"),
                                   labt = adaslabels,
                                   labplassmon = labldfislm,
                                   equisol = ceqsol,
                                   scalebreaksx = ceqlinjex,
                                   scalebreaksy = ceqlinjey,
                                   odata = ccdfadas$dfmodell,
                                   ovariables = c("adv", "asv"),
                                   color = rep('black',4)) + coord_flip()

stabsnatfigadas





