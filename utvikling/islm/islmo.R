library(MakroOEKB1115)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(latex2exp)
# # Declaring variables
# ## Endogenous
# Y, C, I, G, Ld, Ms, P, M, NX, R, IM, EX, E = sympy.symbols('Y C I G L_d M_s  P M NX R IM EX E')
# ## Parametere og konstantledd
# oC, c1, b, oI, k, h, t, m, m1, m2, x1, x2, P, Ps = sympy.symbols('oC c_1 b oI k h t m m1 m2 x1 x2 P Ps')
# ## Exogene styringsvariableh
# oG, i, T, Ys, rp, i_s = sympy.symbols('oG i T Ys rp i_s')
#######################################################################################################################
iv <- 0:5
islmexoparvalv <- c(list(c_1 = 0.6, oC = 50, oG= 50, b = 10, oI = 10, T = 50, M= 100,
                         P=1, h = 10, k =1, Y = 130, m=1, E=1, Ps=1,
                         m1=0.5, m2=0.5, x1=0.5, x2=0.5, Ys=100, i_s=3, rp=0), list(i=c(iv)))

# eislmexoparvalv <- c(list(c_1 = 0.6, oC = 50, oG= 50, b = 10, oI = 10, T = 50, M= 110,
#                           P=1, h = 10, k =1, Y = 130, m=1), list(i=c(iv)))

dfislm <- dfgeneric(modell='islmo', exoparval = islmexoparvalv, eqsel = c(1,2))
#edfislm <- dfgeneric(modell='islmo', exoparval = eislmexoparvalv, eqsel = c(1,3))

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

fastmflikevekt <- genmakrofigure(dfnumeric=dfislm,
                               variables = c(dfislm$varnavn)[c(1,2)],
                               labt = labelsislm,
                               scalejust = list(x=0, y=75))  + coord_flip()

flytmflikevekt <- genmakrofigure(dfnumeric=dfislm,
                                 variables = c(dfislm$varnavn)[c(1,2)],
                                 labt = labelsislm,
                                 scalejust = list(x=0, y=75))  + coord_flip()
