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
                      scalejust = list(x=200, y=0)) +
  geom_line(data=data.frame(x=dfadas$yeae[1], y=0:10), aes(x,y),linetype="dotted") +
  geom_text()


## AD-AS komparativ statikk (skiftanalyse)
### Kort sikt
eksadaslikevekt <- genmakrofigure(dfnumeric=dfadas,
                               variables = c(dfadas$varnavn),
                               labt = labelsadas,
                               scalejust = list(x=200, y=0)) +
  geom_line(data=data.frame(x=dfadas$yeae[1], y=0:10), aes(x,y),linetype="dotted") +
  geom_text()

### Mellomlang sikt
emsadaslikevekt <- genmakrofigure(dfnumeric=dfadas,
                                  variables = c(dfadas$varnavn),
                                  labt = labelsadas,
                                  scalejust = list(x=200, y=0)) +
  geom_line(data=data.frame(x=dfadas$yeae[1], y=0:10), aes(x,y),linetype="dotted") +
  geom_text()

## AD-AS stabiliseringspolitikk
### Finanspolitikk
sgadaslikevekt <- genmakrofigure(dfnumeric=dfadas,
                                  variables = c(dfadas$varnavn),
                                  labt = labelsadas,
                                  scalejust = list(x=200, y=0)) +
  geom_line(data=data.frame(x=dfadas$yeae[1], y=0:10), aes(x,y),linetype="dotted") +
  geom_text()


adaslikevekt
eksadaslikevekt
emsadaslikevekt
sgadaslikevekt
