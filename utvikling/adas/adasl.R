library(MakroOEKB1115)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(latex2exp)

# AD-AS likevekt
###########################################################################################################
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

# ### Mellomlang sikt
# cmsYv <- 200:250 # Guess
# cmsadasexoparvalv <- c(list(c_1 = 0.6, oC = 25, oG= 75, b = 100, oI = 75,
#                             T = 75, M= 2300, P=1, h = 2, k =2, Pe=1, mu = 0.1,
#                             l_1=-20,l_2=1, z=1, A=0.5, N=400, alpha = 1, Ac = 2), list(Y=c(Yv)))
#
# cmsdfadas <- dfgeneric(modell='adasl', exoparval = cmsadasexoparvalv)
#
# cmsadaslikevekt <- cgenmakrofigure(dfnumeric=dfadas,
#                                   variables = c(dfadas$varnavn),
#                                   labt = labelsadas,
#                                   scalejust = list(x=200, y=0)) +
#   geom_line(data=data.frame(x=dfadas$yeae[1], y=0:10), aes(x,y),linetype="dotted") +
#   geom_text()
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




#######################
#' @export cgenmakrofigure
cgenmakrofigure <- function(dfnumeric=NULL,
                            edfnumeric=NULL,
               variables = NULL,
               labt = NULL,
               elabt=NULL,
               scalejust = list(x=0, y=0)){

  # Henter dataene
  datainp <- dplyr::filter(dfnumeric$dfmodell, variable %in% variables) %>% dplyr::mutate(kat='naa')
  odatainp <- dplyr::filter(edfnumeric$dfmodell, variable %in% variables) %>% dplyr::mutate(kat='naa')

  # Scalering
  xscal <- c(dfnumeric$yeae[1], edfnumeric$yeae[1])
  yscal <- c(dfnumeric$yeae[2], edfnumeric$yeae[2])

  # Grafikk
  ggplot() +
    labs(title = labt$title, x = labt$x, y = labt$y) +
    geom_line(data = datainp, aes(x = Iv, y = value, color = factor(variable))) +
    geom_line(data = odatainp, aes(x = Iv, y = value, color = factor(variable))) +
    geom_text(data = labt$kurver, aes(x = x, y = y, label = kurve), color = labt$kurver$fargel) +
    geom_text(data = elabt$kurver, aes(x = x, y = y, label = kurve), color = elabt$kurver$fargel) +
    geom_point(aes(x=dfnumeric$yeae[1], y=dfnumeric$yeae[2])) +
    geom_point(aes(x=edfnumeric$yeae[1], y=edfnumeric$yeae[2])) +
    geom_segment(aes(x =dfnumeric$yeae[1], y = dfnumeric$yeae[2] ,
                     xend = dfnumeric$yeae[1], yend = scalejust$y), lty = 2) +
    geom_segment(aes(x = scalejust$x, y = dfnumeric$yeae[2], xend = dfnumeric$yeae[1],
                     yend = dfnumeric$yeae[2]), lty = 2) +
    geom_segment(aes(x =edfnumeric$yeae[1], y = edfnumeric$yeae[2] ,
                     xend = edfnumeric$yeae[1], yend = scalejust$y), lty = 2) +
    geom_segment(aes(x = scalejust$x, y = edfnumeric$yeae[2], xend = edfnumeric$yeae[1],
                     yend = edfnumeric$yeae[2]), lty = 2) +
    scale_x_continuous(breaks = c(dfnumeric$yeae[1], edfnumeric$yeae[1]), labels = c(labt$x0, elabt$x0)) +
    scale_y_continuous(breaks = c(dfnumeric$yeae[2], edfnumeric$yeae[2]), labels = c(labt$y0, elabt$y0)) +
    scale_colour_manual(values = labt$kurver$fargek) +
    theme_classic() +
    theme(legend.position="none")
}






######################
# p <- adaslikevekt + eksadaslikevekt +
#   geom_point(aes(x=edfnumeric$yeae[1], y=edfnumeric$yeae[2])) +
#   geom_text(data = alabt$kurver, aes(x = x, y = y, label = kurve), color = alabt$kurver$fargek) +
#   geom_segment(aes(x =edfnumeric$yeae[1], y = edfnumeric$yeae[2] ,
#                    xend = edfnumeric$yeae[1], yend = scalejust$y), lty = 2) +
#   geom_segment(aes(x = scalejust$x, y = edfnumeric$yeae[2], xend = edfnumeric$yeae[1],
#                    yend = edfnumeric$yeae[2]), lty = 2) +
#   scale_x_continuous(breaks = edfnumeric$yeae[1], labels = alabt$x0) +
#   scale_y_continuous(breaks = edfnumeric$yeae[2], labels = alabt$y0)


# ### Mellomlang sikt
# cmsYv <- 200:250 # Guess
# cmsadasexoparvalv <- c(list(c_1 = 0.6, oC = 25, oG= 75, b = 100, oI = 75,
#                             T = 75, M= 2300, P=1, h = 2, k =2, Pe=1, mu = 0.1,
#                             l_1=-20,l_2=1, z=1, A=0.5, N=400, alpha = 1, Ac = 2), list(Y=c(Yv)))
#
# cmsdfadas <- dfgeneric(modell='adasl', exoparval = cmsadasexoparvalv)
#
# cmsadaslikevekt <- cgenmakrofigure(dfnumeric=dfadas,
#                                   variables = c(dfadas$varnavn),
#                                   labt = labelsadas,
#                                   scalejust = list(x=200, y=0)) +
#   geom_line(data=data.frame(x=dfadas$yeae[1], y=0:10), aes(x,y),linetype="dotted") +
#   geom_text()
#
# ## AD-AS stabiliseringspolitikk
# ### Finanspolitikk
# stYv <- 200:250 # Guess
# stadasexoparvalv <- c(list(c_1 = 0.6, oC = 25, oG= 75, b = 100, oI = 75,
#                             T = 75, M= 2300, P=1, h = 2, k =2, Pe=1, mu = 0.1,
#                             l_1=-20,l_2=1, z=1, A=0.5, N=400, alpha = 1, Ac = 2), list(Y=c(Yv)))
#
# cmsdfadas <- dfgeneric(modell='adasl', exoparval = stadasexoparvalv)
#
# sgadaslikevekt <- cgenmakrofigure(dfnumeric=dfadas,
#                                   variables = c(dfadas$varnavn),
#                                   labt = labelsadas,
#                                   scalejust = list(x=200, y=0)) +
#   geom_line(data=data.frame(x=dfadas$yeae[1], y=0:10), aes(x,y),linetype="dotted") +
#   geom_text()
