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
  geom_line(data=data.frame(x=dfadas$yeae[1], y=0:10), aes(x,y),linetype="dotted")

#+
#  geom_text()


## AD-AS komparativ statikk (skiftanalyse)
### Kort sikt
cksYv <- 200:250 # Guess
cksadasexoparvalv <- c(list(c_1 = 0.6, oC = 25, oG= 75, b = 100, oI = 75,
                         T = 75, M= 2300, P=1, h = 2, k =2, Pe=1, mu = 0.1,
                         l_1=-20,l_2=1, z=1, A=0.5, N=400, alpha = 1, Ac = 2), list(Y=c(Yv)))

cksdfadas <- dfgeneric(modell='adasl', exoparval = cksadasexoparvalv)

cdfkurver = data.frame(kurve=c("AD'", "AS"),
                       fargel = c('red', 'red'),
                       fargek = c('red', 'red'),
                       x = cksdfadas$varnavnmaksverdi$Iv,
                       y = cksdfadas$varnavnmaksverdi$value+2)

labelsadas <- list(title = 'AD-AS modellen',
                   x = 'produksjon, inntekt (Y)',
                   y = 'prisnivå (P)',
                   x0 = c(TeX('$Y_{1}$')),
                   y0 = c(TeX('$P_{1}$')),
                   kurver = cdfkurver)


eksadaslikevekt <- cgenmakrofigure(edfnumeric = cksdfadas,
                                   evariables = c(cksdfadas$varnavn),
                                   alabt = labelsadas)

p <- adaslikevekt + eksadaslikevekt +
  geom_point(aes(x=edfnumeric$yeae[1], y=edfnumeric$yeae[2])) +
  geom_text(data = alabt$kurver, aes(x = x, y = y, label = kurve), color = alabt$kurver$fargek) +
  geom_segment(aes(x =edfnumeric$yeae[1], y = edfnumeric$yeae[2] ,
                   xend = edfnumeric$yeae[1], yend = scalejust$y), lty = 2) +
  geom_segment(aes(x = scalejust$x, y = edfnumeric$yeae[2], xend = edfnumeric$yeae[1],
                   yend = edfnumeric$yeae[2]), lty = 2) +
  scale_x_continuous(breaks = edfnumeric$yeae[1], labels = alabt$x0) +
  scale_y_continuous(breaks = edfnumeric$yeae[2], labels = alabt$y0)

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


######################################################################################
adaslikevekt <- genmakrofigure(dfnumeric=dfadas,
                               variables = c(dfadas$varnavn),
                               labt = labelsadas,
                               scalejust = list(x=200, y=0))
######################################################################################
cksYv <- 200:250 # Guess
cksadasexoparvalv <- c(list(c_1 = 0.6, oC = 25, oG= 75, b = 100, oI = 75,
                            T = 75, M= 2500, P=1, h = 2, k =2, Pe=1, mu = 0.1,
                            l_1=-20,l_2=1, z=1, A=0.5, N=400, alpha = 1, Ac = 2), list(Y=c(Yv)))

cksdfadas <- dfgeneric(modell='adasl', exoparval = cksadasexoparvalv)

#dfadas$varnavnmaksverdi
#cdfadas$varnavnmaksverdi
######################################################################################
cdfkurver = data.frame(kurve=c("AD'", "AS"),
                      fargel = c('red', 'red'),
                      fargek = c('red', 'red'),
                      x = cksdfadas$varnavnmaksverdi$Iv,
                      y = cksdfadas$varnavnmaksverdi$value+2)

labelsadas <- list(title = 'AD-AS modellen',
                   x = 'produksjon, inntekt (Y)',
                   y = 'prisnivå (P)',
                   x0 = c(TeX('$Y_{1}$')),
                   y0 = c(TeX('$P_{1}$')),
                   kurver = cdfkurver)


eksadaslikevekt <- cgenmakrofigure(edfnumeric = cksdfadas,
                                   evariables = c(cksdfadas$varnavn),
                                   alabt = labelsadas)

p <- adaslikevekt + eksadaslikevekt +
  geom_point(aes(x=edfnumeric$yeae[1], y=edfnumeric$yeae[2])) +
  geom_text(data = alabt$kurver, aes(x = x, y = y, label = kurve), color = alabt$kurver$fargek) +
  geom_segment(aes(x =edfnumeric$yeae[1], y = edfnumeric$yeae[2] ,
                 xend = edfnumeric$yeae[1], yend = scalejust$y), lty = 2) +
  geom_segment(aes(x = scalejust$x, y = edfnumeric$yeae[2], xend = edfnumeric$yeae[1],
                   yend = edfnumeric$yeae[2]), lty = 2) +
  scale_x_continuous(breaks = edfnumeric$yeae[1], labels = alabt$x0) +
  scale_y_continuous(breaks = edfnumeric$yeae[2], labels = alabt$y0)



#' @export genmakrofigure
cgenmakrofigure <- function(edfnumeric = NULL,
                            evariables = NULL,
                            alabt = NULL){

  edatainp <- dplyr::filter(edfnumeric$dfmodell, variable %in% evariables) %>% dplyr::mutate(kat='naa')

  #browser()

  geom_line(data = edatainp, aes(x = Iv, y = value, color = factor(variable))) #+
  #geom_point(aes(x=edfnumeric$yeae[1], y=edfnumeric$yeae[2]))
  #geom_text(data = alabt$kurver, aes(x = x, y = y, label = kurve), color = alabt$kurver$fargek) #+

}




# Henter dataene
  datainp <- dplyr::filter(dfnumeric$dfmodell, variable %in% variables) %>% dplyr::mutate(kat='tid')

  # Grafikk
  ggplot() +
    labs(title = labt$title, x = labt$x, y = labt$y) +
    geom_line(data = datainp, aes(x = Iv, y = value, color = factor(variable))) +
    geom_point(aes(x=dfnumeric$yeae[1], y=dfnumeric$yeae[2])) +
    geom_text(data = labt$kurver, aes(x = x, y = y, label = kurve), color = labt$kurver$fargel) +
    geom_segment(aes(x =dfnumeric$yeae[1], y = dfnumeric$yeae[2] ,
                     xend = dfnumeric$yeae[1], yend = scalejust$y), lty = 2) +
    geom_segment(aes(x = scalejust$x, y = dfnumeric$yeae[2], xend = dfnumeric$yeae[1],
                     yend = dfnumeric$yeae[2]), lty = 2) +
    scale_x_continuous(breaks = dfnumeric$yeae[1], labels = labt$x0) +
    scale_y_continuous(breaks = dfnumeric$yeae[2], labels = labt$y0) +
    scale_colour_manual(values = labt$kurver$fargek) +
    theme_classic() +
    theme(legend.position="none")

}
