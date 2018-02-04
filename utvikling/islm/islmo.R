library(MakroOEKB1115)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(latex2exp)

# IS-LM likevekt
iv <- 0:7.5
islmexoparvalv <- c(list(c_1 = 0.6, oC = 25, oG= 75, b = 15, oI = 75, T = 10, M= 200, P=1, h = 80, k =2, Y = 300), list(i=c(iv)))

#!: endogenisere gjetteverdier
dfislm <- dfgeneric(modell='islm', exoparval = islmexoparvalv)

dfkurver <- data.frame(kurve=c("IS", "LM"),
                      fargel = c('red', 'red'),
                      fargek = c('red', 'red'),
                      y = dfislm$varnavnmaksverdi$value[c(3,4)],
                      x = dfislm$varnavnmaksverdi$Iv[c(3,4)])


labelsislm <- list(title = 'IS-LM modellen',
                   y = 'produksjon, inntekt (Y)',
                   x = 'rentenivå (i)',
                   x0 = c(TeX('$i_{0}}$')),
                   y0 = c(TeX('$Y_{0}$')),
                   kurver = dfkurver)

islmlikevekt <- genmakrofigure(dfnumeric=dfislm,
                               variables = c(dfislm$varnavn)[c(3,4)],
                               labt = labelsislm,
                               scalejust = list(x=0, y=0))  + coord_flip()

# islmlikevekt
#' @export genmakrofigure
genmakrofigure <- function(dfnumeric = NULL,
                           variables = NULL,
                           labt = labelsadas,
                           scalejust = list(x=0, y=0)){
  # Henter dataene
  datainp <- dplyr::filter(dfnumeric$dfmodell, variable %in% variables) %>% dplyr::mutate(kat='naa')

  # Grafikk
  ggplot() +
    labs(title = labt$title, x = labt$x, y = labt$y) +
    geom_line(data = datainp, aes(x = Iv, y = value, color = factor(variable))) +
    geom_text(data = labt$kurver, aes(x = x, y = y, label = kurve), color = labt$kurver$fargel) +
    geom_point(aes(x=dfnumeric$yeae[1], y=dfnumeric$yeae[2])) +
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
