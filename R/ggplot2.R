#' @export gpmakro
gpmakro <- function(data = moltmacrousa, variables = c('lngdp', 'hptrendg'), katv = c('gdp'),
                    startp="1929-04-01", endp =" 2017-12-12",
                    variabeln = c('var1', 'var2'),
                    labt = list(x='x-variabel', y='y-variabel')){
  # Filtrere datasettet
  datainp <- dplyr::filter(data, variable %in% variables, kat %in% katv, date >= startp & date <= endp)
  # Lager plot
  ggplot(data = datainp, aes(x = date, y = value, color = factor (variable, labels = variabeln))) +   geom_line() + theme_classic() +
    labs(color = "Variabel:",x = labt$x, y = labt$y) +
    labs(x = labt$x, y = labt$y)
}



gpmodell <- function(data = datainp){
  datainp
    # keykg <- ggplot(data = datainp, aes(x = x, y = value, color = factor(variable))) +
    # geom_line() +
    # geom_point(aes(x=0, y=0)) +
    # geom_segment(aes(x = 500, y = 0, xend = 500, yend = 100), lty = 2) +
    # #geom_text(aes(x=500, y=500, label=TeX('$\theta$')), parse=TRUE) +
    # scale_x_continuous(name=lablist$x, breaks = c(400), labels = c(TeX('$Y_{0}$')) ) +
    # scale_y_continuous(name=lablist$y, breaks = c(400), labels = c('def') ) +
    # labs(title= 'Keyneskrysset') + theme_classic() +
    # theme(legend.position="none") +
    # coord_cartesian()

}
