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
