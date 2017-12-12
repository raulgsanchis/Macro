#' @export gpmakro
gpmakro <- function(data=tusaunem, startp="1975-07-01", endp =" 2017-12-12", labt = list(x=NULL, y=NULL)){
  datainp <- dplyr::filter(data, date > startp & date < endp)
  ggplot(data = datainp, aes(date, value)) + geom_line() + theme_classic() +
    #geom_smooth(method = 'loess', color = 'red', size = 0.5, se = FALSE) +
    labs(x = labt$x, y = labt$y)
}

