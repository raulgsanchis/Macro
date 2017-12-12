#https://fred.stlouisfed.org/series/GDPCA
#https://fred.stlouisfed.org/series/M0892AUSM156SNBR

devtools::install_github("jcizel/FredR")
library(FredR)
library(pipeR)
library(ggplot2)
library(dplyr)

api.key = 'd62b9d8d4ce53e56ea04049dc463ac51'  # substitute ... with your API key
fred <- FredR(api.key)
str(fred,1)
macro.series1 <- fred$series.search("GDP")
macro.series2 <- fred$series.search("unemployment")
macro.series3 <- fred$series.search("money")
macro.series4 <- fred$series.search("inflation")

# Makrotall USA
usainf <- fred$series.observations(series_id = c('CPIAUCSL', 'CPILFESL')[1])
usamone <- fred$series.observations(series_id = c('M1'))
usagdp <- fred$series.observations(series_id = c('GDPCA'))
usaunem <- rbind(fred$series.observations(series_id = c('M0892AUSM156SNBR')), fred$series.observations(series_id = c('UNRATE')))

tusainf <- usainf %>% dplyr::mutate(date = as.Date(date), value = as.numeric(value)) %>% arrange(date)
tusamone <- usamone %>% dplyr::mutate(date = as.Date(date), value = as.numeric(value)) %>% arrange(date)
tusagdp  <- usagdp %>% dplyr::mutate(date = as.Date(date), value = as.numeric(value)) %>% arrange(date)
tusaunem <- usaunem %>% dplyr::mutate(date = as.Date(date), value = as.numeric(value)) %>% arrange(date)


devtools::use_data(tusagdp, overwrite = TRUE)
devtools::use_data(tusaunem, overwrite = TRUE)
devtools::use_data(tusainf, overwrite = TRUE)
devtools::use_data(tusamone, overwrite = TRUE)


a <- gpmakro(tusaunem)


gpmakro <- function(data=tusaunem, startp="1975-07-01", endp =" 2017-12-12", labt = list(x=NULL, y=NULL)){
  datainp <- dplyr::filter(data, date > startp & date < endp)
  ggplot(data = datainp, aes(date, value)) + geom_line() + theme_classic() +
  geom_smooth(method = 'loess', color = 'red', size = 0.5, se = FALSE) +
  labs(x = labt$x, y = labt$y)
}




dt <- tusaunem %>%
  select(
    date,
    value
  ) %>%
  mutate(
    date = as.Date(date),
    value = as.numeric(value)) %>% arrange(date)

qplot(data = dt, x = date, y = value, geom = 'line')

library(ggplot2)
a <- ggplot2::ggplot(data = dt, x = date, y = value) + geom_abline(aes(intercept=0, slope = 1, size = 10)) +
  labs(title='abc', subtitle = 'def', caption ='123', x='x', y = 'y', AES = 'AES')
