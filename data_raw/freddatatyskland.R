# 1. Download data
#https://fred.stlouisfed.org/series/GDPCA
#https://fred.stlouisfed.org/series/M0892AUSM156SNBR

devtools::install_github("jcizel/FredR")
library(FredR)
library(pipeR)
library(ggplot2)
library(dplyr)
library(mFilter)
library(MASS)
library(latex2exp)

api.key = 'd62b9d8d4ce53e56ea04049dc463ac51'  # substitute ... with your API key
fred <- FredR(api.key)
str(fred,1)
macro.series1 <- fred$series.search("Germany")

# Makrotall
landgdp <- fred$series.observations(series_id = c('RGDPNADEA666NRUG'))
landunem <- rbind(fred$series.observations(series_id = c('LMUNRRTTDEA156N')))
landpricei <- fred$series.observations(series_id = c('DEUCPIALLMINMEI'))

# 2. Cleaning up data
tlandgdp  <- landgdp %>% dplyr::select(-1, -2) %>% dplyr::mutate(date = as.Date(date), value = as.numeric(value)) %>% arrange(date)
tlandunem <- landunem %>% dplyr::select(-1, -2) %>% dplyr::mutate(date = as.Date(date), value = as.numeric(value)) %>% arrange(date)
tlandpricei <- landpricei %>% dplyr::select(-1, -2) %>% dplyr::mutate(date = as.Date(date), value = as.numeric(value)) %>% arrange(date)

# 3. Manipulating og transformerer dataene
molttlandgdp <- tlandgdp %>%
  dplyr::mutate(gdp = ts(tlandgdp$value,c(as.numeric(substring(tlandgdp$date[1],1,4)),as.numeric(substring(tlandgdp$date[1],6,7))), frequency = 4)) %>%
  dplyr::select(-value) %>%
  dplyr::mutate(date = substring(date,1,4)) %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(gdp = mean(gdp)) %>%
  base::unique() %>%
  dplyr::ungroup() %>%
  dplyr::mutate(lngdp = log(gdp)) %>%
  dplyr::mutate(Lgdp = lag(gdp,n=1)) %>%
  dplyr::mutate(Llngdp = lag(lngdp,n=1)) %>%
  dplyr::mutate(ggdp = round(gdp/Lgdp-1, digits=4)) %>%
  dplyr::mutate(glgdp = round(lngdp - Llngdp, digits = 4)) %>%
  dplyr::mutate(hpcycleg = hpfilter(lngdp, freq = 100)$cycle) %>%
  dplyr::mutate(hptrendg = hpfilter(lngdp, freq = 100)$trend) %>%
  reshape2::melt(id.vars = c("date")) %>%
  dplyr::mutate(kat=c('gdp'))

#ggplot(data = dplyr::filter(molttlandgdp, variable %in% c('lngdp', 'hptrendg')), aes(x = date, y =  value)) + geom_line(aes(color = variable))

nunem <- mean(tlandunem$value)
molttlandunem <- tlandunem %>%
  dplyr::mutate(unem = ts (value)) %>%
  dplyr::select(-value) %>%
  dplyr::mutate(date = substring(date,1,4)) %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(unem = mean(unem)) %>%
  base::unique() %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Lunem = lag(unem, n = 1)) %>%
  dplyr::mutate(cunem = round(unem - Lunem, digits = 4)) %>%
  #dplyr::mutate(hpcycleu = hpfilter(unem, freq = 100)$cycle) %>%
  #dplyr::mutate(hptrendu = hpfilter(unem, freq = 100)$trend) %>%
  dplyr::mutate(trendu = nunem) %>%
  reshape2::melt(id.vars = c("date")) %>%
  dplyr::mutate(kat = 'unem')

molttlandpricei <- tlandpricei %>%
  dplyr::mutate(pricei = ts(value)) %>%
  dplyr::select(-value) %>%
  dplyr::mutate(date = substring(date,1,4)) %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(pricei = mean(pricei)) %>%
  base::unique() %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Lpricei = lag(pricei, n = 1)) %>%
  dplyr::mutate(inflation = round((pricei - Lpricei)/Lpricei, digits=4)) %>%
  dplyr::mutate(Linflation = lag(inflation, n = 1)) %>%
  dplyr::mutate(cinflation = round(inflation - Linflation,digits=4)) %>%
  reshape2::melt(id.vars = c("date")) %>%
  dplyr::mutate(kat = 'inf')

## Samler alle dataene for land
<<<<<<< HEAD
moltmacroger <- rbind(molttlandunem, molttlandgdp, molttlandpricei) %>% dplyr::mutate(land='ger') %>%
  dplyr::mutate(freqm = substring(date,6,7))

=======
moltmacroger <- rbind(molttlandunem, molttlandgdp, molttlandpricei) %>% dplyr::mutate(land='nor') %>%
  dplyr::mutate(freqm = substring(date,6,7))


>>>>>>> kontoret
# 4. Saving data in Rda-format
devtools::use_data(moltmacroger, overwrite = TRUE)

# # Appendiks: grafikk
# Henter datasett
lmoltmacroger <- reshape2::dcast(moltmacroger, date  + land ~ variable )
names(lmoltmacroger)

okuns <-
  qplot(data = lmoltmacroger, x = cunem, y = ggdp) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title= 'Okuns lov - Tyskland', x='Endring i ledighet', y = 'Vekst i BNP (real)')

ggsave(paste0(devtools::as.package(".")$path,'/inst/webside/figurer/sem1/okunsger.png'))

phillips <- qplot(x = unem, y = cinflation, data = lmoltmacroger, geom = c('point')) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title= 'Phillips-kurven - Tyskland', x='Ledighetsrate', y = 'Endring i inflation')

<<<<<<< HEAD
ggsave(paste0(devtools::as.package(".")$path,'/inst/webside/figurer/sem1/phillipsger.png'))
=======

ggsave(paste0(devtools::as.package(".")$path,'/inst/webside/figurer/sem1/phillipsger.png'))


>>>>>>> kontoret
