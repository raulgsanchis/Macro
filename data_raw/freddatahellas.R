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
macro.series1 <- fred$series.search("Greece")

# Makrotall
landgdp <- fred$series.observations(series_id = c('RGDPNAGRA666NRUG'))
landunem <- rbind(fred$series.observations(series_id = c('LRHUTTTTGRM156S')))
landpricei <- fred$series.observations(series_id = c('GRCCPIALLAINMEI'))

# 2. Cleaning up data
tlandgdp  <- landgdp %>% dplyr::select(-1, -2) %>% dplyr::mutate(date = as.Date(date), value = as.numeric(value)) %>% arrange(date)
tlandunem <- landunem %>% dplyr::select(-1, -2) %>% dplyr::mutate(date = as.Date(date), value = as.numeric(value)) %>% arrange(date)
tlandpricei <- landpricei %>% dplyr::select(-1, -2) %>% dplyr::mutate(date = as.Date(date), value = as.numeric(value)) %>% arrange(date)

# 3. Manipulating og transformerer dataene
molttlandgdp <- tlandgdp %>% dplyr::mutate(gdp = ts(value,freq = 4)) %>%
    dplyr::mutate(lngdp = log(gdp)) %>%
    dplyr::mutate(Lgdp = lag(gdp,n=4)) %>%
    dplyr::mutate(Llngdp = lag(lngdp,n=4)) %>%
    dplyr::mutate(ggdp = round(gdp/Lgdp-1,digits=4)) %>%
    dplyr::mutate(glgdp = round(lngdp - Llngdp, digits = 4)) %>%
    dplyr::mutate(hpcycleg = hpfilter(lngdp, freq = 1600)$cycle) %>%
    dplyr::mutate(hptrendg = hpfilter(lngdp, freq = 1600)$trend) %>%
  reshape2::melt(id.vars = c("date")) %>%
  dplyr::mutate(kat=c('gdp'))

ggplot(data = dplyr::filter(molttlandgdp, variable %in% c('lngdp', 'hptrendg')), aes(x = date, y =  value)) + geom_line(aes(color = variable))
#qplot(data = dplyr::filter(molttlandgdp, variable %in% c('lngdp', 'hptrendg'), x=date, y=value, geom = 'point'))


nunem <- mean(tlandunem$value)
molttlandunem <- tlandunem %>%
    dplyr::mutate(unem = ts (value)) %>%
    dplyr::mutate(Lunem = lag(unem, n = 12)) %>%
    dplyr::mutate(cunem = round(unem - Lunem, digits = 4)) %>%
  #dplyr::mutate(hpcycleu = hpfilter(unem, freq = 6000000000000)$cycle) %>%
  #dplyr::mutate(hptrendu = hpfilter(unem, freq = 6000000000000)$trend) %>%
  dplyr::mutate(trendu = nunem) %>%
  reshape2::melt(id.vars = c("date")) %>%
  dplyr::mutate(kat = 'unem')

molttlandpricei <- tlandpricei %>%
  dplyr::mutate(pricei = ts(value)) %>%
  dplyr::mutate(Lpricei = lag(pricei, n = 12)) %>%
  dplyr::mutate(inflation = round(pricei - Lpricei,digits=4)) %>%
  dplyr::mutate(Linflation = lag(inflation, n = 12)) %>%
  dplyr::mutate(cinflation = round(inflation - Linflation,digits=4)) %>%
  reshape2::melt(id.vars = c("date")) %>%
  dplyr::mutate(kat = 'inf')

data1 <- dplyr::filter(molttlandpricei, variable %in% c('pricei'))
data2 <- dplyr::filter(molttlandpricei, variable %in% c('Lpricei'))
datas <- dplyr::filter(molttlandpricei, variable %in% c('pricei', 'Lpricei'))

## Samler alle dataene for land
moltmacrohellas <- rbind(molttlandunem, molttlandgdp, molttlandpricei) %>% dplyr::mutate(land='gre')

# 4. Saving data in Rda-format
devtools::use_data(moltmacrohellas, overwrite = TRUE)

# # Appendiks: grafikk
# Henter datasett
moltmacroland <- moltmacrohellas

landdataI <- dplyr::filter(moltmacroland, variable %in% c('unem', 'cunem', 'ggdp', 'inflation', 'cinflation', 'yinflation', 'cyinflation'))
flanddataI <- reshape2::dcast(landdataI, date + freqm ~ variable) %>%
  dplyr::filter(freqm=='01', date >= '1999-01-01 '& date < '2011-01-01')

okunusag <- ggplot(data = flanddataI, aes(x = cunem, y = ggdp))  + geom_point() +  geom_smooth(method=lm, se=FALSE) + theme_classic() +
  labs(title = 'Okuns lov', subtitle = 'Hellas', x = 'Endring i arbeidsledigheten', y = c('Vekst i BNP (realpriser)'))

philipusag <- ggplot(data = flanddataI, aes(x = unem, y = cinflation))  + geom_point() +
  geom_smooth(method=lm, se=FALSE) + theme_classic() +
  labs(title = 'Hellas', subtitle = 'Norge' ,x = 'Ledighetsrate', y = 'Endring i inflation')




