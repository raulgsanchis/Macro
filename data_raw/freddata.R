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

api.key = 'd62b9d8d4ce53e56ea04049dc463ac51'  # substitute ... with your API key
fred <- FredR(api.key)
str(fred,1)
macro.series1 <- fred$series.search("GDP")
macro.series2 <- fred$series.search("unemployment")
macro.series3 <- fred$series.search("money")
macro.series4 <- fred$series.search("inflation")

# Makrotall USA
usagdp <- fred$series.observations(series_id = c('GDPCA'))
usaunem <- rbind(fred$series.observations(series_id = c('M0892AUSM156SNBR')), fred$series.observations(series_id = c('UNRATE')))
usainf <- fred$series.observations(series_id = c('CPIAUCSL', 'CPILFESL')[1])
usamone <- fred$series.observations(series_id = c('M1'))

# 2. Cleaning up data
tusagdp  <- usagdp %>% dplyr::select(-1, -2) %>% dplyr::mutate(date = as.Date(date), value = as.numeric(value)) %>% arrange(date)
tusaunem <- usaunem %>% dplyr::select(-1, -2) %>% dplyr::mutate(date = as.Date(date), value = as.numeric(value)) %>% arrange(date)
tusainf <- usainf %>% dplyr::select(-1, -2) %>% dplyr::mutate(date = as.Date(date), value = as.numeric(value)) %>% arrange(date)
tusamone <- usamone %>% dplyr::select(-1, -2) %>% dplyr::mutate(date = as.Date(date), value = as.numeric(value)) %>% arrange(date)

# 3. Manipulating og transformerer dataene
molttusagdp <- tusagdp %>% dplyr::mutate(gdp = ts(value, start = c(1929,1), end = c(2016,1), freq = 1)) %>%
  dplyr::mutate(lngdp = log(gdp)) %>%
  dplyr::mutate(Lgdp = lag(gdp,n=1)) %>%
  dplyr::mutate(Llngdp = lag(lngdp,n=1)) %>%
  dplyr::mutate(ggdp = round(gdp/Lgdp-1,digits=4)) %>%
  dplyr::mutate(glgdp = round(lngdp - Llngdp, digits = 4)) %>%
  dplyr::mutate(hpcycleg = hpfilter(lngdp, freq = 2000)$cycle) %>%
  dplyr::mutate(hptrendg = hpfilter(lngdp, freq = 2000)$trend) %>%
  reshape2::melt(id.vars = c("date")) %>%
  dplyr::mutate(kat=c('gdp'))

#ggplot(data = dplyr::filter(molttusagdp, variable %in% c('lngdp','hptrendg')), aes(x = date, y =  value)) + geom_line(aes(color = variable))

nunem <- mean(dplyr::filter(tusaunem, date >'1939-12-01' & date < '2007-12-01')$value)
molttusaunem <- tusaunem %>%
    dplyr::mutate(unem = ts (value)) %>%
    dplyr::mutate(Lunem = lag(unem, n = 12)) %>%
    dplyr::mutate(cunem = round(unem - Lunem, digits = 4)) %>%
  #dplyr::mutate(hpcycleu = hpfilter(unem, freq = 6000000000000)$cycle) %>%
  #dplyr::mutate(hptrendu = hpfilter(unem, freq = 6000000000000)$trend) %>%
  dplyr::mutate(trendu = nunem) %>%
  reshape2::melt(id.vars = c("date")) %>%
  dplyr::mutate(kat = 'unem')

#ggplot(data = dplyr::filter(molttusaunem, variable %in% c('unem','rendu')), aes(x = date, y =  value)) + geom_line(aes(color = variable))

molttusainf <- tusainf %>% reshape2::melt(id.vars = c("date")) %>%
  dplyr::mutate(inflation = ts(value)) %>%
  dplyr::mutate(Linflation=lag(inflation, n = 12)) %>%
  dplyr::mutate(cinflation = round(inflation - Linflation,digits=4)) %>%
  reshape2::melt(id.vars = c("date")) %>%
  dplyr::mutate(kat = 'inf')

molttusamone <- tusamone %>% dplyr::mutate(money = ts(value)) %>%
  reshape2::melt(id.vars = c("date")) %>%
  dplyr::mutate(kat='mon')

## Samler alle dataene for USA
moltmacrousa <- rbind(molttusaunem, molttusagdp, molttusainf, molttusamone) %>%
  dplyr::mutate(freqm = substring(date,6,7))

# 4. Saving data in Rda-format
devtools::use_data(moltmacrousa, overwrite = TRUE)

# Appendiks: grafikk
unique(moltmacrousa$variable)
a <- dplyr::filter(moltmacrousa, variable %in% c('unem','cunem'))
b <- reshape2::dcast(a, date ~ variable)
c <- dplyr::filter(moltmacrousa, variable %in% c('cunem',  'ggdp', 'inflation', 'cinflation'))
d <- reshape2::dcast(c, date + freqm ~ variable) %>% dplyr::filter(freqm=='01', date >= '1948-01-01'
                                                                   & date < '2017-01-01')
qplot(data = d, x = ggdp, y = cunem, geom = 'point') + geom_smooth(model=lm)
qplot(data = d, x = inflation, y = cinflation, geom = 'point') + geom_smooth(model=lm)


