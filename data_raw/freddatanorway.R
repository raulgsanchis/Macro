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
macro.series1 <- fred$series.search("CLVMNACNSAB1GQNO")
macro.series2 <- fred$series.search("LMUNRLTTNOQ647S")
macro.series3 <- fred$series.search("NORCPIALLMINMEI")

# Makrotall nor
norgdp <- fred$series.observations(series_id = c('CLVMNACNSAB1GQNO'))
norunem <- rbind(fred$series.observations(series_id = c('LMUNRLTTNOQ647S')), fred$series.observations(series_id = c('UNRATE')))
norinf <- fred$series.observations(series_id = c('NORCPIALLMINMEI')[1])

# 2. Cleaning up data
tnorgdp  <- norgdp %>% dplyr::select(-1, -2) %>% dplyr::mutate(date = as.Date(date), value = as.numeric(value)) %>% arrange(date)
tnorunem <- norunem %>% dplyr::select(-1, -2) %>% dplyr::mutate(date = as.Date(date), value = as.numeric(value)) %>% arrange(date)
tnorinf <- norinf %>% dplyr::select(-1, -2) %>% dplyr::mutate(date = as.Date(date), value = as.numeric(value)) %>% arrange(date)

# 3. Manipulating og transformerer dataene
molttnorgdp <- tnorgdp %>% dplyr::mutate(gdp = ts(value,freq = 4)) %>%
  dplyr::mutate(lngdp = log(gdp)) %>%
  dplyr::mutate(Lgdp = lag(gdp,n=4)) %>%
  dplyr::mutate(Llngdp = lag(lngdp,n=4)) %>%
  dplyr::mutate(ggdp = round(gdp/Lgdp-1,digits=4)) %>%
  dplyr::mutate(glgdp = round(lngdp - Llngdp, digits = 4)) %>%
  dplyr::mutate(hpcycleg = hpfilter(lngdp, freq = 2000)$cycle) %>%
  dplyr::mutate(hptrendg = hpfilter(lngdp, freq = 2000)$trend) %>%
  reshape2::melt(id.vars = c("date")) %>%
  dplyr::mutate(kat=c('gdp'))

ggplot(data = dplyr::filter(molttnorgdp, variable %in% c('lngdp','hptrendg')), aes(x = date, y =  value)) + geom_line(aes(color = variable))

nunem <- mean(dplyr::filter(tnorunem, date >'1939-12-01' & date < '2007-12-01')$value)
molttnorunem <- tnorunem %>%
    dplyr::mutate(unem = ts (value)) %>%
    dplyr::mutate(Lunem = lag(unem, n = 12)) %>%
    dplyr::mutate(cunem = round(unem - Lunem, digits = 4)) %>%
  #dplyr::mutate(hpcycleu = hpfilter(unem, freq = 6000000000000)$cycle) %>%
  #dplyr::mutate(hptrendu = hpfilter(unem, freq = 6000000000000)$trend) %>%
  dplyr::mutate(trendu = nunem) %>%
  reshape2::melt(id.vars = c("date")) %>%
  dplyr::mutate(kat = 'unem')

#ggplot(data = dplyr::filter(molttnorunem, variable %in% c('unem','rendu')), aes(x = date, y =  value)) + geom_line(aes(color = variable))

molttnorinf <- tnorinf %>% reshape2::melt(id.vars = c("date")) %>%
  dplyr::mutate(inflation = ts(value)) %>%
  dplyr::mutate(Linflation=lag(inflation, n = 12)) %>%
  dplyr::mutate(cinflation = round(inflation - Linflation,digits=4)) %>%
  reshape2::melt(id.vars = c("date")) %>%
  dplyr::mutate(kat = 'inf')

## Samler alle dataene for nor
moltmacronor <- rbind(molttnorunem, molttnorgdp, molttnorinf)

  #dplyr::mutate(freqm = substring(date,6,7))

# 4. Saving data in Rda-format
devtools::use_data(moltmacronor, overwrite = TRUE)

# Appendiks: grafikk
