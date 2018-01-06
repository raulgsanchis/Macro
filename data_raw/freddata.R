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
usainf <- fred$series.observations(series_id = c('CPIAUCSL', 'CPILFESL')[1])
usamone <- fred$series.observations(series_id = c('M1'))
usaunem <- rbind(fred$series.observations(series_id = c('M0892AUSM156SNBR')), fred$series.observations(series_id = c('UNRATE')))

# 2. Cleaning up data
tusagdp  <- usagdp %>% dplyr::select(-1, -2) %>% dplyr::mutate(date = as.Date(date), value = as.numeric(value)) %>% arrange(date)
tusaunem <- usaunem %>% dplyr::select(-1, -2) %>% dplyr::mutate(date = as.Date(date), value = as.numeric(value)) %>% arrange(date)
tusainf <- usainf %>% dplyr::select(-1, -2) %>% dplyr::mutate(date = as.Date(date), value = as.numeric(value)) %>% arrange(date)
tusamone <- usamone %>% dplyr::select(-1, -2) %>% dplyr::mutate(date = as.Date(date), value = as.numeric(value)) %>% arrange(date)

# 3. Manipulating og transformerer dataene
molttusagdp <- tusagdp %>% dplyr::mutate(gdp = ts(value, start = c(1929,1), end = c(2016,1), freq = 1)) %>%
  dplyr::mutate(lngdp = log(gdp)) %>%
  dplyr::mutate(hpcycleg = hpfilter(lngdp, freq = 100)$cycle) %>%
  dplyr::mutate(hptrendg = hpfilter(lngdp, freq = 100)$trend) %>%
  reshape2::melt(id.vars = c("date")) %>%
  dplyr::mutate(kat=c('gdp'))

molttusaunem <- tusaunem %>% dplyr::mutate(unem = ts (value)) %>%
  dplyr::mutate(hpcycleu = hpfilter(unem, freq = 14400)$cycle) %>%
  dplyr::mutate(hptrendu = hpfilter(unem, freq = 14400)$trend) %>%
  reshape2::melt(id.vars = c("date")) %>%
  dplyr::mutate(kat = 'unem')

molttusainf <- tusainf %>% reshape2::melt(id.vars = c("date")) %>%
  dplyr::mutate(kat = 'inf')

molttusamone <- tusamone %>% reshape2::melt(id.vars = c("date")) %>%
  dplyr::mutate(kat='mon')

## Samler alle dataene for USA
moltmacrousa <- rbind(molttusaunem, molttusagdp, molttusainf, molttusamone)

# 4. Saving data in Rda-format
devtools::use_data(moltmacrousa, overwrite = TRUE)

