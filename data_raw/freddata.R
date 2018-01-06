# 1. Download data
#https://fred.stlouisfed.org/series/GDPCA
#https://fred.stlouisfed.org/series/M0892AUSM156SNBR

devtools::install_github("jcizel/FredR")
library(FredR)
library(pipeR)
library(ggplot2)
library(dplyr)
library(mFilter)

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
tusaunem <- usaunem %>% dplyr::mutate(date = as.Date(date), value = as.numeric(value)) %>% arrange(date)
tusainf <- usainf %>% dplyr::mutate(date = as.Date(date), value = as.numeric(value)) %>% arrange(date)
tusamone <- usamone %>% dplyr::mutate(date = as.Date(date), value = as.numeric(value)) %>% arrange(date)

# 3. Manipulating the data
tusagdp <- tusagdp %>% dplyr::mutate(gdp = ts(value, start = c(1929,1), end = c(2016,1), freq = 1)) %>%
  dplyr::mutate(lngdp = log(gdp)) %>%
  dplyr::mutate(hpcycle = hpfilter(lngdp, freq = 100)$cycle) %>%
  dplyr::mutate(hptrend = hpfilter(lngdp, freq = 100)$trend) %>%

tusaunem <- tusaunem %>% dplyr::mutate(unem = ts (value)) %>%
  dplyr::mutate(hptrend = hpfilter(unem, freq = 14400)$cycle) %>%
  dplyr::mutate(hptrend = hpfilter(unem, freq = 14400)$trend)

# 5. Saving data in Rda-format
devtools::use_data(tusagdp, overwrite = TRUE)
devtools::use_data(tusaunem, overwrite = TRUE)
devtools::use_data(tusainf, overwrite = TRUE)
devtools::use_data(tusamone, overwrite = TRUE)

# A. Testing
names(iris)
qplot(data = mpg, cty, hwy, geom = 'point')

