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
macro.series <- fred$series.search("GDP")
macro.series2 <- fred$series.search("unemployment")

#Civilian Unemployment Rate
usagdp <- fred$series.observations(series_id = c('GDPCA')) %>% dplyr::select(date, value)
usaunem <- rbind(fred$series.observations(series_id = c('M0892AUSM156SNBR')), fred$series.observations(series_id = c('UNRATE')))

devtools::use_data(usagdp, overwrite = TRUE)
devtools::use_data(usaunem, overwrite = TRUE)

# dt <- ma %>%
#   select(
#     date,
#     value
#   ) %>%
#   mutate(
#     date = as.Date(date),
#     value = as.numeric(value)) %>% arrange(date)
#
# qplot(data = dt, x = date, y = value, geom = 'line')
