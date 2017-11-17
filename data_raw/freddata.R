#https://fred.stlouisfed.org/series/GDPCA
#https://fred.stlouisfed.org/series/M0892AUSM156SNBR
#


devtools::install_github("jcizel/FredR")
library(FredR)
library(pipeR)
library(ggplot2)
library(dplyr)

api.key = 'd62b9d8d4ce53e56ea04049dc463ac51'  # substitute ... with your API key
fred <- FredR(api.key)
str(fred,1)
macro.series <- fred$series.search("Money")

macro.series %>%
  select(
    id,
    title,
    observation_start,
    observation_end,
    popularity
  ) %>%
  arrange(
    desc(as.numeric(popularity))
  )


ma <- fred$series.observations(series_id = 'AMBSL')

dt <- ma %>%
  select(
    date,
    value
  ) %>%
  mutate(
    date = as.Date(date),
    value = as.numeric(value)) %>% arrange(date)

qplot(data = dt[50:150,], x = date, y = value, geom = 'line')

devtools::use_data(dt)

#[M2USA](http://www.sjsu.edu/faculty/watkins/depmon.htm)
