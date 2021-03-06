#https://fred.stlouisfed.org/series/GDPCA
#https://fred.stlouisfed.org/series/M0892AUSM156SNBR

options(encoding="UTF-8")
library(httr)
# henter rjstat bibliotek for behandling av JSON-stat
library(rjstat)
# Adresse til et ferdig json-stat datasett for Detaljomsetningsindeksen
url <- "http://data.ssb.no/api/v0/dataset/1066.json?lang=no"
d.tmp<-GET(url)
# Henter ut innholdet fra d.tmp som tekst deretter bearbeides av fromJSONstat
sbtabell <- fromJSONstat(content(d.tmp, "text"))
# Henter ut kun datasettet fra sbtabell
ds <- sbtabell[[1]]
# Viser datasettet
ds
# Barplott av den kjedelige sorten
barplot(ds$value, names.arg = ds$`tid`)
