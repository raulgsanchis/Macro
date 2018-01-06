load("~/gitclones/teaching/MakroOEKB1115/data/moltmacrousa.rda")
# A. Testing
# names(mpg)
# qplot(data = mpg, cty, hwy, geom = 'point')

# melttusagdp <-
#
#
#
#
# ## Time-series
# qplot(data = tusagdp, date, lngdp, geom = c('point', 'smooth'), method = 'lm')
# ## Melted

# qplot
qplot(data = subset(moltmacrousa, kat == 'gdp', variable =='value'), date, value, geom = 'point', color =  variable)
# ggplot2
ggplot(data = subset(moltmacrousa, kat == 'gdp'), aes(x = date, y =  value)) + geom_point(aes(color = variable))


ggplot(data = subset(moltmacrousa, select = c('lngdp'), kat == 'gdp'), aes(x = date, y =  value)) + geom_point(aes(color = variable))




# ## Time-series


# ggplot(data = tusagdp, aes(x = date, y = lngdp)) + geom_point() + geom_smooth( method = rlm, color = 'red',
#                                                                                size = 0.5, se = FALSE)
# ##
# ggplot(data = melttusagdp, aes(x = date, y =  value)) + geom_point(aes(color = variable))
#
#
# View(melttusagdp)
#
#
#
# ## Facets
data("economics")
emp <- reshape2::melt(economics, id = 'date', measure = c('unemploy', 'uempmed'))
#
#
qplot(date, value, data = emp, geom = 'line') + facet_grid(variable ~ ., scales = 'free_y')
#
# t <- ggplot(mpg, aes(cty, hwy)) + geom_point()
# a <- t + facet_grid(. ~ fl)
# b <- t + facet_grid( ~ fl)
