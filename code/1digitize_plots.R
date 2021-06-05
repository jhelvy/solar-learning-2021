# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

library(stats)  # for `approx`: interpolation
theme_set(theme_bw())

# # Install latest version of digitize
# devtools::install_github("east-winds/digitize")
library(digitize)

# Digitize data contained in figures, save to csv ----

## Fraunhofer ----
# "Recent facts about photovoltaics in Germany", 2020
# Fig 2 (installed rooftop systems)

# digitize
#   manually select top of bars
df <- digitize(file.path(
    dir$data, 'germany',
    'Fraunhofer-recent-facts-about-photovoltaics-in-germany-Fig2.png'))
df.bak = df

# round x to nearest yr
df$x <- round(df$x)
df <- plyr::rename(df, c('x'='year'))

# add observation types
df$type = rep(c(1,2),nrow(df)/2)
df$type <- factor(df$type, labels = c('module','bos_inverter'))

# subtract cumulative portion in type 2
df.wide <- reshape(df, direction='wide', idvar = c('year'), timevar = 'type')
df.wide$y.2 = df.wide$y.2 - df.wide$y.1
df <- reshape(df.wide)
df <- plyr::rename(df, c('y.1'='y'))

ggplot(df) +
    geom_line(aes(year, y, col = type))

write.csv(df, file.path(dir$data, 'germany', 'fraunhofer_fig2.csv'),
    row.names = F)

## Nemet silicon prices ----
# Nemet, G. F. (2019). How solar energy became cheap: A model for low-carbon innovation. Routledge.
# Figure 7.2

# auto-digitize
#   manual steps:
#       1) select bottom-left and top-right corner of graph
#       2) enter axis bounds of two axes
fn <- digitize(file.path(
    dir$data, 'silicon',
    'Nemet-silicon-prices.png'), twopoints = T, auto = T)

# plot
x <- seq(1970, 2017, 0.01)
ggplot() +
    geom_line(aes(x, fn(x)))

# create annual dataframe
df <- data.frame(yr = seq(1980, 2016, 1),
                 price = fn(seq(1980, 2016, 1)))
# find last datapoint, assign to 2017
for (x in seq(2016.01, 2017, 0.01)) {
    if (!is.na(fn(x))) { y <- fn(x)}
}
df[nrow(df)+1,] <- c(2017,y)

write.csv(df, file.path(dir$data, 'silicon', 'nemet_silicon.csv'),
          row.names = FALSE)

