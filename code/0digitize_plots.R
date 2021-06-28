# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

library(stats)  # for `approx`: interpolation
theme_set(theme_bw())

# load the latest version of digitize
# devtools::install_github("east-winds/digitize")
library(digitize)

# Digitize data contained in figures, save to csv ----

## Fraunhofer ----
# "Recent facts about photovoltaics in Germany", 2020
# Fig 2 (installed rooftop systems)

# digitize
#   manually select top of bars
df <- digitize(file.path(dir$data, 'germany',
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
df.wide$y.bos_inverter = df.wide$y.bos_inverter - df.wide$y.module
df <- reshape(df.wide)
df <- plyr::rename(df, c('y.module'='y'))

ggplot(df) + geom_line(aes(year, y, col=type))

write.csv(df, file.path(dir$data, 'germany', 'fraunhofer_fig2.csv'),
    row.names = F)
