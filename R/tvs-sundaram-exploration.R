library(data.table)
library(ggplot2)
library(ggthemes)
library(GGally)
library(psych)

source('./R/utils.R')

#csv files are in data folder

tvs <- fread('./data/532343-tvs.csv')
colnames(tvs) <- c('date', 'open', 'high', 'low', 'close', 'wap', 'num_shares', 'num_trades', 'turnover', 'qty', 'delivered_pct', 'spread_high_low', 'spread_close_open')

sun <- fread('./data/520056-suncla.csv')
colnames(sun) <- c('date', 'open', 'high', 'low', 'close', 'wap', 'num_shares', 'num_trades', 'turnover', 'qty', 'delivered_pct', 'spread_high_low', 'spread_close_open')

td <- merge(tvs, sun, by.x='date', by.y='date', suffixes = c('_tvs', '_sun'))
td$date <- as.POSIXct(strptime(td$date, '%d-%b-%Y'))
sd <- as.POSIXct(as.Date("2005-01-01"))
td <- td[date > sd]
qplot(td$date, td$close_tvs)

td$spread_sun_tvs_close <- td$close_sun - td$close_tvs
td$spread_sun_tvs_ratio <- td$spread_sun_tvs_close / td$close_tvs
td$close_tvs_ratio <- td$close_sun/mean(td$close_tvs)

ggplot(td, aes(x=date)) +
  geom_line(aes(y = close_tvs)) + 
  geom_line(aes(y = close_sun))

ggplot(td, aes(x=date)) +
  geom_line(aes(y = close_tvs)) + 
  geom_line(aes(y = spread_sun_tvs_close))

ggplot(td, aes(x=date)) +
  geom_line(aes(y = close_tvs_ratio, color="close_tvs_ratio")) + 
  geom_line(aes(y = spread_sun_tvs_ratio, color="spread_sun_tvs_ratio"))


p1 <- ggplot(td, aes(x=date)) +
  geom_line(aes(y = close_tvs, color="close_tvs")) + 
  theme(legend.position="top")

p2 <- ggplot(td, aes(x=date)) +
  geom_line(aes(y = close_sun, color="close_sun")) +
  theme(legend.position="top")

p3 <- ggplot(td, aes(x=date)) +
  geom_line(aes(y = spread_sun_tvs_ratio, color="spread_sun_tvs_ratio")) +
  theme(legend.position="top")

multiplot(p1, p2, p3)

ggplot(td[close_sun > 2000], aes(x=spread_sun_tvs_ratio)) +
  geom_histogram(binwidth=0.1)

ggplot(td[close_sun > 2000], aes(x=close_sun, y=spread_sun_tvs_ratio)) +
  geom_point() +
  geom_smooth(method="lm")

cor(td$close_tvs, td$close_sun)
cor(td$close_tvs, td$spread_sun_tvs_ratio)

ggpairs(td[, c(5, 17, 26, 27)])

ggpairs(td[, c('num_shares_tvs', 'turnover_tvs', 'close_tvs'), with=F])
