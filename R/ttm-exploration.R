library(data.table)
library(ggplot2)
library(ggthemes)
library(GGally)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#csv files are in data folder

ttm <- fread('./data/500570-ttm.csv')
colnames(ttm) <- c('date', 'open', 'high', 'low', 'close', 'wap', 'num_shares', 'num_trades', 'turnover', 'qty', 'delivered_pct', 'spread_high_low', 'spread_close_open')
dvr <- fread('./data/570001-ttm-dvr.csv')
colnames(dvr) <- c('date', 'open', 'high', 'low', 'close', 'wap', 'num_shares', 'num_trades', 'turnover', 'qty', 'delivered_pct', 'spread_high_low', 'spread_close_open')

td <- merge(ttm, dvr, by.x='date', by.y='date', suffixes = c('_ttm', '_dvr'))
td$date <- as.POSIXct(strptime(td$date, '%d-%b-%Y'))
td$spread_ttm_dvr_close <- td$close_ttm - td$close_dvr
td$spread_ttm_dvr_ratio <- td$spread_ttm_dvr_close / td$close_ttm
td$close_ttm_ratio <- td$close_dvr/mean(td$close_ttm)

ggplot(td, aes(x=date)) +
  geom_line(aes(y = close_ttm)) + 
  geom_line(aes(y = close_dvr))

ggplot(td, aes(x=date)) +
  geom_line(aes(y = close_ttm)) + 
  geom_line(aes(y = spread_ttm_dvr_close))

ggplot(td, aes(x=date)) +
  geom_line(aes(y = close_ttm_ratio, color="close_ttm_ratio")) + 
  geom_line(aes(y = spread_ttm_dvr_ratio, color="spread_ttm_dvr_ratio"))


p1 <- ggplot(td, aes(x=date)) +
    geom_line(aes(y = close_ttm, color="close_ttm")) + 
    geom_line(aes(y = close_dvr, color="close_dvr")) +
    theme(legend.position="top")

p2 <- ggplot(td, aes(x=date)) +
    geom_line(aes(y = spread_ttm_dvr_ratio, color="spread_ttm_dvr_ratio")) +
    theme(legend.position="top")

multiplot(p1, p2)

ggplot(td, aes(x=spread_ttm_dvr_ratio)) +
  geom_histogram(binwidth=0.005)

cor(td$close_ttm, td$close_dvr)
cor(td$close_ttm, td$spread_ttm_dvr_close)

ggpairs(td[, c(5, 17, 26, 27)])
