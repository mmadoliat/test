data <- read.csv('data-raw/rclust-data.csv')
rdata <- data[1:10,]
pdis <- read.csv('data-raw/pdist.csv')
row.names(pdis) <- pdis[,1]; pdis <- pdis[,-1]
ptim <- read.csv('data-raw/ptime.csv')
row.names(ptim) <- ptim$X; ptim <- ptim[,-1]
save(rdata,pdis,ptim, file="data/rclust.rda")
