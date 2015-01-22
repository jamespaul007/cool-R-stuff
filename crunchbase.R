library(reshape)
library(plyr)
library(ggplot2)
data <- read.csv('crunchbase_monthly_export_201403_investments.csv', sep=';', stringsAsFactors=F)
inv <- data[,c("investor_name", "company_name", "company_category_code", "raised_amount_usd", "investor_category_code")]
inv$raised_amount_usd[is.na(inv$raised_amount_usd)] <- 1

inv <- inv[inv$investor_category_code %in% c("finance", ""),]
top <- ddply(inv, .(investor_name), summarize, sum(raised_amount_usd))
names(top) <- c("investor_name", "usd")
top <- top[order(top$usd, decreasing=T),][1:100,]
invtop <- inv[inv$investor_name %in% top$investor_name[1:100],]


inv.mat <- cast(invtop[,1:4], investor_name~company_category_code, sum)
inv.names <- inv.mat$investor_name
inv.mat <- inv.mat[,3:40] # drop the name column and the V1 column (unknown market)

inv.seg <- ddply(invtop, .(company_category_code), summarize, sum(raised_amount_usd))
names(inv.seg) <- c("Market", "USD")
inv.seg <- inv.seg[inv.seg$Market != "",]
inv.seg$Market <- as.factor(inv.seg$Market)
inv.seg$Market <- reorder(inv.seg$Market, inv.seg$USD)
ggplot(inv.seg, aes(Market, USD/1000000))+geom_bar(stat="identity")+coord_flip()+ylab("$1M USD")

inv.market <- log(t(inv.mat))
inv.market[inv.market == -Inf] <- 0

fit <- kmeans(inv.market, 7, nstart=50)
pca <- prcomp(inv.market)
pca <- as.matrix(pca$x)
plot(pca[,2], pca[,1], type="n", xlab="Principal Component 1", ylab="Principal Component 2", main="Market Segments")
text(pca[,2], pca[,1], labels = names(inv.mat), cex=.7, col=fit$cluster)

inv.log <- log(inv.mat)
inv.log[inv.log == -Inf] <- 0
inv.rel <- scale(inv.mat)

fit <- kmeans(inv.log, 6, nstart=15)
pca <- prcomp(inv.log)
pca <- as.matrix(pca$x)
plot(pca[,2], pca[,1], type="n", xlab="Principal Component 1", ylab="Principal Component 2", main="VC firms")
text(pca[,2], pca[,1], labels = inv.names, cex=.7, col=fit$cluster)

inv.rel <- scale(inv.mat)

fit <- kmeans(inv.rel, 6, nstart=15)
pca <- prcomp(inv.rel)
pca <- as.matrix(pca$x)
plot(pca[,2], pca[,1], type="n", xlab="Principal Component 1", ylab="Principal Component 2", main="VC firms")
text(pca[,2], pca[,1], labels = inv.names, cex=.7, col=fit$cluster)


