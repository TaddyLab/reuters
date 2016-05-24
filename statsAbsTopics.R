# statsAbsTopics.R

tkns <- read.table("statsAbsTkns.txt", 
            quote='', comment='', 
            col.names=c("doi","tkn","cnt"))
tkns <- head(tkns,-1)

library(Matrix)
x <- sparseMatrix(
        i=as.numeric(tkns$doi), 
        j=as.numeric(tkns$tkn), 
        x=tkns$cnt,
        dimnames=list(levels(tkns$doi),levels(tkns$tkn)))
x <- x[,colSums(x>0)>=20]

# match up rownames
meta <- read.csv("SCC2015-metadata/paperMeta.txt",as.is=TRUE,row.names=1)
zebra <- match(rownames(x),rownames(meta))
x <- x[!is.na(zebra),]
zebra <- zebra[!is.na(zebra)]
meta <- meta[zebra,c("year","title","citCounts")]

# remove empties
notemp <- rowSums(x)>0
x <- x[notemp,]
meta <- meta[notemp,]

## topic modeling
library(maptpx)
x <- as.simple_triplet_matrix(x)
tpc <- topics(x, K=4:20, verb=TRUE)
sumtp <- summary(tpc,10)

oavg <- apply(tpc$omega,2,function(o) tapply(o,meta$year,mean))
sort(apply(oavg,2,function(o)mean(tail(o))-mean(head(o))))
yr <- 2003:2012


deltas <- c(1,3,5)
dcol <- c("red","blue","green")
dleg <- paste(deltas, ":", sep="")
for(i in 1:length(deltas)) 
    dleg[i] <- paste(dleg[i], paste(sumtp$phrase[sumtp$topic==deltas[i]],collapse=" "))

plot(yr, oavg[,1], type="l",lwd=2,bty="n",col=0,ylim=c(0.04,.14), ylab="topic weight",
    main="change in average topic weights over time")
for(i in 1:length(deltas)) lines(yr, oavg[,deltas[i]], col=dcol[i], lwd=2)
legend("bottomleft", bty="n",  legend=dleg, col=dcol,lwd=2)

meta[order(-tpc$omega[,1])[1:5],"title"]
meta[order(-tpc$omega[,3])[1:5],"title"]
meta[order(-tpc$omega[,5])[1:5],"title"]


## MNIR stuff
y <- meta$citCounts/(2013-meta$year)
d <- model.matrix(~factor(meta$year)-1)
w <- tpc$omega[,-1]*10 #intercept is mathstats
colnames(d) <- levels(factor(meta$year))
summary(citefit <- lm(y ~ d + w -1))
# w3     0.108089   0.009906  10.911  < 2e-16 ***