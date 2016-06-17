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

# tpc <- topics(x, K=4:16, verb=TRUE)
# saveRDS(tpc, file="WINtpc.rds",compress=FALSE)
tpc <- readRDS("tpc.rds")

tops <- function(tpc, deltas=NULL, plotit=TRUE){
	sumtp <- summary(tpc,6)

	oavg <- apply(tpc$omega,2,function(o) tapply(o,meta$year,mean))
	print(
		sort(odiff <- apply(oavg,2,function(o)mean(tail(o))-mean(head(o)))))
	yr <- 2003:2012


	if(is.null(deltas)) deltas <- which(abs(odiff)>0.01)
	dcol <- rainbow(length(deltas))
	dleg <- paste(deltas, ":", sep="")
	for(i in 1:length(deltas)){ 
    	dleg[i] <- paste(dleg[i], 
    		paste(sumtp$phrase[sumtp$topic==deltas[i]],collapse=" "))
    	cat(odiff[deltas[i]], "/", dleg[i],"\n")
    }

	if(plotit){
		pdf("tpc.pdf", width=6,height=6)
		plot(yr, oavg[,1], type="l",lwd=2,bty="n",
		 col=0,ylim=c(0,.12), ylab="average topic weight", xlab="publication year",
    	 main="LDA")
		par(xpd=TRUE)
		for(i in 1:length(deltas)) lines(yr, oavg[,deltas[i]], col=dcol[i], lwd=2)
		legend("bottomleft", bty="n",  legend=dleg, col=dcol,lwd=2)
		dev.off()
	}
	invisible()
}
tops( tpc, deltas=c(1,4,8))


print("topic 1 articles")
print(meta[order(-tpc$omega[,1])[1:5],"title"])
print("topic 4 articles")
print(meta[order(-tpc$omega[,4])[1:5],"title"])
print("topic 8 articles")
print(meta[order(-tpc$omega[,8])[1:5],"title"])
# [1] "The adaptive lasso and its oracle properties"                                         
# [2] "Regularization and variable selection via the elastic net"                            
# [3] "On the adaptive elastic-net with a diverging number of parameters"                    
# [4] "Variable selection with the strong heredity constraint and its oracle property"       
# [5] "Shrinkage tuning parameter selection with a diverging number of parameters"           

## Regression stuff
y <- meta$citCounts/(2013-meta$year)
d <- model.matrix(~factor(meta$year)-1)
w <- tpc$omega[,-1]*10 #intercept is mathstats
colnames(d) <- levels(factor(meta$year))
summary(citefit <- lm(y ~ d + w -1))
# w8     0.105125   0.010733   9.795  < 2e-16 ***

## STM
library(stm)
docs =  lapply(split(cbind(x$j,x$v), x$i), 
	function(jv) matrix(jv, nrow=2, byrow=TRUE))
vocab <- colnames(x)
docmet <- data.frame(pubdate=factor(meta$year), cites=y)
stmfit <- stm(docs, vocab, K=15, 
	prevalence =~ cites + pubdate, data=docmet, 
	max.em.its=75,  init.type="Spectral")
saveRDS(stmfit,file="stm.rds", compress=FALSE)
stmfit <- readRDS("stm.rds")


topsSTM <- function(stm, deltas=NULL, dcol=NULL, plotit=TRUE){
	require(stm)
	sumtp <- labelTopics(stmfit, n=5)

	oavg <- apply(stmfit$theta,2,function(o) tapply(o,meta$year,mean))
	odiff <- apply(oavg,2,function(o)mean(tail(o))-mean(head(o)))
	names(odiff) <- 1:length(odiff)
	print(sort(odiff))
	yr <- 2003:2012


	if(is.null(deltas)) deltas <- which(abs(odiff)>0.01)
	if(is.null(dcol)) dcol <- rainbow(length(deltas))
	dleg <- paste(deltas, ":", sep="")
	for(i in 1:length(deltas)){ 
    	dleg[i] <- paste(dleg[i], 
    		paste(sumtp$lift[deltas[i],],collapse=" "))
    	cat(odiff[deltas[i]], "/", dleg[i],"\n")
    }

	if(plotit){
		pdf("stm.pdf", width=6,height=6)
		plot(yr, oavg[,1], type="l",lwd=2,bty="n",
		 col=0,ylim=c(0,.12), ylab="average topic weight", xlab="publication year",
    	 main="STM")
		par(xpd=TRUE)
		for(i in 1:length(deltas)) lines(yr, oavg[,deltas[i]], col=dcol[i], lwd=2)
		legend("bottomleft", bty="n",  legend=dleg, col=dcol,lwd=2)
		dev.off()
	}
	invisible()
}
topsSTM(stmfit)#, deltas=c(2,5,14,15),dcol=c(4,"orange",3,2))

prep <- estimateEffect(1:15 ~ cites + pubdate, stmfit,
			meta = docmet, uncertainty = "Global")
plot.estimateEffect(prep, covariate="cites", method="continuous", topic=2)

citebeta <- sapply(prep$parameters[[2]], function(v) v$est["cites"])

mean(citebeta)
# [1] 0.04749852
sd(citebeta)
# [1] 0.004547176 
