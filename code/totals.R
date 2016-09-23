# calculate m values
library(Matrix)
regions <- read.table("data/raw/rcv1-v2.regions.qrels")
did <- factor(regions[,2])
m <- rep(0,nlevels(did))
u <- c()
o <- c()

for(part in 0:20){
	print(part)
	fname <- sprintf("data/tokens/part-%05d", part)
	if(!file.exists(fname)) stop("no counts file for this id")

	x <- read.table(fname, sep="|")
	x[,2] <- factor(x[,2], levels=levels(did))
	x <- sparseMatrix(i=as.integer(x[,2]), 
			j=as.integer(x[,1]),x=x[,3],
			dims=c(nlevels(did),nlevels(x[,1])),
			dimnames=list(NULL,levels(x[,1])))
 	x <- x[,colSums(x>0)>100]

 	m <- m + rowSums(x)
 	u <- c(u, colSums(x))
	o <- c(o, colSums(x>0))
}

v <- cbind(u,o)
write.table(m, "data/m.txt", row.names=FALSE, col.names=FALSE)
write.table(v, "data/vocab.csv", row.names=TRUE, col.names=FALSE, sep=",")
