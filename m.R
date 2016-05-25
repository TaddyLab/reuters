# calculate m values
library(Matrix)
regions <- read.table("Reuters/rcv1-v2.regions.qrels")
did <- factor(regions[,2])
m <- rep(0,nlevels(did))

for(part in 1:20){
	print(part)
	fname <- sprintf("reutersTokens/part-%05d", part)
	if(!file.exists(fname)) stop("no counts file for this id")

	x <- read.table(fname, sep="|")
	x[,2] <- factor(x[,2], levels=levels(did))
	x <- sparseMatrix(i=as.integer(x[,2]), 
			j=as.integer(x[,1]),x=x[,3],
			dims=c(nlevels(did),nlevels(x[,1])),
			dimnames=list(NULL,levels(x[,1])))
 	x <- x[,colSums(x>0)>100]

 	m <- m + rowSums(x)
}

write.table(m, "m.txt", row.names=FALSE, col.names=FALSE)
