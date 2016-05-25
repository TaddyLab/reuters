## reutersMNIR
industries <- read.table("Reuters/rcv1-v2.industries.qrels")
topics <- read.table("Reuters/rcv1-v2.topics.qrels")
regions <- read.table("Reuters/rcv1-v2.regions.qrels")

regions[,2] <- did <- factor(regions[,2])
industries[,2] <- factor(industries[,2],levels=levels(did))
topics[,2] <- factor(topics[,2],levels=levels(did))

library(Matrix)
I <- sparseMatrix(i=as.integer(industries[,2]),
		j=as.integer(industries[,1]),
		x=industries[,3],
		dims=c(nlevels(did),nlevels(industries[,1])),
		dimnames=list(NULL,levels(industries[,1])))
T <- sparseMatrix(i=as.integer(topics[,2]),
		j=as.integer(topics[,1]),
		x=topics[,3],
		dims=c(nlevels(did),nlevels(topics[,1])),
		dimnames=list(NULL,levels(topics[,1])))
R <- sparseMatrix(i=as.integer(regions[,2]),
		j=as.integer(regions[,1]),
		x=regions[,3],
		dims=c(nlevels(did),nlevels(regions[,1])),
		dimnames=list(NULL,levels(regions[,1])))

args <- commandArgs(TRUE)
if(!exists("part")) 
  part <- as.integer(args[1])
fname <- sprintf("reutersTokens/part-%05d", part)
if(!file.exists(fname)) stop("no counts file for this id")

x <- read.table(fname, sep="|")
x[,2] <- factor(x[,2], levels=levels(did))
x <- sparseMatrix(i=as.integer(x[,2]), j=as.integer(x[,1]),x=x[,3],
	dims=c(nlevels(did),nlevels(x[,1])),dimnames=list(NULL,levels(x[,1])))

## NODE setup
where       <- Sys.getenv("OUT") 
cat(sprintf("%s part%03d IR fit\n", where, part))

library(parallel)
who  <- Sys.info()['nodename']
what <- detectCores()
cat(sprintf("on %s with %d cores\n", who, what))
cat("start cluster\n")
cl <- makeCluster(what, type = "FORK",
  outfile = sprintf("%s-log/zcpu%03d.log", where, part))
print(cl)

fit <- dmr(cl = cl, verb=0,
    	covars = cBind(R,T,I), counts = x,  mu = mu, 
        nzcheck = TRUE, standardize = FALSE, lmr=1e-4)
