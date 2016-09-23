## reutersMNIR

args <- commandArgs(TRUE)
if(!exists("part")) part <- as.integer(args[1])

suppressMessages(library(distrom))

where <- Sys.getenv('SLURM_JOB_NAME')
who  <- Sys.info()['nodename']
what <- detectCores()
cat(sprintf("on %s with %d cores\n", who, what))

cat("start cluster\n")
cl <- makeCluster(what, type = "FORK",
  	outfile = sprintf("output/%s/zcpu-%05d.log", where, part))
print(cl)

fname <- sprintf("data/tokens/part-%05d", part)
if(!file.exists(fname)) stop("no counts file for this id")
cat(sprintf("%s part%05d IR fit\n", 
	Sys.getenv("SLURM_JOBID") , part))

industries <- read.table("data/raw/rcv1-v2.industries.qrels")
topics <- read.table("data/raw/rcv1-v2.topics.qrels")
regions <- read.table("data/raw/rcv1-v2.regions.qrels")

regions[,2] <- did <- factor(regions[,2])
industries[,2] <- factor(industries[,2],levels=levels(did))
topics[,2] <- factor(topics[,2],levels=levels(did))

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

x <- read.table(fname, sep="|")
x[,2] <- factor(x[,2], levels=levels(did))
x <- sparseMatrix(i=as.integer(x[,2]), j=as.integer(x[,1]),x=x[,3],
	dims=c(nlevels(did),nlevels(x[,1])),dimnames=list(NULL,levels(x[,1])))
x <- x[,colSums(x>0)>100]

m <- scan("data/m.txt", quiet=TRUE)
if(any(m==0)){
	mz <- which(m==0)
	x <- x[-mz,]
	I <- I[-mz,]
	R <- R[-mz,]
	T <- T[-mz,]
}
mu <- log(m)


fit <- dmr(cl = cl, 
    	covars = cBind(R,T), counts = x,  mu = mu, gamma=1,
        verb=2, maxit=1000, standardize = FALSE,  lmr=1e-4)

saveRDS(fit,  sprintf("output/%s/fit-%05d.rds",where,part), compress=FALSE) 
