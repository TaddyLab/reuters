# 
library(distrom)

u <- read.table("data/u.txt", row.names=1)
o <- read.table("data/o.txt", row.names=1)
vocab <- cbind(u,o)

fit <- c()
for(part in 0:20) 
	fit <- c(fit, readRDS(sprintf("output/noindustry/fit-%05d.rds",part)))

zebra <- match(rownames(o),names(fit))
mod <- fit[zebra]
class(fit) <- "dmr"

names(fit)[!names(fit)%in%rownames(o)]
B <- coef(fit, )

getnz <- function(v) sort(B[v,B[v,]!=0],decreasing=TRUE)

b <- getnz("E12")
# "MARKETS" M1
# "Commodity Markets" M14
# "Metals Trading" M142 


# "Environment"

# "Defense Contracts" 