# 
library(distrom)
fit <- c()
for(part in 0:20) 
	fit <- c(fit, readRDS(sprintf("output/noindustry/fit-%05d.rds",part)))

class(fit) <- "dmr"
B <- coef(fit)

getnz <- function(v) sort(B[v,B[v,]!=0],decreasing=TRUE)

b <- getnz("E12")
# "MARKETS" M1
# "Commodity Markets" M14
# "Metals Trading" M142 

# "Environment"

# "Defense Contracts" 