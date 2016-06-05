setwd("C://Users/taddy/Documents/GitHub/NLP")
library(distrom)
fit <- c()
for(part in 0:20) 
	fit <- c(fit, readRDS(sprintf("reutersOutput/fit-%05d.rds",part)))

