dt <- read.csv(file = "./data/massReg-processed.csv", header = T, sep = ",")
ht <- dt[,5:45]
ulbody <- as.matrix(ht[,c(1,3,5,7,9,11,39,41)])
urbody <- as.matrix(ht[,c(2,4,6,8,10,12,39,41)])
llbody <- as.matrix(ht[,c(13,15,17,19,21,23,25,27,29,31,33,35,37,40,41)])
lrbody <- as.matrix(ht[,c(14,16,18,20,22,24,26,28,30,32,34,36,38,40,41)])

lista <- list(ulbody, urbody, llbody, lrbody)


