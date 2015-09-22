# main file for Gonçalves et al. Skeletal Mass Regression Project
#
# Keypoints to address in this project:
#
# Regression model complexity, assumptions, peer-acceptance
# Missing value handling
# Bilateral values handling
# Colinearity, colinearity everywhere...

# read data.frame
df <- df <- read.csv('./data/massReg-processed.csv')

# split into different data.frame structures to ease up data analysis
bio <- df[, c("SEX","AGE")] # biological profile data, SEX and AGE
total.mass <- df[, "TOTAL.MASS"] # target variable, total skeletal mass
longbones.l <- df[, c("HMR.L","FMR.L")] # left femur and humerus
clavicles <- df[, 5:6] 
metacarpals <- df[, 7:16]
patellas <- df[, 17:18]
tarsals <- df[, 19:32]
metatarsals <- df[, 33:42]
# Variable names
var.names.lr <- c(colnames(clavicles),colnames(metacarpals),colnames(patellas),
                  colnames(tarsals),colnames(metatarsals))
var.names.idx <- grep(pattern = ".L",var.names.lr,fixed = TRUE)
var.names <- gsub(pattern = ".L", replacement = "", 
                  x = var.names.lr[var.names.idx],fixed = TRUE)

## Handling bilateral values
#  Auxiliar function
bilateral.asymmetry <- function (m, sig = 0.01) {
  # Test for bilateral asymmetry using relative directional asymmetry and 
  # Wilcoxon signed ranks test.
  #
  # m: a n-by-2 matrix containing antimeres of a bilateral bone
  # note: matrix is ordered from left to right
  
  idx <- complete.cases(m)
  m.aux <- m[idx,]
  wcx.paired <- wilcox.test(x = m.aux[,1],y = m.aux[,2], exact = F, paired=T)
  if(wcx.paired$p.value <= sig){
    asymm <- TRUE 
  }else{
    asymm <- FALSE
  }
  da <- ((m.aux[,2]-m.aux[,1])/apply(m.aux, 1, mean))*100
  da <- median(da)
  
  res <- list(DA = da, statistic= as.numeric(wcx.paired$statistic), 
              p.value = wcx.paired$p.value, n = NROW(m.aux), asymm = asymm)
  return(res)
}

# Testing for bilateral asymmetry
result <- vector()
result <- rbind(result, unlist(bilateral.asymmetry(clavicles)))
for(i in seq(1,NCOL(metacarpals),2)){
  m <- cbind(metacarpals[,i], metacarpals[,i+1])
  result <- rbind(result, unlist(bilateral.asymmetry(m)))
}
result <- rbind(result, unlist(bilateral.asymmetry(patellas)))
for(i in seq(1,NCOL(tarsals),2)){
  m <- cbind(tarsals[,i],tarsals[,i+1])
  result <- rbind(result, unlist(bilateral.asymmetry(m)))
}
for(i in seq(1,NCOL(metatarsals),2)){
  m <- cbind(metatarsals[,i],metatarsals[,i+1])
  result <- rbind(result, unlist(bilateral.asymmetry(m)))
}
rownames(result) <- var.names
# write bilateral asymmetry results to csv file
write.csv(result,"./results/asymmetry.csv")

# PREDICTIVE ANALYSIS
# LINEAR REGRESSION - LOO VALIDATION
# UPPER  AND LOWER LIMB
upper.lower.limb <- data.frame(HMR.L = longbones.l[, 1], clavicles, metacarpals,
                               FMR.L = longbones.l[, 2], patellas, tarsals, 
                               metatarsals)
N <- NCOL(upper.lower.limb)
n <- NROW(upper.lower.limb)
ulimb.results <- matrix(NA, n, N)
regrCoefficients <- matrix(NA,N,2)
for(i in seq(1, N, 1)){
  for(j in seq(1, n, 1)){
    y <- total.mass[-j]
    x <- upper.lower.limb[-j, i]
    lr.model <- lm(y ~ x)
    ulimb.results[j,i] <- predict(lr.model, 
                                  newdata = data.frame(x=upper.lower.limb[j,i]))
  }
  y <- total.mass
  x <- upper.lower.limb[, i]
  lrModel <- lm(y ~ x)
  regrCoefficients[i, ] <- as.numeric(coefficients(lrModel))
}

rmse <- vector()
rsq <- vector()
for(i in seq(1, N, 1)){
  tmp.var <- cbind(total.mass, ulimb.results[,i])
  tmp.var <- tmp.var[complete.cases(tmp.var), ]
  rmse[i] <- sqrt(mean((tmp.var[,1]-tmp.var[,2])^2))
  rsq[i] <- cor(tmp.var)[1,2]^2
}

ul.metrics <- data.frame(RMSE = rmse, RSQ = rsq)
ul.regrCoefficients <- data.frame(Intercept = regrCoefficients[, 1], 
                              X = regrCoefficients[, 2])
rownames(ul.metrics) <- colnames(upper.lower.limb)
rownames(ul.regrCoefficients) <- colnames(upper.lower.limb)
# write.results
write.csv(round(ul.metrics, 3),'./results/accuracy.csv')
write.csv(round(ul.regrCoefficients, 3),'./results/coefficients.csv')

  
# # PREDICTIVE ANALYSIS
# # DENSITY ESTIMATION ALGORITHM - LOO VALIDATION
# # UPPER  AND LOWER LIMB
# require(GenKern)
# source(file = './bayesian-regressor.R')
# 
# N <- NCOL(upper.lower.limb)
# n <- NROW(upper.lower.limb)
# ulimb.results.pden <- matrix(NA, n, N)
# for(i in seq(1, N, 1)){
#   for(j in seq(1, n, 1)){
#     y <- total.mass[-j]
#     x <- upper.lower.limb[-j, i]
#     pden.model <- pdenModel(x, y, gridsize = 512)
#     ulimb.results.pden[j,i] <- predict.pdenModel(pden.model, 
#                                             newdata = upper.lower.limb[j, i],
#                                             show.plot = FALSE,
#                                             point.estimate.only = TRUE)
#     print(c(ulimb.results.pden[j,i], ulimb.results[j,i]))
#   }
# }
# 
# rmse.pden <- vector()
# rsq.pden <- vector()
# for(i in seq(1, N, 1)){
#   tmp.var <- cbind(total.mass, ulimb.results.pden[,i])
#   tmp.var <- tmp.var[complete.cases(tmp.var), ]
#   rmse.pden[i] <- sqrt(mean((tmp.var[,1]-tmp.var[,2])^2))
#   rsq.pden[i] <- cor(tmp.var)[1,2]^2
# }
# ul.metrics.pden <- data.frame(RMSE = rmse.pden, RSQ = rsq.pden)
# rownames(ul.metrics.pden) <- colnames(upper.lower.limb)

