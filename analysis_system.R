# STAT 517 PROJECT 2
# By:


# DATA PROCESSING 
setwd("C://Users//surfacepro//Desktop")
data <- read.delim("Panssdata.txt")
pdata <- data.frame(data[8:nrow(data),])
pdata1 <- data.frame(rbind(paste(pdata[1,], pdata[2,]),pdata)[-c(2,3),])

pdata2 <- data.frame(matrix(NA, nrow=70, ncol=32))
colnames(pdata2) <- unlist(strsplit(paste(pdata[1,], pdata[2,]), " "))[-22]

for (i in 1:nrow(pdata1)) {
  pdata2[i,] <- rbind(unlist(strsplit(as.character(pdata1[i,]), " ")), pdata2)
}

pdata3 <- pdata2[-1,]
pdata3[1,] <- unlist(strsplit(as.character(pdata1[2,]), " "))[-c(1,2)]
fdata <- sapply(pdata3[-2], as.numeric)[-1,]
expert <- sapply(pdata3[-2], as.numeric)[1,]

# DATA ANALYSIS 
pcnt <- rep(NA, 1, nrow(fdata))
ncnt <- rep(NA, 1, nrow(fdata))
gcnt <- rep(NA, 1, nrow(fdata))

for (i in 1:nrow(fdata)) {
  pcnt1 <- 0
  ncnt1 <- 0
  gcnt1 <- 0
  for (j in 2:8) {
    if (is.na(fdata[i,j])) {}
    else if (fdata[i,j] %in% seq(expert[j]-1,expert[j]+1)) {
      pcnt1 <- pcnt1 + 1  }
  }
  for (j in 9:15) {
    if (is.na(fdata[i,j])) {}
    else if (fdata[i,j] %in% seq(expert[j]-1,expert[j]+1)){
      ncnt1 <- ncnt1 + 1
    }
  }
  for (j in 16:31){
    if (is.na(fdata[i,j])) {}
    else if (fdata[i,j] %in% seq(expert[j]-1,expert[j]+1)) {
      gcnt1 <- gcnt1 + 1
    }
  }
  pcnt[i] <- pcnt1
  ncnt[i] <- ncnt1
  gcnt[i] <- gcnt1
}

result <- rep(NA, 1, nrow(fdata))

for (i in 1:nrow(fdata)) {
  if (pcnt[i] >= 5 & ncnt[i] >=5 & gcnt[i] >=10) {
        result[i] <- "PASS" 
  } else {
    result[i] <- "FAIL"
  }
}

# VISUALIZATION
LANG <- pdata3$LANG[-1]
fdata1 <- data.frame(fdata,LANG, result)
write.csv(fdata1, "fdata1.csv")
# i did them on Tableau. I can make an R version but I'll do that next time.


# LANGUAGE COMPARISON
# we tried hyp testing, but it didn't work cuz we didn't realize that it can only work for continuous variables
# AND SO, we decided to use a Fisher's Exact Test for Count Data to compare if there is a language effect.
tab <- table(result,LANG)
fisher.test(tab)
# pvalue = 0.06039
# if we use alpha = 0.05, then we say that language does not affect whether or not a doctor passes or fails
# BUT, this looks awfully close to it. bam.
