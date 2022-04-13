
library(plyr)
library(data.table)
rowlab<-Test[,1:6]
Ncol<-ncol(Test)
test2<-Test[,7:ncol(test)]
Nrow<-nrow(all)
total<-numcolwise(sum)(test2)

all<-test2

# Due to labels at found start at 7
TEST<-Test[,6:ncol(Test)]


#This works without the labels

dataorg<-function(dataframe, labels){
data<-dataframe[,(labels+1):ncol(dataframe)]
removed<-dataframe[,1:labels]
apply2<-apply(data,2, function(x){sqrt(x*100/sum(x))})
all<-cbind(removed,apply2)
DT <- as.data.table(all)
# which columns are numeric
numeric_cols <- which(sapply(DT, is.numeric))
Y<-DT[, lapply(.SD, sum), by = `2`, .SDcols = numeric_cols]
  #new columns labels BHH, BFH, SHH, SHL, SFH, SFL
n<-Y$`2`
df<-as.data.frame(t(Y[,-1]))
colnames(df)<-n
colorder<-c("BHH", "BFH","SHH","SHL", "SFH", "SFL")
dforder<-df[,colorder]
}
summaryGroup <- ddply(big.data, c("Site", "FSphase", "English"), function(x) c(d15N=mean(x$adjustedN15), sd=sd(x$adjustedN15),  d13C=mean(x$adjustedC13), sd=sd(x$adjustedC13)))
