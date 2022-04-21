library(plyr)
dataorg<-function(dataframe, groupcol){
data<-dataframe[,(groupcol+1):ncol(dataframe)]
groups<-dataframe[,groupcol]
colnames(groups)<-"category"
apply2<-apply(data,2,function(x){sqrt(x*100/sum(x))})
all<-cbind(groups,apply2)
sumgroups<-aggregate(.~`category`, data=all, FUN=sum)
labels<-toupper(sumgroups$`category`)
df<-as.data.frame(t(sumgroups[,-1]))
colnames(df)<-labels
colorder<-c("BHH", "BFH","SHH","SHL", "SFH", "SFL")
dforder<-df[,colorder]
}
