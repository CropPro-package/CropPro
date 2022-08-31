library(plyr)
dataorg<-function(dataframe, groupcol){
data<-dataframe[,(groupcol+1):ncol(dataframe)]
groups<-dataframe[,groupcol]
groups<-as.data.frame(groups)
colnames(groups)<-"category"

apply2<-apply(data,2,function(x){sqrt(x*100/sum(x))})
all<-cbind(groups,apply2)
sumgroups<-aggregate(.~`category`, data=all, FUN=sum)
labels<-toupper(sumgroups$`category`)
df<-as.data.frame(t(sumgroups[,-1]))
colnames(df)<-labels
resultdf<-data.frame #OF 7 columns and X row number of samples ( so number of columns data has)
resultdf$samples<-rownames(df)
resultdf$BHH<-df$BHH
resultdf$BFH<-df$BFH
resultdf$SHH<-df$SHH
resultdf$SHL<-df$SHL
resultdf$SFH<-df$SFH

dforder<-df[,colorder]
}
