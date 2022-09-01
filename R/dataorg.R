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

samples<-rownames(df)
BHH<-df$BHH
BFH<-df$BFH
SHH<-df$SHH
SHL<-df$SHL
SFH<-df$SFH
SFL<-df$SFL
if(is.null(BHH)== TRUE){BHH<-sample(0,length(samples),replace = T)}
if(is.null(BFH)== TRUE){BFH<-sample(0,length(samples),replace = T)}
if(is.null(SHH)== TRUE){SHH<-sample(0,length(samples),replace = T)}
if(is.null(SHL)== TRUE){SHL<-sample(0,length(samples),replace = T)}
if(is.null(SFH)== TRUE){SFH<-sample(0,length(samples),replace = T)}
if(is.null(SFL)== TRUE){SFL<-sample(0,length(samples),replace = T)}
results<-data.frame(BHH, BFH, SHH, SHL, SFH, SFL)

}
