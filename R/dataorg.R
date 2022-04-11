
library(plyr)

rowlab<-Test[,1:6]
Ncol<-ncol(Test)
test2<-Test[,7:ncol(test)]
Nrow<-nrow(all)
total<-numcolwise(sum)(test2)

all<-test2

Due to labels at fround start at 7


This works without the labels

dataorg<-function(dataframe, labels){

  REMOVE LABELS
  apply2<-apply(dataframe,2, function(x){sqrt(x*100/sum(x))})
  Add labels back
  Sum by group
  new columns labels BHH, BFH, SHH, SHL, SFH, SFL
}

dataorg<-function(dataframe, labels){
  for(i in labels:ncol(all)){
    apply<-apply(all,2, function(x){sqrt(x*100/sum(x))})
  }


new<-dataorg(dataframe=Test)
