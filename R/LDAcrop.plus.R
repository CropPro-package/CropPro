LDAcrop.plus<-function(x){
data.model<-data.frame(data.model,stringsAsFactors = FALSE)
data.model$PROC<-as.numeric(data.model$PROC)
x$PROC<-5
labels<-x[c(1)]
x<-x[c("PROC","BHH","BFH", "SHH", "SHL", "SFH", "SFL")]
model.arch<-data.frame(stringsAsFactors = FALSE)
model.arch<-rbind(data.model, x, stringsAsFactors = FALSE)
discrim_cv <- lda(PROC ~ BHH+BFH+SHH+SHL+SFH+SFL, model.arch, CV = TRUE)
discrim_cv50 <- lda(PROC ~ BHH+BFH+SHH+SHL+SFH+SFL, model.arch, CV = TRUE,prior=c(1,1,1,1,1)/5)
model_lda50 <- lda(PROC ~ BHH+BFH+SHH+SHL+SFH+SFL, model.arch, prior=c(1,1,1,1,1)/5)
model_lda <- lda(PROC ~ BHH+BFH+SHH+SHL+SFH+SFL, model.arch)
predictionmodel <- predict(model_lda, model.arch)
predictionmodel50 <- predict(model_lda50, model.arch)
functionalAt <- data.frame(PROC = as.factor(model.arch$PROC),
                           Classification= predictionmodel$class,
                           predictionmodel$x)
functionalAt50 <- data.frame(PROC = as.factor(model.arch$PROC),
                           Classification= predictionmodel50$class,
                           predictionmodel50$x)
centroids <- functionalAt %>%
  group_by(PROC) %>%
  summarise(centroid1 = mean(LD1),
            centroid2= mean(LD2),
            centroid3= mean(LD3),
            centroid4 = mean(LD4))
centroids50 <- functionalAt50 %>%
  group_by(PROC) %>%
  summarise(centroid1 = mean(LD1),
            centroid2= mean(LD2),
            centroid3= mean(LD3),
            centroid4 = mean(LD4))

model <- cbind(labels,as.data.frame(predict(model_lda,x)))
model<-model%>% mutate(across(where(is.numeric), round, digits =3))
names(model)<-c("Sample","Class", "Prob.1", "Prob.2","Prob.3", "Prob.4", "Prob.5", "LD1*","LD2*", "LD3*","LD4*")
models50 <- cbind(labels,as.data.frame(predict(model_lda50,x)))
models50<-models50%>% mutate(across(where(is.numeric), round, digits =3))
names(models50)<-c("Sample","CLASS_std*", "Prob.1_std*", "Prob.2_std*", "Prob.3_std*", "Prob.4_std*", "Prob.5_std*", "Ld1_std", "Ld2_std", "Ld3_std", "Ld4_std")
tbl50 <- table(models50$CLASS_std)
res50 <- cbind(tbl50,round(prop.table(tbl50)*100,2))
colnames(res50) <- c('Count','Percentage')
row.names(res50)<-c( "Winnowing by-product", "Coarse sieveg by-product", "Fine sieve by-product", "Fine sieve product","Archaeological")
models <- cbind( as.data.frame(predict(model_lda50,x)),as.data.frame((predict(model_lda,x))))
names(models)<-c("CLASS_std", "Prob.1_std", "Prob.2_std", "Prob.3_std", "Prob.4_std", "Prob.5_std", "Ld1_std", "Ld2_std", "Ld3_std", "Ld4_std","Class", "Prob.1", "Prob.2","Prob.3", "Prob.4", "Prob.5", "LD1","LD2", "LD3","LD4")
models<-models%>% mutate(across(where(is.numeric), round, digits =3))
modelselect<-cbind(models50[1:7], model[8:11])
print("Classification results and linear discriminant scores")
print(modelselect)
print("classification table")
print(res50)
modelname<-cbind(labels,models,x)
names(modelname)<-c("Samples","CLASS_std*", "Prob.1_std*", "Prob.2_std*", "Prob.3_std*", "Prob.4_std*", "Prob.5_std*", "Ld1_std", "Ld2_std", "Ld3_std", "Ld4_std","Class", "Prob.1", "Prob.2","Prob.3", "Prob.4", "Prob.5", "LD1*","LD2*", "LD3*","LD4*", "PROC", "BHH", "BFH", "SHH", "SHL", "SFH","SFL")
results<-modelname
}
