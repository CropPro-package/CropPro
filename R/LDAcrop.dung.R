LDAcrop.dung<-function(x){
data.model<-data.frame(data.model,stringsAsFactors = FALSE)
data.model$PROC<-as.numeric(data.model$PROC)
x$PROC<-5
x$NO<-"arch"
labels<-x[c(1)]
x<-x[c(9,8,2:7)]
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
names(model)<-c("Sample","Class", "Prob.1", "Prob.2","Prob.3", "Prob.4", "Prob.5", "LD1","LD2", "LD3","LD4")


tbl <- table(model$Class)
res <- cbind(tbl,round(prop.table(tbl)*100,2))
colnames(res) <- c('Count','Percentage')
row.names(res)<-c( "Winnowing by-products", "Coarse-sieving by-products", "Fine-sieving by-products", "Fine-sieving products","Archaeological")
models50 <- cbind(labels,as.data.frame(predict(model_lda50,x)))
models50<-models50%>% mutate(across(where(is.numeric), round, digits =3))
names(models50)<-c("Sample","CLASS_std", "Prob.1_std", "Prob.2_std", "Prob.3_std", "Prob.4_std", "Prob.5_std", "Ld1_std", "Ld2_std", "Ld3_std", "Ld4_std")

tbl50 <- table(models50$CLASS_std)
res50 <- cbind(tbl50,round(prop.table(tbl50)*100,2))
colnames(res50) <- c('Count','Percentage')
row.names(res50)<-c( "Winnowing by-products", "Coarse-sieving by-products", "Fine-sieving by-products", "Fine-sieving products","Archaeological")
models <- cbind(as.data.frame((predict(model_lda,x))), as.data.frame(predict(model_lda50,x)))
names(models)<-c("Class", "Prob.1", "Prob.2","Prob.3", "Prob.4", "Prob.5", "LD1","LD2", "LD3","LD4","CLASS_std", "Prob.1_std", "Prob.2_std", "Prob.3_std", "Prob.4_std", "Prob.5_std", "Ld1_std", "Ld2_std", "Ld3_std", "Ld4_std")
models<-models%>% mutate(across(where(is.numeric), round, digits =3))

print("results and linear discriminant scores - unstandardised")
print(model)
print("classifications- unstandardised")
print(res)
print("results and linear discriminant scores - standardised")
print(models50)
print("classifications- standardised")
print(res50)
modelname<-cbind(labels,models,x)
names(modelname)<-c("Samples","Class", "Prob.1", "Prob.2","Prob.3", "Prob.4", "Prob.5", "LD1","LD2", "LD3","LD4","CLASS_std", "Prob.1_std", "Prob.2_std", "Prob.3_std", "Prob.4_std", "Prob.5_std", "Ld1_std", "Ld2_std", "Ld3_std", "Ld4_std", "NO", "PROC", "BHH", "BFH", "SHH", "SHL","SFH", "SFL")
results<-modelname
}
