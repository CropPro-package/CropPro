LDAcrop.dung<-function(x){
data.model<-data.frame(data.model)
x$PROC<-6
x$NO<-"arch"
x<-x[c(8,7,1:6)]
model.arch<-rbind(data.model, x)
discrim_cv <- lda(PROC ~ BHH+BFH+SHH+SHL+SFH+SFL, model.arch, CV = TRUE,prior=c(0.2,0.2,0.2,0.2,0.2) )
model_lda50 <- lda(PROC ~ BHH+BFH+SHH+SHL+SFH+SFL, model.arch, prior=c(0.2,0.2,0.2,0.2,0.2))
model_lda <- lda(PROC ~ BHH+BFH+SHH+SHL+SFH+SFL, model.arch)
predictionmodel <- predict(model_lda, model.arch)
functionalAt <- data.frame(PROC = as.factor(model.arch$PROC),
                           Classification= predictionmodel$class,
                           predictionmodel$x)
centroids <- functionalAt %>%
  group_by(PROC) %>%
  summarise(centroid1 = mean(LD1),
            centroid2= mean(LD2),
            centroid3= mean(LD3),
            centroid4 = mean(LD4))
models <- cbind(as.data.frame(predict(model_lda,x)),x)
tbl <- table(models$class)
res <- cbind(tbl,round(prop.table(tbl)*100,2))
colnames(res) <- c('Count','Percentage')
row.names(res)<-c( "Winnowing by-products", "Coarse-sieving by-products", "Fine-sieving by-products", "Fine-sieving products","Archaeological")
print("results and linear discriminant scores")
print(models)
print("classifications")
print(res)
models<-models
}
