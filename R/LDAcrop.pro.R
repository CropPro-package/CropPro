LDAcrop.pro<-function(x){
  data.model<-data.frame(data.model)
  discrim_cv <- lda(PROC ~ BHH+BFH+SHH+SHL+SFH+SFL,data.model, CV = TRUE, prior=c(0.25,0.25,0.25,0.25))
  model_lda <- lda(PROC ~ BHH+BFH+SHH+SHL+SFH+SFL,data.model, prior=c(0.25,0.25,0.25,0.25))
  predictionmodel <- predict(model_lda,data.model)
  functionalAt <- data.frame(PROC = as.factor(data.model$PROC),
                             Classification= predictionmodel$class,
                             predictionmodel$x)
  centroids <- functionalAt %>%
    group_by(PROC) %>%
    summarise(centroid1 = mean(LD1),
              centroid2= mean(LD2),
              centroid3=mean(LD3))

  model <- cbind(as.data.frame(predict(model_lda,x)),x)
  tbl <- table(model$class)
  res <- cbind(tbl,round(prop.table(tbl)*100,2))
  colnames(res) <- c('Count','Percentage')
  row.names(res)<-c("Winnowing by-products", "Coarse-sieving by-products", "Fine-sieving by-products", "Fine-sieving products")
  print("results and linear discriminant scores")
  print(model)
  print("classifications")
  print(res)
results<-model
}
