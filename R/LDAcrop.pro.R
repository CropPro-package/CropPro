
LDAcrop.pro<-function(x){
  library(dplyr)
  library(MASS)
  data.model<-data.frame(CP.data.model)
  discrim_cv <- lda(PROC ~ BHH+BFH+SHH+SHL+SFH+SFL,CP.data.model, CV = TRUE)
  model_lda <- lda(PROC ~ BHH+BFH+SHH+SHL+SFH+SFL,CP.data.model)
  predictionmodel <- predict(model_lda,CP.data.model)
  functionalAt <- data.frame(PROC = as.factor(CP.data.model$PROC),
                             Classification= predictionmodel$class,
                             predictionmodel$x)
  centroids <- functionalAt %>%
    group_by(PROC) %>%
    summarise(centroid1 = mean(LD1),
              centroid2= mean(LD2),
              centroid3=mean(LD3))
  model <- cbind(as.data.frame(predict(model_lda,x)),x)

  print(model)

}
