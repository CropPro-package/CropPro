LDAcrop.pro<-function(x){
  PROC<-LD1<-LD2<-LD3<-NULL
  data.model<-data.frame(data.model)
  discrim_cv50 <- lda(PROC ~ BHH+BFH+SHH+SHL+SFH+SFL,data.model, CV = TRUE, prior=c(0.25,0.25,0.25,0.25))
  discrim_cv <- lda(PROC ~ BHH+BFH+SHH+SHL+SFH+SFL,data.model, CV = TRUE)
  model_lda <- lda(PROC ~ BHH+BFH+SHH+SHL+SFH+SFL,data.model)
  model_lda50 <- lda(PROC ~ BHH+BFH+SHH+SHL+SFH+SFL,data.model, prior=c(0.25,0.25,0.25,0.25))
  predictionmodel <- predict(model_lda,data.model)
  predictionmodel50 <- predict(model_lda50,data.model)
  functionalAt <- data.frame(PROC = as.factor(data.model$PROC),
                             Classification= predictionmodel$class,
                             predictionmodel$x)
  functionalAt50 <- data.frame(PROC = as.factor(data.model$PROC),
                             Classification= predictionmodel50$class,
                             predictionmodel50$x)

  centroids <- functionalAt %>%
    group_by(PROC) %>%
    summarise(centroid1 = mean(LD1),
              centroid2= mean(LD2),
              centroid3=mean(LD3))

  centroids50 <- functionalAt50 %>%
    group_by(PROC) %>%
    summarise(centroid1 = mean(LD1),
              centroid2= mean(LD2),
              centroid3=mean(LD3))

  model <- cbind(as.data.frame(predict(model_lda,x)),x)
  model<-model%>% mutate(across(where(is.numeric), round, digits =3))
  model50 <- cbind(as.data.frame(predict(model_lda50,x)),x)
  model50<-model50%>% mutate(across(where(is.numeric), round, digits =3))
  models <- cbind(as.data.frame(predict(model_lda50,x)),as.data.frame(predict(model_lda,x)))
  names(models)<-c("Class_std*", "Prob.1_std*", "Prob.2_std*", "Prob.3_std*", "Prob.4_std*", "Ld1_std", "Ld2_std", "Ld3_std","Class", "Prob.1", "Prob.2","Prob.3", "Prob.4", "LD1*","LD2*", "LD3*")
  modelround<-models%>% mutate(across(where(is.numeric), round, digits =3))
  selectedC<-cbind(model[9],modelround[c(1:5,14:16)])
  tbl <- table(model$class)
  res <- cbind(tbl,round(prop.table(tbl)*100,2))
  colnames(res) <- c('Count','Percentage')
  row.names(res)<-c("Winnowing by-product", "Coarse sieve by-product", "Fine sieve by-product", "Fine sieve product")
  tbl50 <- table(model50$class)
  res50 <- cbind(tbl50,round(prop.table(tbl50)*100,2))
  colnames(res50) <- c('Count','Percentage')
  row.names(res50)<-c("Winnowing by-product", "Coarse sieve by-product", "Fine sieve by-product", "Fine sieving product")

  print("Classification results and linear discriminant scores ")
  print(selectedC)
  print("classification table")
  print(res50)
  modelname<-cbind(x[1],modelround)
  names(modelname)<-c("Samples","Class_std*", "Prob.1_std*", "Prob.2_std*", "Prob.3_std*", "Prob.4_std*", "Ld1_std", "Ld2_std", "Ld3_std","Class", "Prob.1", "Prob.2","Prob.3", "Prob.4", "LD1*","LD2*", "LD3*")
  results<-modelname
}
