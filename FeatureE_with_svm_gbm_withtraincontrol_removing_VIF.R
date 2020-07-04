
#hyperparemeter


library(caret)

rf_data = read.csv(file.choose()) # choose from Green ball pellet

#removing features having high VIF

vif_func<-function(in_frame,thresh=10,trace=T,...){
  
  library(fmsb)
  
  if(any(!'data.frame' %in% class(in_frame))) in_frame<-data.frame(in_frame)
  
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]), na.rm = TRUE)
  
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{
    
    in_dat<-in_frame
    
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      
      vif_vals<-NULL
      var_names <- names(in_dat)
      
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat, ...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2]), na.rm = TRUE))[1]
      
      vif_max<-as.numeric(vif_vals[max_row,2])
      
      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }
      
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
      
    }
    
    return(names(in_dat))
    
  }
  
}

#removing response variable

all_x = rf_data[,-45]

a = vif_func(in_frame=all_x,thresh=5,trace=T)

rf_df = data.frame(rf_data[,a] , RF = rf_data$Return.Fines..)

train_all <- sample(nrow(rf_df), 0.7*nrow(rf_df), replace = FALSE)
TrainSet_all <- rf_df[train_all,]
ValidSet_all <- rf_df[-train_all,]

library(randomForest)

model_f <- randomForest(RF ~ ., data = TrainSet_all, importance = TRUE , n_tree = 150, oob_score = TRUE, n_jobs = -1,random_state =50 , max_features = "auto", min_samples_leaf = 50)

predValid <- predict(model_f, ValidSet_all)

error = ValidSet_all$RF - predValid

mape = mean(abs(error/ValidSet_all$RF))

train_e = TrainSet_all$RF - model_f$predicted

mape_train = mean(abs(train_e/TrainSet_all$RF))

plot(ValidSet_all$RF , type="l" , main = "Randomforest Actual Vs Predicted")
lines(predValid , col=4)


library(caret)

#finding best fit from selected hyperparameters

trctrl <- trainControl(method = "cv", number = 4,verboseIter=T)
grid_radial <- expand.grid(sigma = c(0.01,0.001,.005,0.02,0.025,.05,.1,.2),
                           C = c( 0.5, 0.75,
                                  1,1.2,1.5,2,3,4))


set.seed(3233)
svm_Radial_Grid <- caret::train(RF~ ., data =TrainSet_all
                                , method = "svmRadial",
                                trControl=trctrl,
                                vebrose=T,
                                preProcess = c("center", "scale"),
                                tuneGrid = grid_radial,
                                tuneLength = 10)

plot(svm_Radial_Grid) 


metric <- "RMSE"
trainControl <- trainControl(method="cv", number=4,verboseIter=T)
gbmGrid <- expand.grid(.interaction.depth = c(2,3,4),.n.trees = c(100,200,300,400,500), .shrinkage = c(.02,.03,.04),.n.minobsinnode = c(5,10,15))  
set.seed(99)
gbm.caret3 <- caret::train(RF~ ., data =TrainSet_all  #select(train,-c("CAST_NO","BLOWER","SCRAP1"))
                           , distribution="gaussian"
                           , method="gbm"
                           , trControl=trainControl
                           , verbose=T
                           , tuneGrid=gbmGrid
                           , metric=metric
                           , bag.fraction=0.75
) 

p2 = predict(gbm.caret3 , ValidSet_all)

plot(gbm.caret3)

plot(ValidSet_all$RF , type = "l" , main="GBM Actual vs predicted")
lines(p2 , col = 2)

#best set of hyperparameter selection 

plot(gbm.caret3)



#plotting and all


p=predict(svm_Radial_Grid,ValidSet_all)

plot( ValidSet_all$RF, type = "l" , main = "SVM Actual vs Predicted " )
lines(p , col = rgb(0,1,0.5))

p1 = predict(model , ValidSet_all$RF)

plot(p,te_data$K_lag1)


library(Metrics)

RMSE(p,te_data$K_lag1)
RMSE(p1,te_data$K_lag1)


