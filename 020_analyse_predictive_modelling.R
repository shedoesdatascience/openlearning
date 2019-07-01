#****************************************************************************************
#
# PROJECT: 20181002
#
# MODULE: 020 - ANALYSE - PREDICTIVE MODELLING
#
# DESCRIPTION:

#
#              
#         
# STEPS
# 1.Set libraries
# 2. Set up data for modelling
# 3. Run GBM
#****************************************************************************************


##1. Set libraries ####
library(data.table)
library(dplyr)
library(caret)
library(ggplot2)



library(h2o) #no support for java 9 yet - get errors
localH2O <- h2o.init(nthreads = -1)
 h2o.init()


 ##2. Set up data for modelling ####
 


model_train.h2o <- as.h2o(train)
model_test.h2o <- as.h2o(test)

y_dv<- which(colnames(model_train.h2o)=="final_result")
x_iv_start<-y_dv-1
x_iv_end<-y_dv+1
x_iv<-c(1:x_iv_start,x_iv_end:length(train))


#3. Run gbm ####


gbm_model <-h2o.gbm(y=y_dv, x=x_iv, training_frame = model_train.h2o, validation_frame = model_test.h2o,
                    ntrees =500, max_depth = 4, distribution="multinomial", #for multi-classification
                    learn_rate = 0.01, seed = 1234, max_hit_ratio_k=3, nfolds = 5, keep_cross_validation_predictions = TRUE)


saveRDS(gbm_model, "./gbm_model.rds")



variable.importance.list<-as.data.frame(h2o.varimp(gbm_model))

summary(gbm_model)                   ## View information about the model.


# re-run gbm with re-defined dependent variable

y_dv<- which(colnames(model_train.h2o)=="success")

x_iv_end<-y_dv-1
x_iv<-c(1:x_iv_end)
gbm_model <-h2o.gbm(y=y_dv, x=x_iv, training_frame = model_train.h2o, validation_frame = model_test.h2o,
                    ntrees =500, max_depth = 4, distribution="bernoulli", #for binomial
                    learn_rate = 0.01, seed = 1234, max_hit_ratio_k=3, nfolds = 5, keep_cross_validation_predictions = TRUE)


saveRDS(gbm_model, "./gbm_model.rds")

# Optional: Average the holdout AUCs
cvAUCs <- sapply(sapply(gbm_model@model$cross_validation_models, `[[`, "name"), function(x) { h2o.auc(h2o.getModel(x), valid=TRUE) })

print(cvAUCs)
mean(cvAUCs)

variable.importance.list<-as.data.frame(h2o.varimp(gbm_model))



#Predict on test set
gbm.prediction = h2o.predict(gbm_model, newdata=model_test.h2o, type='response')
gbm.prediction_prob = h2o.predict(gbm_model, newdata=model_test.h2o)[,2]

predicted_values_model<-h2o.make_metrics(gbm.prediction_prob,model_test.h2o$success)

gbm.auc = h2o.auc(h2o.performance(gbm_model, newdata=model_test.h2o))


#Produce AUC curve

fpr <- h2o.fpr( h2o.performance(gbm_model, newdata=model_test.h2o) )[['fpr']]
tpr <- h2o.tpr( h2o.performance(gbm_model, newdata=model_test.h2o) )[['tpr']]
ggplot( data.table(fpr = fpr, tpr = tpr), aes(fpr, tpr) ) + 
  geom_line() + theme_bw() + ggtitle( sprintf('AUC: %f', gbm.auc) )



