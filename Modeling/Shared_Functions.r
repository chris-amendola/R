# Databricks notebook source
download_link<-function(  filespec=''
                        ,url_stem=''
                        ,o_value='DOWNLOAD'
                        ,html=FALSE){

  # Needs to be replaced with dbutils calls to get these values - no permmsions at the moment                        
  stem<-'https://adb-5036857654770991.11.azuredatabricks.net';     
  o_parm<-'5036857654770991'
      
  final_url<-glue('{stem}/{filespec}')
  link<-(glue('<a href="{final_url}">{o_value}</a>'))

  print(glue('{o_value} FILE: '))
  print(final_url)
  
  if(html){
    displayHTML(link)}

}

# COMMAND ----------

elastic_eval<-function( deps
                       ,resp

                       ,response_var

                       ,alph=0
                       ,n_folds=5
                       ,seed_value=2
                       ,out_dir='/dbfs/FileStore/tables/users/camendol/ERGmodeling/models/'){

  library(glmnet)

  ct<-nrow(deps)

  print(glue('ERG MODEL EVALUATION:\n MODEL: {response_var}\nALPHA: {alph}\n CV FOLDS: {n_folds}\n SEED VALUE: {seed_value} '))
  print(glue('OBS: {ct}'))   

  model_desc<-glue('{response_var}_{alph}_{fin_input}_{enroll_months}')
  print(glue('{model_desc}')) 

  flds <- createFolds( c(1:ct)
                    ,k = n_folds
                    ,list=TRUE
                    ,returnTrain = FALSE)

  mod <- cv.glmnet( deps
                   ,resp
                   ,type.measure="mse"
                   ,family="gaussian"
                   ,n_folds=n_folds
                   ,alpha=alph)

  print('\nMODEL: ')
  print(mod)
  print('')     
 
  coefs <- coef( mod
                ,s=mod$lambda.min)
  
  full_coefs<-setDT(data.frame( coeff=coefs@Dimnames[[1]][coefs@i+1]
                               ,value=coefs@x))
             
  col_ns<-setDT( data.frame( coeff=dimnames(deps)[[-1]]
                             ,freq=colSums(deps)))

  coeff_report<-coeff_set[ col_ns[ full_coefs
                                          ,on=.(coeff)]
                                  ,on=.(coeff)]
  
  names(coeff_report)<-c('Coeffcient','n', model_desc)
  
  fwrite(coeff_report,file=glue('{out_dir}full_coeff_{model_desc}.csv'))
  download_link(glue( '{out_dir}full_coeff_{model_desc}.csv')
                     ,o_value='COEFF_FULL_MODEL')
     
  preds <- predict(mod, s=mod$lambda.min, new = deps)
  acts <- resp
  
  full_model_RSQ <- 1-(sum((preds - acts)^2)/sum((acts - mean(acts))^2))
  full_model_RMSE <- sqrt(mean((preds - acts)^2))
  full_model_MAE <- mean(abs(preds-acts))
  train_mean <- mean(acts)
  train_sd <- sd(acts)

  ## Cross Validation ##
  print('Cross-Validation...')
  rsqlist <- NULL
  rmselist <- NULL
  maelist <- NULL

  for(k in 1:n_folds){

      deps_test<-deps[-flds[[k]],]
      deps_train<-deps[flds[[k]],]

      resp_test<-resp[-flds[[k]],]
      resp_train<-resp[flds[[k]],] 

      # Model Run with current fold left out
      mod <- cv.glmnet( deps_train
                       ,resp_train
                       ,type.measure="mse"
                       ,family="gaussian"
                       ,nfolds=5
                       ,alpha=alph)

      #Current fold predicted
      preds <- predict( mod
                       ,s=mod$lambda.min
                      ,new=deps_test)
      #Current Fold Actuals 
      acts<-resp_test

      coefs <- coef(mod, s = mod$lambda.min)                
      testRsq <- 1-sum((preds - acts)^2)/sum((acts - mean(acts))^2)
      RMSE <- sqrt(mean((preds - acts)^2))
      MAE <- mean(abs(preds-acts))

      rsqlist[glue('fold_{k}_rsq')]<-testRsq
      rmselist[glue('fold_{k}_rmse')]<-RMSE
      maelist[glue('fold_{k}_mae')]<-MAE
      
  }
   
   meaneffRsq<-mean(rsqlist)
   RMMSE<-sqrt(mean(rmselist^2))
   MMAE<-mean(maelist)
   
  model_val_sum<-list( 'model_description'=model_desc
                      ,'alpha'=alph
                      ,'full_model_RSQ'=full_model_RSQ
                      ,'full_model_RMSE'=full_model_RMSE
                      ,'full_model_MAE'=full_model_MAE
                      ,'meaneffRsq'=meaneffRsq
                      ,'RMMSE'=RMMSE
                      ,'MMAE'=MMAE
                      ,'train_mean'=train_mean
                      ,'train_sd'=train_sd)
  
  all_sum<-c(model_val_sum,rsqlist,rmselist,maelist)

  write.csv(all_sum,glue('{out_dir}full_model_{model_desc}.csv'))                    
  download_link(glue( '{out_dir}full_model_{model_desc}.csv')
                      ,o_value='MODEL_SUM')

  return(list(coeff=coeff_report,model_eval=all_sum))                    
}

# COMMAND ----------

{}
