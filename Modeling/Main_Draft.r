# Databricks notebook source
install.packages('glmnetUtils')

# COMMAND ----------

# DBTITLE 1,Libraries
library(caret)
library(glmnet)
library(glmnetUtils)
library(dplyr)
library(data.table)
library(glue)

# COMMAND ----------

# DBTITLE 1,Options
options(width=160)

# COMMAND ----------

# DBTITLE 1,Parameters
maps_src<-'/dbfs/FileStore/tables/camendol/'
demo_marker_map<-'/dbfs/FileStore/tables/camendol/MAP_DEMO_MARKERS'
demos<-glue("gunzip -c /dbfs/mnt/jvmlshare/users/camendol/NHI_xfer/SUB_ETG/demos_1.csv.gz")

numfolds<-5
overall<-NULL
coefdffull<-NULL
set.seed(55)

# COMMAND ----------

# DBTITLE 1,Trim Points Data
trim_points<-fread("/dbfs/mnt/jvmlshare/users/camendol/NHI_xfer/ETG_SEV/comoutliers12.csv")

# COMMAND ----------

# DBTITLE 1,Code Descriptions
etgdesc<-fread(glue("{maps_src}BASEETG_DESC.csv"))
colnames(etgdesc)[colnames(etgdesc)=='ETG\nBase\nClass ']<-"code"
etgdesc$code<-as.character(etgdesc$code)

conddesc<-fread(glue("{maps_src}MAP_COND_STAT.csv"))
colnames(conddesc)[colnames(conddesc)=='Condition\nStatus Code']<-"code"
colnames(conddesc)[colnames(conddesc)=='Description']<-"cd_desc"
conddesc$code<-as.character(conddesc$code)

cmordesc<-fread(glue("{maps_src}MAP_COMORB.csv"))
colnames(cmordesc)[colnames(cmordesc)=='Description']<-"cd_desc"
cmordesc$code<-as.character(cmordesc$code)

all_cd_desc<-rbind(conddesc[,.(code,cd_desc)],cmordesc[,.(code,cd_desc)])

# COMMAND ----------

# DBTITLE 1,Start of by ETG Loop
etg<-'669007'

# COMMAND ----------

etg_desc<-unlist(etgdesc[code==etg
                   ,.(`Short Description`)])
etg_desc
# FIX the etg base class variable

# COMMAND ----------

# DBTITLE 1,Raw Data Load
raw_data<-fread(glue("gunzip -c /dbfs/mnt/jvmlshare/users/camendol/NHI_xfer/ETG_SEV/sev_mark_{etg}_1.csv.gz"))
print(nrow(raw_data[]))

# COMMAND ----------

# DBTITLE 1,Demos from Raw Data
#Read the raw data a second time as demofile
demo<-raw_data[,.(PS_INDV_ID,AGE,SEX)]

# Variable names due to inconsistency of SAS export
# any future issues with other variables are probably due to the same thing 
names(demo)[names(demo) == 'ps_indv_id'] <- 'PS_INDV_ID'
names(demo)[names(demo) == 'AGE'] <- 'age'
names(demo)[names(demo) == 'sex'] <- 'SEX'

print(nrow(demo[]))
#print(ergstats)

# COMMAND ----------

# DBTITLE 1,Prepare Demo Markers from Mapping of Raw Data
print(demo_marker_map)
conn<-file(demo_marker_map,open="r")
linn<-readLines(conn,warn=FALSE)

demolist<-c()

for (i in 1:length(linn)){

  values<-strsplit(linn[i], split = ",")
  #print(values)
  set<-values[[1]][[1]]
  #print(set)
  age_l<-as.integer(values[[1]][[3]])
  age_h<-as.integer(values[[1]][[4]])
  sex_v<-values[[1]][[2]]
 
  #Build List of Demos
  demolist<-c(demolist,set)

  # Conditional Update
  #demo_markers<-demo[ (  (age>=age_l)
  #                      &(age<=age_h)
  #                      &(SEX==sex_v) )
  #                    ,c('marker'):=set][,c('PS_INDV_ID','marker')]

  demo_markers<-demo[ (  (age>=age_l)
                        &(age<=age_h)
                        &(SEX==sex_v) )
                      ,c(set):=1]                    

}
close(conn)
demo_markers[is.na(demo_markers)]<-0

# COMMAND ----------

#Merge Demos to ETG markers
model_data<-raw_data[demo_markers,on=c('PS_INDV_ID')]
vars<-c(grep("^D_.*", names(model_data), value=TRUE),grep("^F_.*", names(model_data), value=TRUE))

# COMMAND ----------

# DBTITLE 1,Apply High and Low Trim Points
model_trim_raw<-model_data[ trim_points[,c("baseEtg", "TI", "ETG120_Final_Trim", "high_trim_final")]
                           ,on=c(etgbase_i="baseEtg",ti="TI")]
 
#INCLUDE EPISODES HITTING THE LOW THRESHOLD EXACTLY
model_trim<-model_trim_raw[  TOT_AMT_STD>ETG120_Final_Trim 
                           & TOT_AMT_STD < high_trim_final,]

# COMMAND ----------

sums<-colSums(model_trim[,vars,with=FALSE])
excludelow <- names(sums)[sums < 5]
excludelow <- excludelow[!(excludelow %in% demolist)]
counts <- stack(sums)
names(counts) <- c("marker_count", "marker")

# COMMAND ----------

excludelow

# COMMAND ----------

flds<-createFolds( c(1:nrow(model_trim))
                  ,k=numfolds
                  ,list=TRUE
                  ,returnTrain=FALSE)

f1<-as.formula( paste("TOT_AMT_STD ~ 0 +", paste(vars[!(vars %in% c(excludelow))], collapse="+")))

x1<-sparse.model.matrix( f1
                        ,model_trim)
y<-as.matrix( model_trim$TOT_AMT_STD
            ,ncol=1)

#Lasso L1
mod<-cv.glmnet( x1
               ,y
               ,type.measure="mse"
               ,family="gaussian"
               ,alpha = 1)  

preds<-predict( mod
               ,s=mod$lambda.min
               ,new = x1)

acts<-y

reportRsq<-1-sum((preds - acts)^2)/sum((acts - mean(acts))^2)
mrkcnt<-sum(coef(mod, s=mod$lambda.min) != 0)    

coefdf<-stack(coef(mod,s=mod$lambda.min)[,1])
names(coefdf)<-c("lasso","marker")
coefdf$ETGBase<-etg
coefdf$ETGDesc<-etg_desc
coefdf$episode_count<-nrow(model_trim)
coefdf$code<-substr(coefdf$marker,10,14)
coefdf<-left_join(coefdf, all_cd_desc) #by code
coefdf<-left_join(coefdf, counts)
coefdf<-coefdf[,c("ETGBase", "ETGDesc", "episode_count","marker","code","cd_desc","marker_count", "lasso")]

# COMMAND ----------

#Write predicted and actuals table
predsacts<-model_trim[,c( "PS_INDV_ID"
                         ,"EPI_BEG"
                         ,"EPI_END"
                         ,"TOT_AMT_STD")]
predsacts$preds<-preds

fwrite( predsacts
       ,paste( "predsacts_"
              ,etg
              ,".csv"
              ,sep = ""))

# COMMAND ----------

# DBTITLE 1,Homegrown Cross Validation
mod <- NULL
results <- NULL
restemp <- NULL
preds <- NULL
acts <- NULL
for(i in 1:numfolds){
    mod <- glmnet(x1[-flds[[i]],],y[-flds[[i]],], alpha=1, lambda=0)
    preds <- predict(mod, x1[flds[[i]],])
    acts <- y[flds[[i]],]
    effRsq <- 1-sum((preds - acts)^2)/sum((acts - mean(acts))^2)
    RMSE <- sqrt(mean((preds-acts)^2))
    MAE <- mean(abs(preds-acts))
    restemp <- data.frame(t(c(effRsq, RMSE, MAE)))
    names(restemp) <- c("effRsq", "RMSE", "MAE")
    results <- rbind(results, restemp)
    mod <- NULL
}

# COMMAND ----------

minEffRsq <- min(results$effRsq)
maxEffRsq <- max(results$effRsq)
MMAE <- mean(results$MAE)
RMMSE <- sqrt(mean((results$RMSE)^2))
restemp2 <- data.frame(t(c(etg, etg_desc, "lasso", reportRsq, minEffRsq, maxEffRsq, RMMSE, MMAE, mrkcnt,  nrow(model_trim))))
names(restemp2) <- c("etg", "etgdesc", "model", "reportRsq", "minEffRsq", "maxEffRsq", "RMMSE", "MMAE", "markercount", "etgsize")
overall <- rbind(overall, restemp2)
  
  
coefdffull <- rbind(coefdffull, coefdf)
  
#write.csv(overall, 'etgdxtest-stats-8-23-22.csv')
    
#write.csv(coefdffull, 'etgdxtest-coefficients-8-23-22.csv')

# COMMAND ----------

print(RMMSE)
print(RMSE)
print(effRsq)
print(minEffRsq)
print(maxEffRsq)

# COMMAND ----------

overall

# COMMAND ----------

coefdffull

# COMMAND ----------


