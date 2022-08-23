######################################################
########### R codes attached to article ##############
###### Comparison of Cox regression and  RSF for######
########## predicting the survival of ################
########Laryngeal squamous cell carcinoma ############
######################################################
######################################################
setwd('')
install.packages('readxl')
install.packages('table1')
install.packages('survivalROC')
install.packages('survminer')
install.packages('rms')
install.packages('magicfor')
install.packages('randomForestSRC')
install.packages('pec')
install.packages('mlr')
install.packages('partykit')

library(readxl)
library(table1)
library(survival)
library(survivalROC)
library(survminer)
library(rms)
library(CoxBoost)
library(magicfor)
library(randomForestSRC)
library(pec)
library(mlr)
library(partykit)
library(party)
library(ggRandomForests)

data<-read.table('data_open.xlsx',sep = '\t',header=T)

#########Variables encoding##################
#Sex Female=1 Male=2
table(data$Sex)
data$Sex=gsub('Female','1',data$Sex)
data$Sex=gsub('Male','2',data$Sex)
#Laterality
#Not a paired  site=1，One side=2，Two side=3
table(data$Laterality)
data$Laterality=gsub('Not a paired  site','1',data$Laterality)
data$Laterality=gsub('One side','2',data$Laterality)
data$Laterality=gsub('Two side','3',data$Laterality)
#Age.group
#<60 yr old=1，≥60 yr old=2
table(data$Age.group)
data$Age.group=gsub('<60 yr old','1',data$Age.group)
data$Age.group=gsub('≥60 yr old','2',data$Age.group)
#Race
#White=1，Black=2，Other=3
table(data$Race)
data$Race=gsub('White','1',data$Race)
data$Race=gsub('Black','2',data$Race)
data$Race=gsub('Other','3',data$Race)
#Year.of.diagnosis
#>2010=1，≤2010=2
table(data$Year.of.diagnosis)
data$Year.of.diagnosis=gsub('>2010','1',data$Year.of.diagnosis)
data$Year.of.diagnosis=gsub('≤2010','2',data$Year.of.diagnosis)
#Primary.site
#Glottis=1，Supraglottis=2，Subglottis=3，Other=4
table(data$Primary.site)
data$Primary.site=gsub('Glottis','1',data$Primary.site)
data$Primary.site=gsub('Supraglottis','2',data$Primary.site)
data$Primary.site=gsub('Subglottis','3',data$Primary.site)
data$Primary.site=gsub('Other','4',data$Primary.site)
#Grade
#Well=1，Moderately=2，Poorly=3，Undifferentiated=4
table(data$Grade)
data$Grade=gsub('Well','1',data$Grade)
data$Grade=gsub('Moderately','2',data$Grade)
data$Grade=gsub('Poorly','3',data$Grade)
data$Grade=gsub('Undifferentiated','4',data$Grade)
#Extent.of.Disease
#Localized=1，Regional=2，Distant=3
table(data$Extent.of.Disease)
data$Extent.of.Disease=gsub('Localized','1',data$Extent.of.Disease)
data$Extent.of.Disease=gsub('Regional','2',data$Extent.of.Disease)
data$Extent.of.Disease=gsub('Distant','3',data$Extent.of.Disease)
#AJCC.Stage
#I=1，II=2，III=3，IV=4
table(data$AJCC.Stage)
data$AJCC.Stage=gsub('I','1',data$AJCC.Stage)
data$AJCC.Stage=gsub('II','2',data$AJCC.Stage)
data$AJCC.Stage=gsub('III','3',data$AJCC.Stage)
data$AJCC.Stage=gsub('IV','4',data$AJCC.Stage)
#T
#T1=1，T2=2，T3=3，T4=4
table(data$T)
data$T=gsub('T1','1',data$T)
data$T=gsub('T2','2',data$T)
data$T=gsub('T3','3',data$T)
data$T=gsub('T4','4',data$T)
#N
#N0=1，N1=2，N2=3，N3=4
table(data$N)
data$N=gsub('N0','1',data$N)
data$N=gsub('N1','2',data$N)
data$N=gsub('N2','3',data$N)
data$N=gsub('N3','4',data$N)
#Surgery.of.primary.site
#Endoscopic surgery=1，Excisional biopsy=2
#Partial laryngectomy=3，Total laryngectomy=4
#Pharyngolaryngectomy=5，Surgery NOS=6
table(data$Surgery.of.primary.site)
data$Surgery.of.primary.site=gsub('Endoscopic surgery','1',data$Surgery.of.primary.site)
data$Surgery.of.primary.site=gsub('Excisional biopsy','2',data$Surgery.of.primary.site)
data$Surgery.of.primary.site=gsub('Partial laryngectomy','3',data$Surgery.of.primary.site)
data$Surgery.of.primary.site=gsub('Total laryngectomy','4',data$Surgery.of.primary.site)
data$Surgery.of.primary.site=gsub('Pharyngolaryngectomy','5',data$Surgery.of.primary.site)
data$Surgery.of.primary.site=gsub('Surgery NOS','6',data$Surgery.of.primary.site)
#Lymph.node.dissection
#No=1，Yes=2
table(data$Lymph.node.dissection)
data$Lymph.node.dissection=gsub('No','1',data$Lymph.node.dissection)
data$Lymph.node.dissection=gsub('Yes','2',data$Lymph.node.dissection)
#Radiotherapy
#No=1，Yes=2
table(data$Radiotherapy)
data$Radiotherapy=gsub('No','1',data$Radiotherapy)
data$Radiotherapy=gsub('Yes','2',data$Radiotherapy)
#Chemotherapy
#No=1，Yes=2
table(data$Chemotherapy)
data$Chemotherapy=gsub('No','1',data$Chemotherapy)
data$Chemotherapy=gsub('Yes','2',data$Chemotherapy)
#Regional.nodes.positive
#Negative=1，Positive=2，No examined=3
table(data$Regional.nodes.positive)
data$Regional.nodes.positive=gsub('Negative','1',data$Regional.nodes.positive)
data$Regional.nodes.positive=gsub('Positive','2',data$Regional.nodes.positive)
data$Regional.nodes.positive=gsub('No examined','3',data$Regional.nodes.positive)
#Tumor.size
#0~1 cm =1，1~2 cm =2，2~3 cm=3
#3~4 cm =4，4~5 cm =5，5~6 cm=6
#6~7 cm =7，7~8 cm =8，8~9 cm=9
#9~10 cm =10，>10 cm =11
table(data$Tumor.size)
data$Tumor.size=gsub('0~1 cm','1',data$Tumor.size)
data$Tumor.size=gsub('1~2 cm','2',data$Tumor.size)
data$Tumor.size=gsub('2~3 cm','3',data$Tumor.size)
data$Tumor.size=gsub('3~4 cm','4',data$Tumor.size)
data$Tumor.size=gsub('4~5 cm','5',data$Tumor.size)
data$Tumor.size=gsub('5~6 cm','6',data$Tumor.size)
data$Tumor.size=gsub('6~7 cm','7',data$Tumor.size)
data$Tumor.size=gsub('7~8 cm','8',data$Tumor.size)
data$Tumor.size=gsub('8~9 cm','9',data$Tumor.size)
data$Tumor.size=gsub('9~10 cm','10',data$Tumor.size)
data$Tumor.size=gsub('>10 cm','11',data$Tumor.size)
#Regional.nodes.positive
#Larynx=1，Other Cause of Death=2，Alive=3
table(data$Cause.of.death)
data$Cause.of.death=gsub('Larynx','1',data$Cause.of.death)
data$Cause.of.death=gsub('Other Cause of Death','2',data$Cause.of.death)
data$Cause.of.death=gsub('Alive','3',data$Cause.of.death)
#Vital.status
#Alive=0，Dead=1
table(data$Vital.status)
data$Vital.status=gsub('Alive','0',data$Vital.status)
#Marital.status.at.diagnosis
#Married=1，Single=2，Divorced=3，Widowed=4，Other=5
table(data$Marital.status.at.diagnosis)
data$Marital.status.at.diagnosis=gsub('Married','1',data$Marital.status.at.diagnosis)
data$Marital.status.at.diagnosis=gsub('Single','2',data$Marital.status.at.diagnosis)
data$Marital.status.at.diagnosis=gsub('Divorced','3',data$Marital.status.at.diagnosis)
data$Marital.status.at.diagnosis=gsub('Widowed','4',data$Marital.status.at.diagnosis)
data$Marital.status.at.diagnosis=gsub('Other','5',data$Marital.status.at.diagnosis)

######data preperation######
data$Sex<-as.factor(data$Sex)
data$Laterality<-as.factor(data$Laterality)
data$Age.group<-as.factor(data$Age.group)
data$Primary.site<-as.factor(data$Primary.site)
data$Extent.of.Disease<-as.factor(data$Extent.of.Disease)
data$T<-as.factor(data$T)
data$N<-as.factor(data$N)
data$Surgery.of.primary.site<-as.factor(data$Surgery.of.primary.site)
data$Radiotherapy<-as.factor(data$Radiotherapy)
data$Chemotherapy<-as.factor(data$Chemotherapy)
data$Tumor.size<-as.factor(data$Tumor.size)
data$Marital.status.at.diagnosis<-as.factor(data$Marital.status.at.diagnosis)
data$Vital.status<-as.logical(data$Vital.status)
data$Survival.months<-as.numeric(data$Survival.months)
data=data.frame(data)

##########################################
#########Building  model##################

#cox model
random_tune <- makeTuneControlRandom(maxit = 1L)
rdesc = makeResampleDesc( "CV", iters = 10, stratify = TRUE ) #"Holdout")
###1
seeds<-c(1:50)
magic_for(print, silent = TRUE)
for (k in seeds){
  set.seed(k)
  index <- sample(1:nrow(data), round(0.3*nrow(data)))
  test_set <- data[-index,]
  train_set<- data[index,]
  task<-makeSurvTask(data = train_set,target=c('Survival.months','Vital.status'))
  cox.lrn <- makeLearner(cl="surv.coxph", 
                         predict.type="response")
  modcox = train(cox.lrn, task) 
  train_pred<-predict(modcox, newdata = train_set)
  train_p<-performance(train_pred, measures = list(cindex)) 
  test_pred<-predict(modcox, newdata = test_set)
  test_p<-performance(test_pred, measures = list(cindex)) 
  print(round(train_p,3),round(test_p,3))
}
performance<-magic_result_as_dataframe()
summary(performance)


#Random survival forest
magic_for(print, silent = TRUE)


for (k in seeds) {
  set.seed(k)
  index <- sample(1:nrow(data), round(0.3*nrow(data)))
  train_set <- data[index,]
  test_set <- data[-index,]
  train_set<-train_set[,c(1:14)]
  test_set<-test_set[,c(1:14)]
  common <- intersect(names(train_set), names(test_set)) 
  for (p in common) { 
    if (class(train_set[[p]]) == "factor") { 
      levels(test_set[[p]]) <- levels(train_set[[p]]) 
      levels(train_set[[p]]) <- levels(test_set[[p]]) 
      # print(levels(test_set[[p]]))
    } 
  }
  task<-makeSurvTask(data = train_set,
                     target=c('Survival.months','Vital.status'))
  rfsrc.lrn<-makeLearner(cl='surv.randomForestSRC',
                         predict.type = 'response')
  getParamSet("surv.randomForestSRC")
  model_params_3<-makeParamSet(
    makeIntegerParam('ntree', lower=10,upper=100),
    makeIntegerParam('mtry',lower = 1,upper = 3),
    #makeIntegerParam('nodedepth',lower = 1, upper = 20),
    #makeIntegerParam('nodesize',lower = 3, upper=100),
    makeIntegerParam('nsplit',lower = 0, upper=10),
    makeDiscreteParam('splitrule',values = 'logrank', special.vals = list('logrank','logrankscore','random'))
  )
  # Tune model to find best performing parameter settings using random search algorithm
  tuned_model_3 <- tuneParams(learner = rfsrc.lrn,
                              task = task,
                              resampling = rdesc,
                              measures =  cindex,     
                              par.set = model_params_3,
                              control = random_tune,
                              show.info = FALSE)
  # Apply optimal parameters to model
  rfsrc.lrn <- setHyperPars(learner = rfsrc.lrn,
                            par.vals = tuned_model_3$x)
  modrfsrc = train(rfsrc.lrn, task)
  train_pred_rfsrc<-predict(modrfsrc, newdata = train_set)
  train_p<-performance(train_pred_rfsrc, measures = list(cindex)) # c-index in training set
  test_pred_rfsrc<-predict(modrfsrc, newdata = test_set) #prediction in test set
  test_p<-performance(test_pred_rfsrc, measures = list(cindex)) #  
  print(round(train_p,3),round(test_p,3))
}
performance<-magic_result_as_dataframe()
summary(performance)

# overtime cindex using pec package ###
# overtime C index in complete cases for training set
ApparrentCindex1 <- pec::cindex(list(cox1,
                                     rsf1),
                                formula=Surv(Survival.months, Vital.status) ~ .,
                                data=train_set,
                                splitMethod="cv",
                                B=10,
                                eval.times=seq(0,60,5))
plot(ApparrentCindex1,legend=c(0,1),xlim=c(0,60))
legend("topright", legend=c("Cox","RSF"),
       col=c('red','blue'), lty=1, cex=0.8)
library(prodlim)
library(survival)
###########
PredError <- pec(object=list(cox1 ,rsf1),
                 formula=Surv(Survival.months, Vital.status) ~ .,
                 data=train_set_1,
                 exact=TRUE,
                 cens.model="marginal",
                 splitMethod="none",
                 B=0,
                 verbose=TRUE)

print(PredError,times=seq(5,30,5))
summary(PredError)
plot(PredError)


# calibration plot 
library(rpart)
library(pec)
library(party)
set.seed(12345)
index1 <- sample(1:nrow(data), round(0.7*nrow(data)))
train_set_1 <- data[index1,]
test_set_1 <- data[-index1,]
cox1 <- coxph(Surv(Survival.months, Vital.status) ~., x = T, y = T, 
              data = train_set_1)
rsf1 <- rfsrc(Surv(Survival.months, Vital.status)~.,
              data=train_set_1,
              ntree=100,forest=TRUE,
              tree.err=T, importance=T,
              na.action = "na.impute")
cf1=calPlot(list(cox1 ,rsf1),
            col=c('red','blue'),
            time=60,
            type="survival",
            legend=F, 
            data = test_set_1,
            splitMethod = "cv",
            B=10
)
plot(bst1)
cf1=calPlot(list(cox1 ,rsf1),
            col=c('red','blue'),
            time=36,
            type="survival",
            legend=F, 
            data = test_set_1,
            splitMethod = "cv",
            B=10
)
legend("topleft", legend=c("Cox","RSF"),
       col=c('red','blue'), lty=1, cex=0.8)
cf1=calPlot(bst1,
            col=c("black"),
            time=36,
            type="survival",
            legend=F, 
            data = test_set_1,
            splitMethod = "cv",
            B=10
)




