library(readstata13)
library(tidyverse)
library(naniar)
library(performance)
library(party)
library(caret)
library(rpart)
library(partykit)
library(ROCR)
library(data.tree)
library(randomForest)
library(caTools)
library(forcats)

setwd("/Users/andreacorrea/Documents/R/ENS tesis")

dat <- read.dta13("23.07.26_base_ens_5520_R_enviar.dta")

dat2<- dat %>% dplyr::select("cariesnt",
                             "die1a",
                             "die2",
                             "die3",
                             "die4",
                             "die5",
                             "die6",
                             "die7",
                             "die8",
                             "die9",
                             "die10_a",
                             "die10_b",
                             "die10_c",
                             "die10_d", 
                             "die10_e",
                             "die10_f",
                             "die11",
                             "die14",
                             "frecsembebida",
                             "frecsemjugo",
                             "sexo",
                             "edad_cat",
                             "zona",
                             "nedu",
                             "region",
                             "macrozona",
                             "indigena",
                             "c1",
                             "remanentes",
                             Edad)

dat2$cariesnt <- factor(dat2$cariesnt,levels=c(0,1),labels = c("Libre de Caries","Caries"))
dat2<- dat2 %>% filter(remanentes > 0) 

dat2[,4]<- fct_na_value_to_level(dat2[,4], "NO CONSUME LACTEOS")
dat2$die7<- dat2$die7 %>% replace(is.na(.),0)
dat2$die9<- dat2$die9 %>% replace(is.na(.),0)
dat2$die11<- droplevels(dat2$die11)
dat2<-drop_na(dat2)

#MISS DATA######################################################################
vis_miss(dat2,
         sort_miss = TRUE,
         cluster = TRUE)

gg_miss_upset(dat2)

#LM1############################################################################
m1<-glm(cariesnt ~ 
          die1a + 
          die2 + 
          die3 + 
          die4 + 
          die5 + 
          die6 + 
          die7 + 
          die8 + 
          die9 +
          die10_a +
          die10_b +
          die10_c +
          die10_e +
          die10_f +
          die11 +
          die14 +
          frecsembebida +
          frecsemjugo + sexo + edad_cat + zona + nedu + region + indigena + c1, data=dat2, family = `binomial`)

summary(m1)
check_model(m1)
#LM2############################################################################
m2<-glm(cariesnt ~ 
          die1a + 
          die2 + 
          die3 + 
          die4 + 
          die5 + 
          die6 + 
          die7 + 
          die8 + 
          die9 +
          die10_b +
          die11 +
          die14 +
          frecsembebida +
          frecsemjugo + sexo + edad_cat + zona + nedu + region + indigena + c1, data=dat2, family = `binomial`)

summary(m2)
check_model(m2)

#macrozone

macrozona<-c()
macrozonaarbol<-c()
for (i in 1:5) {
  macrozona[[i]]<- dat2  %>% filter(macrozona==i)
  macrozonaarbol[[i]]<-ctree(cariesnt ~ 
                               die1a + 
                               die2 + 
                               die3 + 
                               die4 + 
                               die5 + 
                               die6 + 
                               die7 + 
                               die8 + 
                               die9 +
                               die10_a +
                               die10_b +
                               die10_c +
                               die10_e +
                               die10_f +
                               die11 +
                               die14 +
                               frecsembebida +
                               frecsemjugo + sexo + Edad + zona + nedu + macrozona + indigena + c1, data=macrozona[[i]])
  
}

plot(macrozonaarbol[[1]])
plot(macrozonaarbol[[2]])
plot(macrozonaarbol[[3]])
plot(macrozonaarbol[[4]])
plot(macrozonaarbol[[5]])

menores30<- dat2  %>% filter(Edad <30)
mayores30<- dat2  %>% filter(Edad >=30)

menores30arbol<- ctree(cariesnt ~ 
                         die1a + 
                         die2 + 
                         die3 + 
                         die4 + 
                         die5 + 
                         die6 + 
                         die7 + 
                         die8 + 
                         die9 +
                         die10_a +
                         die10_b +
                         die10_c +
                         die10_e +
                         die10_f +
                         die11 +
                         die14 +
                         frecsembebida +
                         frecsemjugo + sexo + Edad + zona + nedu + as.factor(macrozona) + indigena + c1, data=menores30)

mayores30arbol<- ctree(cariesnt ~ 
                         die1a + 
                         die2 + 
                         die3 + 
                         die4 + 
                         die5 + 
                         die6 + 
                         die7 + 
                         die8 + 
                         die9 +
                         die10_a +
                         die10_b +
                         die10_c +
                         die10_e +
                         die10_f +
                         die11 +
                         die14 +
                         frecsembebida +
                         frecsemjugo + sexo + Edad + zona + nedu + as.factor(macrozona) + indigena + c1, data=mayores30)

plot(menores30arbol,cex=2)
plot(mayores30arbol,cex=2)
#REGRESSION TREE RECURSIVE PARTITIONING#########################################

set.seed(2023)
training.ids2 <- createDataPartition(dat2$cariesnt, p = 0.9, list = F)
test2 <- dat[-training.ids,]

mod <- rpart(cariesnt ~ 
               as.factor(dat$comp) + sexo + Edad + zona + nedu + as.factor(macrozona) + indigena + c1, 
             data = dat[training.ids2,],
             method = "class", 
             control = rpart.control(minsplit = 0, cp = 0.0019))
plot(as.party(mod))
mod$cptable
cp2<-mod$cptable[which.min(mod$cptable[, "xerror"]), "CP"]
mod.pruned2 <- prune(mod, cp = cp)
mod.pruned2
plot(as.party(mod.pruned2))
mod.pred2 <- predict(mod, test, type="class")
mod.pred.ba2 <- predict(mod, test, type="prob")
table2 <- table(test$cariesnt ,mod.pred, dnn = c("Actual", "Predicho"))
table2

###creo dat3 incluyendo los comp####
dat3<- dat %>% dplyr::select("cariesnt",
                             "die1a",
                             "die2",
                             "die3",
                             "die4",
                             "die5",
                             "die6",
                             "die7",
                             "die8",
                             "die9",
                             "die10_a",
                             "die10_b",
                             "die10_c",
                             "die10_d", 
                             "die10_e",
                             "die10_f",
                             "die11",
                             "die14",
                             "frecsembebida",
                             "frecsemjugo",
                             "sexo",
                             "edad_cat",
                             "zona",
                             "nedu",
                             "region",
                             "macrozona",
                             "indigena",
                             "c1",
                             "remanentes",
                             "comp",
                             Edad)

sapply(train, function(x) sum(is.na(x)))
dat3$cariesnt <- factor(dat3$cariesnt,levels=c(0,1),labels = c("Libre de Caries","Caries"))
dat3<- dat3 %>% filter(remanentes > 0) 

dat3[,4]<- fct_na_value_to_level(dat3[,4], "NO CONSUME LACTEOS")
dat3$die7<- dat3$die7 %>% replace(is.na(.),0)
dat3$die9<- dat3$die9 %>% replace(is.na(.),0)
dat3$die11<- droplevels(dat3$die11)
dat3<-drop_na(dat3)

#RANDOM FOREST con comp##################################################################
#train data and test data#######################################################

split2 <- sample.split(dat3, SplitRatio = 0.9)#0.9 = 90% train data 
train2 <- subset(dat3, split = "TRUE") 
test2 <- subset(dat3, split = "FALSE") 

# Fitting Random Forest to the train dataset
classifier_RF = randomForest(x = train2[,c(-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-18,-19,-20,-22,-25,-29)], 
                             y = train2$cariesnt, 
                             ntree = 500) 

classifier_RF 

# Predicting the Test set results 
y_pred2 = predict(classifier_RF, newdata = test2[,c(-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-18,-19,-20,-22,-25,-29)]) 

# Confusion Matrix 
confusion_mtx2 = table(test2[,c(1)], y_pred2) 
confusion_mtx2

plot(classifier_RF)

# Plotting model 
plot(classifier_RF)
importance(classifier_RF)

# Importance plot 
importance(classifier_RF) 

# Variable importance plot 
varImpPlot(classifier_RF)

#RANDOM FOREST con comp y region##################################################################
#train data and test data#######################################################

split2 <- sample.split(dat3, SplitRatio = 0.9)#0.9 = 90% train data 
train2 <- subset(dat3, split = "TRUE") 
test2 <- subset(dat3, split = "FALSE") 

# Fitting Random Forest to the train dataset (con region y no macrozona y usando los comp)
classifier_RF = randomForest(x = train2[,c(-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-18,-19,-20,-22,-26,-29)], 
                             y = train2$cariesnt, 
                             ntree = 500) 

classifier_RF 

# Predicting the Test set results 
y_pred2 = predict(classifier_RF, newdata = test2[,c(-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-18,-19,-20,-22,-26,-29)]) 

# Confusion Matrix 
confusion_mtx2 = table(test2[,c(1)], y_pred2) 
confusion_mtx2

plot(classifier_RF)

# Plotting model 
plot(classifier_RF)
importance(classifier_RF)

# Importance plot 
importance(classifier_RF) 

# Variable importance plot 
varImpPlot(classifier_RF)

##Trato de graficar el árbol
RFregioncomp <- randomForest(x = train2[,c(-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-18,-19,-20,-22,-26,-29)], 
                             y = train2$cariesnt, 
                             ntree = 500) 

print(RFregioncomp)
plot(RFregioncomp)

getTree(RFregioncomp, k = 5, labelVar = T) # 5to arbol 



