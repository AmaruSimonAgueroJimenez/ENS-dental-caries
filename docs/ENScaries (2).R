#PACKAGES#######################################################################
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
library(dplyr)
library(ggraph)
library(igraph)
library(e1071)
library(caTools)
library(class)

setwd("/Users/andreacorrea/Documents/R/ENS tesis")
dat <- read.dta13("23.07.26_base_ens_5520_R_enviar.dta")

set.seed(1)
#FUNCTIONS######################################################################
tree_func <- function(final_model, 
                      tree_num) {
  
  # get tree by index
  tree <- randomForest::getTree(final_model, 
                                k = tree_num, 
                                labelVar = TRUE) %>%
    tibble::rownames_to_column() %>%
    # make leaf split points to NA, so the 0s won't get plotted
    mutate(`split point` = ifelse(is.na(prediction), `split point`, NA))
  
  # prepare data frame for graph
  graph_frame <- data.frame(from = rep(tree$rowname, 2),
                            to = c(tree$`left daughter`, tree$`right daughter`))
  
  # convert to graph and delete the last node that we don't want to plot
  graph <- graph_from_data_frame(graph_frame) %>%
    delete_vertices("0")
  
  # set node labels
  V(graph)$node_label <- gsub("_", " ", as.character(tree$`split var`))
  V(graph)$leaf_label <- as.character(tree$prediction)
  V(graph)$split <- as.character(round(tree$`split point`, digits = 2))
  
  # plot
  plot <- ggraph(graph, 'dendrogram') + 
    theme_bw() +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = node_label), na.rm = TRUE, repel = TRUE) +
    geom_node_label(aes(label = split), vjust = 2.5, na.rm = TRUE, fill = "white") +
    geom_node_label(aes(label = leaf_label, fill = leaf_label), na.rm = TRUE, 
                    repel = TRUE, colour = "white", fontface = "bold", show.legend = FALSE) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 18))
  
  print(plot)
}

#setwd("C:/Users/Pc/iCloudDrive/Desktop/Asesorias Estadisticas/Andrea Correa")
# setwd("~/Desktop/Asesorias Estadisticas/Andrea Correa")
#setwd("/Users/andreacorrea/Documents/R/ENS tesis")

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
#dat 3Principal Components Analysis (PCA), Correspondence analysis (CA), Multiple Correspondence Analysis (MCA)
dat3<- dat %>% dplyr::select("cariesnt",
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

dat3$cariesnt <- factor(dat3$cariesnt,levels=c(0,1),labels = c("Libre de Caries","Caries"))
dat3<- dat3 %>% filter(remanentes > 0)
dat3<-drop_na(dat3)

#MISS DATA######################################################################

# vis_miss(dat2,
#          sort_miss = TRUE,
#          cluster = TRUE)
# 
# gg_miss_upset(dat2)


#LM1############################################################################
#dat2
# m1<-glm(cariesnt ~ 
#           die1a + 
#           die2 + 
#           die3 + 
#           die4 + 
#           die5 + 
#           die6 + 
#           die7 + 
#           die8 + 
#           die9 +
#           die10_a +
#           die10_b +
#           die10_c +
#           die10_e +
#           die10_f +
#           die11 +
#           die14 +
#           frecsembebida +
#           frecsemjugo + sexo + edad_cat + zona + nedu + region + indigena + c1, data=dat2, family = `binomial`)
# 
# summary(m1)
# check_model(m1)

#LM2############################################################################
#dat2
# m2<-glm(cariesnt ~ 
#           die1a + 
#           die2 + 
#           die3 + 
#           die4 + 
#           die5 + 
#           die6 + 
#           die7 + 
#           die8 + 
#           die9 +
#           die10_b +
#           die11 +
#           die14 +
#           frecsembebida +
#           frecsemjugo + sexo + edad_cat + zona + nedu + region + indigena + c1, data=dat2, family = `binomial`)
# 
# summary(m2)
# check_model(m2)

#DECISION TREE dat2#############################################################

c_ctree1 <- ctree(cariesnt ~ 
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
                    frecsemjugo + 
                    sexo + 
                    Edad + 
                    zona + 
                    nedu + 
                    region + 
                    indigena + 
                    c1, 
                  data=dat2,)

# Customizing the output

png("my_plot1.png", units="in", width=18, height=12, res=900)   # Background color    # Color model (cmyk is required for most publications)          # Paper size

# Creating a plot
plot(c_ctree1, cex=2)

# Closing the graphical device
dev.off()

#DECISION TREE dat3############################################################# 
c_ctree1.2 <- party::ctree(cariesnt ~ 
                             sexo + 
                             Edad + 
                             zona + 
                             nedu + 
                             as.factor(macrozona) + 
                             indigena + 
                             comp+
                             c1, 
                           data=dat3, 
                    controls = party::ctree_control(minsplit =0, minbucket = 0))

# Customizing the output

png("my_plot1.2.png", units="in", width=18, height=12, res=900)   # Background color    # Color model (cmyk is required for most publications)          # Paper size

# Creating a plot
plot(c_ctree1.2, cex=2)

# Closing the graphical device
dev.off()

#DECISION TREE dat3 region######################################################
c_ctree1.2.2 <- party::ctree(cariesnt ~ 
                             sexo + 
                             Edad + 
                             zona + 
                             nedu + 
                             region + 
                             indigena + 
                             comp+
                             c1, 
                           data=dat3, 
                           controls = party::ctree_control(minsplit =0, minbucket = 0))

# Customizing the output

png("my_plot1.2.2.png", units="in", width=18, height=12, res=900)   # Background color    # Color model (cmyk is required for most publications)          # Paper size

# Creating a plot
plot(c_ctree1.2.2, cex=2)

# Closing the graphical device
dev.off() 


#DECISION TREE dat2 by macrozone################################################
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
          frecsemjugo + sexo + Edad + zona + nedu + indigena + c1, data=macrozona[[i]])
  
}

plot(macrozonaarbol[[1]])
plot(macrozonaarbol[[2]])
plot(macrozonaarbol[[3]])
plot(macrozonaarbol[[4]])
plot(macrozonaarbol[[5]])

#DECISION TREE dat2 by age group################################################

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


#DECISION TREE dat3 by macrozone################################################
macrozona<-c()
macrozonaarbol<-c()
for (i in 1:5) {
  macrozona[[i]]<- dat3 %>% filter(macrozona==i)
  macrozonaarbol[[i]]<-ctree(cariesnt ~ 
                               sexo + 
                               Edad + 
                               zona + 
                               nedu + 
                               indigena + 
                               c1 + 
                               comp, data=macrozona[[i]])
  
}

plot(macrozonaarbol[[1]])
plot(macrozonaarbol[[2]])
plot(macrozonaarbol[[3]])
plot(macrozonaarbol[[4]])
plot(macrozonaarbol[[5]])

#DECISION TREE dat3 by age group################################################

menores30<- dat3  %>% filter(Edad <30)
mayores30<- dat3  %>% filter(Edad >=30)

menores30arbol<- ctree(cariesnt ~ 
                         sexo + 
                         Edad + 
                         zona + 
                         nedu + 
                         as.factor(macrozona) + 
                         indigena + 
                         c1 + 
                         comp, data=menores30)

mayores30arbol<- ctree(cariesnt ~ 
                         sexo + 
                         Edad + 
                         zona + 
                         nedu + 
                         as.factor(macrozona) + 
                         indigena + 
                         c1 + 
                         comp, data=mayores30)

plot(menores30arbol,cex=2)
plot(mayores30arbol,cex=2)

#REGRESSION TREE RECURSIVE PARTITIONING#########################################
# 
# set.seed(2020)
# training.ids <- createDataPartition(dat2$cariesnt, p = 0.9, list = F)
# test <- dat2[-training.ids,]
# 
# mod <- rpart(cariesnt ~ 
#                die1a + 
#                die2 + 
#                die3 + 
#                die4 + 
#                die5 + 
#                die6 + 
#                die7 + 
#                die8 + 
#                die9 +
#                die10_a +
#                die10_b +
#                die10_c +
#                die10_e +
#                die10_f +
#                die11 +
#                die14 +
#                frecsembebida +
#                frecsemjugo + sexo + Edad + zona + nedu + as.factor(macrozona) + indigena + c1, 
#              data = dat2[training.ids,],
#              method = "class", 
#              control = rpart.control(minsplit = 0, cp = 0.0019))
# plot(as.party(mod))
# mod$cptable
# cp<-mod$cptable[which.min(mod$cptable[, "xerror"]), "CP"]
# mod.pruned <- prune(mod, cp = cp)
# mod.pruned
# plot(as.party(mod.pruned))
# mod.pred <- predict(mod, test, type="class")
# mod.pred.ba <- predict(mod, test, type="prob")
# table <- table(test$cariesnt ,mod.pred, dnn = c("Actual", "Predicho"))
# table

#RANDOM FOREST dat2#############################################################
#train data and test data#
split <- sample.split(dat2, SplitRatio = 0.9)#0.9 = 90% train data 
train <- subset(dat2, split == "TRUE") 
test <- subset(dat2, split == "FALSE") 

# Fitting Random Forest to the train dataset
set.seed(1)
classifier_RF = randomForest(x = train[,c(-1,-22,-26,-29)], #-cariesnt, edad_cat,-macrozona - remanentes
                             y = train$cariesnt, 
                             ntree = 500,
                             importance = T) 

classifier_RF 

# Predicting the Test set results 
y_pred = predict(classifier_RF, newdata = test[,c(-1,-22,-26,-29)]) 

# Confusion Matrix 
confusion_mtx = table(test[,c(1)], y_pred) 
confusion_mtx

plot(classifier_RF)

# Plotting model 
plot(classifier_RF)

# Importance plot 
importance(classifier_RF) 

# Variable importance plot 
varImpPlot(classifier_RF)

dat2.2 <- dat2
dat2.2[,c(2:30)] <- sapply(dat2.2[,c(2:30)],as.integer)
cf <- party::cforest(cariesnt ~ 
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
                       frecsemjugo + 
                       sexo + 
                       Edad + 
                       zona + 
                       nedu + 
                       region + 
                       indigena + 
                       c1, 
                     data=dat2.2, controls = party::cforest_unbiased(minsplit=20, minbucket = 7, maxdepth=4))

pt <- prettytree(cf@ensemble[[3]], names(cf@data@get("input"))) 
nt <- new("BinaryTree") 
nt@tree <- pt 
nt@data <- cf@data 
nt@responses <- cf@responses 
png("tree_die_reg.png", units="in", width=20, height=14, res=900)   # Background color    # Color model (cmyk is required for most publications)          # Paper size
plot(nt)
dev.off()

party::varimp(cf) %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(`.`) %>%
  mutate(rowname = forcats::fct_inorder(rowname )) %>%
  ggplot()+
  geom_col(aes(x = fct_reorder(rowname, `.`), y = `.`))+
  coord_flip()+
  theme_bw()


# Fitting KNN Model dat2########################################################
# Splitting data into train
# and test data
split <- sample.split(dat2.2, SplitRatio = 0.9)
train_cl <- subset(dat2.2, split == "TRUE")
test_cl <- subset(dat2.2, split == "FALSE")

# Feature Scaling
train_scale <- scale(train_cl[,c(-1,-22,-26,-29)])
test_scale <- scale(test_cl[,c(-1,-22,-26,-29)])

# to training dataset
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$cariesnt,
                      k = 1)
classifier_knn

# Confusiin Matrix
cm <- table(test_cl$cariesnt, classifier_knn)
cm

# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$cariesnt)
print(paste('Accuracy =', 1-misClassError))

# K = 3
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$cariesnt,
                      k = 3)
misClassError <- mean(classifier_knn != test_cl$cariesnt)
print(paste('Accuracy =', 1-misClassError))

# K = 5
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$cariesnt,
                      k = 5)
misClassError <- mean(classifier_knn != test_cl$cariesnt)
print(paste('Accuracy =', 1-misClassError))

# K = 7
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$cariesnt,
                      k = 7)
misClassError <- mean(classifier_knn != test_cl$cariesnt)
print(paste('Accuracy =', 1-misClassError))

# K = 15
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$cariesnt,
                      k = 15)
misClassError <- mean(classifier_knn != test_cl$cariesnt)
print(paste('Accuracy =', 1-misClassError))

# K = 19
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$cariesnt,
                      k = 19)
misClassError <- mean(classifier_knn != test_cl$cariesnt)
print(paste('Accuracy =', 1-misClassError))

# plot tree
# tree_func(classifier_RF,min(classifier_RF$forest$ndbigtree))

#RANDOM FOREST dat3#############################################################
#train data and test data# #ESTE NO ES PARA LA TESIS!
set.seed(1)
split <- sample.split(dat3, SplitRatio = 0.9)#0.9 = 90% train data 
train <- subset(dat3, split == "TRUE") 
test <- subset(dat3, split == "FALSE") 

# Fitting Random Forest to the train dataset
classifier_RF = randomForest(x = train[,c(-1,-3,-7,-10)], #-cariesnt, -macrozona - remanentes
                             y = train$cariesnt, 
                             ntree = 500,
                             importance = T) 

classifier_RF 

# Predicting the Test set results 
y_pred = predict(classifier_RF, newdata = test[,c(-1,-3,-7,-10)]) 

# Confusion Matrix 
confusion_mtx = table(test[,c(1)], y_pred) 
confusion_mtx

# Plotting model 
plot(classifier_RF)

# Importance plot 
importance(classifier_RF, type = 1, scale = T) 
importance(classifier_RF, type = 2, scale = T) 
# Variable importance plot 
varImpPlot(classifier_RF,scale = T)
#"Global" variable importance is the mean decrease of accuracy over all out-of-bag cross validated predictions, when a given variable is permuted after training, but before prediction. "Global" is implicit. Local variable importance is the mean decrease of accuracy by each individual out-of-bag cross validated prediction. Global variable importance is the most popular, as it is a single number per variable, easier to understand, and more robust as it is averaged over all predictions.

# plot tree
# tree_num <- which(classifier_RF$forest$ndbigtree == min(classifier_RF$forest$ndbigtree))
# tree_func(classifier_RF, tree_num)
dat3.2 <- dat3
dat3.2[,c(2:6,9)] <- sapply(dat3.2[,c(2:6,9)],as.integer)
cf <- party::cforest(cariesnt ~ 
                       sexo + 
                       Edad + 
                       zona + 
                       nedu + 
                       region + 
                       indigena + 
                      comp+
                       c1, 
                             data=dat3.2, controls = party::cforest_unbiased(minsplit=20, minbucket = 7, maxdepth=5))

pt <- prettytree(cf@ensemble[[1]], names(cf@data@get("input"))) 
nt <- new("BinaryTree") 
nt@tree <- pt 
nt@data <- cf@data 
nt@responses <- cf@responses 
plot(nt)

party::varimp(cf) %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(`.`) %>%
  mutate(rowname = forcats::fct_inorder(rowname )) %>%
  ggplot()+
  geom_col(aes(x = fct_reorder(rowname, `.`), y = `.`))+
  coord_flip()+
  theme_bw()
  
# Fitting KNN Model dat3#########################################################
# Splitting data into train
# and test data
split <- sample.split(dat3.2, SplitRatio = 0.9)
train_cl <- subset(dat3.2, split == "TRUE")
test_cl <- subset(dat3.2, split == "FALSE")

# Feature Scaling
train_scale <- scale(train_cl[,c(-1,-3,-7,-10)])
test_scale <- scale(test_cl[,c(-1,-3,-7,-10)])

# to training dataset
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$cariesnt,
                      k = 1)
classifier_knn

# Confusiin Matrix
cm <- table(test_cl$cariesnt, classifier_knn)
cm

# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$cariesnt)
print(paste('Accuracy =', 1-misClassError))

# K = 3
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$cariesnt,
                      k = 3)
misClassError <- mean(classifier_knn != test_cl$cariesnt)
print(paste('Accuracy =', 1-misClassError))

# K = 5
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$cariesnt,
                      k = 5)
misClassError <- mean(classifier_knn != test_cl$cariesnt)
print(paste('Accuracy =', 1-misClassError))

# K = 7
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$cariesnt,
                      k = 7)
misClassError <- mean(classifier_knn != test_cl$cariesnt)
print(paste('Accuracy =', 1-misClassError))

# K = 15
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$cariesnt,
                      k = 15)
misClassError <- mean(classifier_knn != test_cl$cariesnt)
print(paste('Accuracy =', 1-misClassError))

# K = 19
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$cariesnt,
                      k = 19)
misClassError <- mean(classifier_knn != test_cl$cariesnt)
print(paste('Accuracy =', 1-misClassError))

