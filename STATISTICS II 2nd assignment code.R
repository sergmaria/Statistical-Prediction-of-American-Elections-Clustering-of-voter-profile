install.packages("caret")
install.packages("lattice")
install.packages("rpart")
install.packages("acc")
install.packages("randomForest")
install.packages("mclust")
install.packages("rpart.plot")
install.packages("ari")
install.packages("tree")
install.packages("reprtree")

library("readxl")
library(data.table)

county_facts <- read_excel("C://Users//maria//Downloads//STATISTICS II//stat BA II project I.xlsx", sheet = 1)
votes <- read_excel("C://Users//maria//Downloads//STATISTICS II//stat BA II project I.xlsx", sheet = 2)


#####################################  PREPARATION OF THE DATASET  ##################################################
summary(county_facts)

votes <- as.data.table(votes)

votes<-na.omit(votes)
summary(votes)

unique(votes$candidate)
votes <- votes[candidate == "Donald Trump", ]

votes[ , success := 0]
votes[fraction_votes >= 0.5 , success:=1]
votes[fraction_votes < 0.5 , success:=0]

str(votes)

fra_suc <- unique(votes[ , c("fraction_votes","success")])
fra_suc <- fra_suc[order(fraction_votes)]

votes_extend <- votes[ , c("fips","success")]

county_facts <- data.table(county_facts, key = "fips") 
votes_extend <- data.table(votes_extend, key = "fips")
summary(votes_extend)


fulldata <- merge(county_facts, votes_extend)
summary(fulldata)

# removing the unnecessary columns

fulldata = subset(fulldata, select = -c(state_abbreviation,area_name,fips) )
summary(fulldata)

sapply(fulldata,sd)
str(fulldata)

head(fulldata)


chosen_data <- fulldata[,c('success', 'PST120214', 'AGE135214', 
                             'AGE775214', 'SEX255214', 'RHI225214', 'RHI625214', 
                             'POP715213', 'POP645213', 'EDU685213', 
                             'HSG445213', 'HSG096213', 
                             'HSG495213', 'PVY020213', 
                             'SBO315207', 'SBO515207', 
                             'SBO415207', 'LND110210', 
                             'POP060210')]


##data shuffle
shuffle_index <- sample(1:nrow(chosen_data))
head(shuffle_index)

chosen_data <- chosen_data[shuffle_index, ]
head(chosen_data)

n<- nrow(chosen_data)
chosen_data <- chosen_data[sample(n), ]
head(chosen_data)
###############################################   PART A'  #################################################

#######  Splitting data into training and testing sets  ####################################

data <- chosen_data
n_data2 <- round(nrow(chosen_data) * 0.80)

set.seed(22)
data2 <- sample(1:nrow(chosen_data), n_data2, replace= FALSE)
validation <- (1:nrow(chosen_data))[-data2]

train <- data[data2, ]
test <- data[validation, ]

train <- as.data.frame(train)
test <- as.data.frame(test)

table(train$success)
round(prop.table(table(train$success)),1)

table(test$success)
round(prop.table(table(test$success)),1)

##  confirming the correct split of the data  ##
dim(train); dim(test)

prop.table(table(train$success))
prop.table(table(test$success))

str(train)

#-------------------------------- decision tree ----------------------------------------------------------#


library(rpart)
set.seed(22)

treemodel <-rpart(success~.,
                data=train,
                method = "class")
summary(treemodel)
library(rpart.plot)
rpart.plot(treemodel, nn=TRUE)

predtreemodel <- predict(object=treemodel,test[-1],type="class")

cmtreemodel <- print(table(predtreemodel, test$success, 
                       dnn=c("Predicted", "Actual")))

AccuracyTree <- print((cmtreemodel[2,2]+cmtreemodel[1,1])/sum(cmtreemodel) * 100)

SensitivityTree <- print(cmtreemodel[1,1]/(cmtreemodel[1,1]+cmtreemodel[2,1])*100)

SpecificityTree <- print(cmtreemodel[2,2]/(cmtreemodel[2,2]+cmtreemodel[1,2])*100)



#------------------------------------- Random Forest --------------------------------------#
library(randomForest)
set.seed(22)
rforest_model <- randomForest(as.factor(success) ~., data = train)
print(rforest_model)


test$success <- as.factor(mapvalues(as.factor(test$success), from=c("0","1"),to=c("No", "Yes")))

pred_rforest <- predict(rforest_model, test)

cmrforest_model <- print(table(pred_rforest, test$success, 
                           dnn=c("Predicted", "Actual")))

AccuracyForest <- print((cmrforest_model[2,2]+cmrforest_model[1,1])/sum(cmrforest_model) * 100)

SensitivityForest <- print(cmrforest_model[1,1]/(cmrforest_model[1,1]+cmrforest_model[2,1])*100)

SpecificityForest <- print(cmrforest_model[2,2]/(cmtreemodel[2,2]+cmrforest_model[1,2])*100)


plot(rforest_model, main = 'Out Of Bag error')


varImpPlot(rforest_model,type=2, main = 'mean decrease Gini')

library(rpart)
fit=rpart(success~., train)
rpart.plot(fit)
text(fit)

#-------------------------------- Logistic Regression ------------------------------------#

LogReg <- glm(success ~ .,family=binomial(link="logit"),data=train[ , c(-11 , -13 , -15 , -17)])
print(summary(LogReg))
anova(LogReg, test="Chisq")

test$success <- as.character(test$success)
test$success[test$success=="No"] <- "0"
test$success[test$success=="Yes"] <- "1"

fitted.results <- predict(LogReg,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.5 ,1 ,0)


cmlogreg <- print(table(fitted.results, test$success, 
                               dnn=c("Predicted", "Actual")))

Accuracylogreg <- print((cmlogreg[2,2]+cmlogreg[1,1])/sum(cmlogreg) * 100)

Sensitivitylogreg <- print(cmlogreg[1,1]/(cmlogreg[1,1]+cmlogreg[2,1])*100)

Specificitylogreg <- print(cmlogreg[2,2]/(cmlogreg[2,2]+cmlogreg[1,2])*100)

#---------------------Comparison of the best two models with roc auc--------------------------------------##

pr <- prediction(fitted.results, test$success)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc




####################################         PART B'      #######################################################
rm(list = ls())
install.packages("pgmm")
install.packages("NbClust")
install.packages("factoextra")
install.packages("GGally")
install.packages("writexl")
install.packages("plotly")
library('pgmm')
library('cluster')
library('mclust')
library('NbClust')
library('jpeg')
library("readxl")
library('data.table')
library('factoextra')
library('dplyr')
library('GGally')
library('ggplot2')
library('plotly')

clustcounty_facts <- read_excel("C://Users//maria//Downloads//STATISTICS II//stat BA II project I.xlsx", sheet = 1)
clustvotes <- read_excel("C://Users//maria//Downloads//STATISTICS II//stat BA II project I.xlsx", sheet = 2)

clustvotes <- as.data.table(clustvotes)

clustvotes<-na.omit(clustvotes)

clustdata <- merge(clustcounty_facts, clustvotes)
str(clustdata)

clustdata = subset(clustdata, select = -c(state_abbreviation,area_name,fips,state,party,candidate,votes,fraction_votes
                                          ,county))
str(clustdata)


demographics <- clustdata[, c("PST045214", "PST040210","PST120214",
                                       "POP010210","AGE135214","AGE295214",
                                       "AGE775214","SEX255214","RHI125214","RHI225214",
                                       "RHI325214","RHI425214","RHI525214",
                                       "RHI625214","RHI725214","RHI825214",
                                       "POP715213","POP645213","POP815213",
                                       "EDU635213","EDU685213","VET605213")]
str(demographics)

economics <-clustdata[, !(colnames(clustdata) %in% c("PST045214", "PST040210",
                                                     "PST120214", "POP010210",
                                                     "AGE135214","AGE295214",
                                                     "AGE775214","SEX255214",
                                                     "RHI125214","RHI225214",
                                                     "RHI325214","RHI425214",
                                                     "RHI525214","RHI625214",
                                                     "RHI725214","RHI825214",
                                                       "POP715213","POP645213",
                                                     "POP815213","EDU635213","EDU685213","VET605213"))]
str(economics)


#pairs(demographics)

demographics <- scale(demographics)

km2 <- kmeans(demographics,2)
km3 <- kmeans(demographics,3)
km4 <- kmeans(demographics,4)

#pairs(demographics, col = county$Type)	#true clusters
#pairs(demographics, col = km2$cluster)	#k-means with k = 2
#pairs(demographics, col = km3$cluster)	#k-means with k = 3
#pairs(demographics, col = km4$cluster)	#k-means with k = 4

set.seed(1234)
fviz_nbclust(demographics, kmeans, method = "wss")
fviz_nbclust(demographics, kmeans, method = "silhouette")


#---------------------------------------- K means -------------------------------------------------------

# 5 Ckusters

km5 <- kmeans(demographics, 5,iter.max = 10, nstart=25)

km5$centers

km5$size
adjustedRandIndex(demographics[,1], km5$cluster)

fviz_cluster(km5, data = demographics)

silhouetteclust <- silhouette(km5$cluster, dist(demographics))
fviz_silhouette(silhouetteclust)

# 6 Clusters

km6 <- kmeans(demographics, 6,iter.max = 10, nstart=25)

km6$centers

km6$size
adjustedRandIndex(demographics[,1], km6$cluster)

fviz_cluster(km6, data = demographics)

silhouetteclust6 <- silhouette(km6$cluster, dist(demographics))
fviz_silhouette(silhouetteclust6)


#-------------------------------- explanation with the economics ----------------------------------



clustdemographics <- cbind(demographics, cluster = km5$cluster)
str(clustdemographics)
summary(clustdemographics)

explanation <- cbind(economics, cluster = km5$cluster)
str(explanation)
summary(explanation)


explanation_data <-  aggregate(explanation,by=list(Cluster=explanation$cluster), FUN=mean)
str(explanation_data)

explanation_data = subset(explanation_data, select = -c(cluster) )
str(explanation_data)

explanation_data1 <- t(explanation_data)
explanation_data1 <- as.data.frame(explanation_data)



variable_avg <- (explanation)
str(variable_avg)

variable_avg = subset(variable_avg, select = -c(cluster) )
str(variable_avg)



explanationplot <- ggparcoord(data = explanation, columns = c(1:29), mapping=aes(color= as.factor(cluster)),
                groupColumn = "cluster", scale = "std") + 
  
  labs(x = "Economic", y = "value (in standard-deviation units)", title = "Explanation Plot")+
  theme(axis.text.x = element_text(angle = 90,vjust = 1, hjust = 1))

ggplotly(explanationplot)
