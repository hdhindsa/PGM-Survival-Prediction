### General Comments ######################################################################################################################
# Dataset used: NACD dataset which can be found at http://pssp.srv.ualberta.ca/predictors/32
#This file is used to learn the sructure and parameters of a PGM. Then it draws a diagram of the structure and then 
#shows an example plot of the survival curve of a individual patient from the dataset.
#It then calculates the brier score of the learnt model and prints it

#The model being learnt here is to learn a model with different all the survival nodes in the same model. 
#######################################################################################
#implement model 3

install.packages("bnlearn")
library(bnlearn)
install.packages("dplyr")
library(dplyr)
install.packages("arules")
library(arules)
install.packages("gplots")
library(gplots)
install.packages("BiocManager")
BiocManager::install("Rgraphviz")
library(Rgraphviz)
install.packages("plyr")
library(plyr)


# df is the NACD dataset which can be found at http://pssp.srv.ualberta.ca/predictors/32

setwd('C:\\Program Files\\RStudio\\R')

df <- read.csv("NACD.csv", header = TRUE)


#split data into train and test
data1 = sample(nrow(df), nrow(df)*.7)

train<-df[data1,]

test<-df[-data1,]


df<-train
endStudy = max(as.numeric(as.character(df$SURVIVAL)), na.rm=TRUE)

interval = round(sqrt(endStudy), digits = 0)

bin = endStudy/interval
endInterval = endStudy/interval
beginInterval = 0


ls = c()

df1 <- df

finalL <- vector("list", interval)

for (j in 1:interval){
  
  df1$SURVIVAL <- df$SURVIVAL
  x = c("SURVIVAL",as.character(j))
  y = paste(x,collapse="_")
  
  for (i in 1:nrow(df)){
    
    if ((df$SURVIVAL[i] <= endInterval) && (df$CENSORED[i] == 0) && (df$SURVIVAL[i] > beginInterval)){
      
      df1$SURVIVAL[i] = 1
      
    }
    if ((df$SURVIVAL[i] > endInterval)){
      
      df1$SURVIVAL[i] = 0
    }
    if ((df$SURVIVAL[i] < endInterval) && (df$CENSORED[i] == 0) && (df$SURVIVAL[i] < beginInterval)){
      
      df1$SURVIVAL[i] = 2
      
    }
    if ((df$SURVIVAL[i] < endInterval) && (df$CENSORED[i] == 1)){
      
      df1$SURVIVAL[i] = NA
      
    }
    
    
  }
  
  df1[[y]] = df1$SURVIVAL
  
  
  beginInterval = endInterval
  endInterval = endInterval+bin
}

dfC <- df1[df1$CENSORED == 0,]


dfC[] <- lapply(dfC, as.factor)


df1 <- dfC

#######################################################################


#discretize the categorical variables############################

df2 <- df1
df2[] <- lapply(df2, as.factor)
numCol = ncol(df2)
f = 0
for (i in 1:numCol){
  levLen = length(sapply(df2[i], levels))
  if (levLen > 15){
    z = as.matrix(as.data.frame(lapply(df2[i], as.numeric)))
    df2[, i] <- discretize(z, method = "cluster" ,breaks = 10, labels = c(1,2,3,4,5,6,7,8,9,10))
  }
  
}
dfnew2 <- df2[ -c(2) ]
dfnew2 <- dfnew2[ -c(1) ]


dfnew2[] = lapply(dfnew2, as.factor)

bn = hc(dfnew2)



##############################################################################################

#parameter learning#############################################


Trainedbn = bn.fit(bn, dfnew2, method = "bayes")
#Trainedbn



#singlePatient = finalL[[1]][1,]
#singlePatient


#TrainedbnList <- vector("list", interval)
#for(i in 1:interval){
#for the entire data
#TrainedbnList[[i]] = bn.fit(bnList[[i]], dfnew2 , method = "bayes")

#for a single patient
#TrainedbnList[[i]] = bn.fit(bnList[[i]], singlePatient)
#}
#TrainedbnList[[1]]

###############################################################################################
#########################################################################

#plot the graph structure#####################################



graphviz.plot(Trainedbn, shape = "ellipse")

g <- Rgraphviz::layoutGraph(bnlearn::as.graphNEL(Trainedbn))
graph::nodeRenderInfo(g) <- list(fontsize=80)
Rgraphviz::renderGraph(g)
#typeof(TrainedbnList[[6]])
#bn.fit.histogram(TrainedbnList[[6]])

##############################################################################################
###########################################################################################################
#plotting distribution works
#finalL
patient = dfnew2[5,]

#patient
patientProb = 1:interval


for (i in 1:interval){
  patientProb[i] = cpquery(Trainedbn, (SURVIVAL_3 == 1 | SURVIVAL_3 == 2),  evidence = as.list(patient[1,1:51]) , method = "lw" )
}
patientProb[1] = cpquery(Trainedbn, (SURVIVAL_1 == 1 | SURVIVAL_1 == 2),  evidence = as.list(patient[1,1:51]) , method = "lw" )
patientProb[2] = cpquery(Trainedbn, (SURVIVAL_2 == 1 | SURVIVAL_2 == 2),  evidence = as.list(patient[1,1:51]) , method = "lw" )
patientProb[3] = cpquery(Trainedbn, (SURVIVAL_3 == 1 | SURVIVAL_3 == 2),  evidence = as.list(patient[1,1:51]) , method = "lw" )
patientProb[4] = cpquery(Trainedbn, (SURVIVAL_4 == 1 | SURVIVAL_4 == 2),  evidence = as.list(patient[1,1:51]) , method = "lw" )
patientProb[5] = cpquery(Trainedbn, (SURVIVAL_5 == 1 | SURVIVAL_5 == 2),  evidence = as.list(patient[1,1:51]) , method = "lw" )
patientProb[6] = cpquery(Trainedbn, (SURVIVAL_6 == 1 | SURVIVAL_6 == 2),  evidence = as.list(patient[1,1:51]) , method = "lw" )
patientProb[7] = cpquery(Trainedbn, (SURVIVAL_7 == 1 | SURVIVAL_7 == 2),  evidence = as.list(patient[1,1:51]) , method = "lw" )
patientProb[8] = cpquery(Trainedbn, (SURVIVAL_8 == 1 | SURVIVAL_8 == 2),  evidence = as.list(patient[1,1:51]) , method = "lw" )
patientProb[9] = cpquery(Trainedbn, (SURVIVAL_9 == 1 | SURVIVAL_9 == 2),  evidence = as.list(patient[1,1:51]) , method = "lw" )

#TrainedbnList[[3]]

timeInter = 1:interval

plot(timeInter,patientProb, type="l", col="green", lwd=5, xlab="Time", ylab="Probability of Death", main="Individual Survival")



################################################################################################################
###############################################################################################################
#brier score structure 3

df<-test
endStudy = max(as.numeric(as.character(df$SURVIVAL)), na.rm=TRUE)
#endStudy
#print(endStudy)
interval = round(sqrt(endStudy), digits = 0)
#interval
bin = endStudy/interval
endInterval = endStudy/interval
beginInterval = 0
#noInterval

ls = c()

df1 <- df

finalL <- vector("list", interval)

for (j in 1:interval){
  #ls = c()
  df1$SURVIVAL <- df$SURVIVAL
  x = c("SURVIVAL",as.character(j))
  y = paste(x,collapse="_")
  #print(df1)
  for (i in 1:nrow(df)){
    
    if ((df$SURVIVAL[i] <= endInterval) && (df$CENSORED[i] == 0) && (df$SURVIVAL[i] > beginInterval)){
      #ls = c(ls,i)
      df1$SURVIVAL[i] = 1
      
    }
    if ((df$SURVIVAL[i] > endInterval)){
      #ls = c(ls,i)
      df1$SURVIVAL[i] = 0
    }
    if ((df$SURVIVAL[i] < endInterval) && (df$CENSORED[i] == 0) && (df$SURVIVAL[i] < beginInterval)){
      #ls = c(ls,i)
      df1$SURVIVAL[i] = 2
      
    }
    if ((df$SURVIVAL[i] < endInterval) && (df$CENSORED[i] == 1)){
      #ls = c(ls,i)
      df1$SURVIVAL[i] = NA
      
    }
    
    
  }
  
  df1[[y]] = df1$SURVIVAL
  
  beginInterval = endInterval
  endInterval = endInterval+bin
}

dfC <- df1[df1$CENSORED == 0,]

dfC[] <- lapply(dfC, as.factor)


df1 <- dfC





df2 <- df1
df2[] <- lapply(df2, as.factor)
numCol = ncol(df2)
f = 0
for (i in 1:numCol){
  levLen = length(sapply(df2[i], levels))
  if (levLen > 15){
    z = as.matrix(as.data.frame(lapply(df2[i], as.numeric)))
    df2[, i] <- discretize(z, method = "cluster" ,breaks = 10, labels = c(1,2,3,4,5,6,7,8,9,10))
  }
  
}
dfnew2 <- df2[ -c(2) ]
dfnew2 <- dfnew2[ -c(1) ]
#df2[] = lapply(df2, as.factor)

dfnew2[] = lapply(dfnew2, as.factor)
#finalL[[j]] = dfnew2
#bn = hc(dfnew2)





bScore <- c(0,0,0,0,0,0,0,0,0)
bScore
#for(j in 1:interval){
brierScore = 0
patientProb = 1:interval

for (i in 1:nrow(dfnew2)){
  
  actualProb = 1:interval  
  patient = dfnew2[i,]
  #prob = cpquery(Trainedbn, (SURVIVAL == 1 | SURVIVAL == 2),  evidence = as.list(patient[1, -1]) , method = "lw" )
  
  patientProb[1] = cpquery(Trainedbn, (SURVIVAL_1 == 1 | SURVIVAL_1 == 2),  evidence = as.list(patient[1,1:51]) , method = "lw" )
  patientProb[2] = cpquery(Trainedbn, (SURVIVAL_2 == 1 | SURVIVAL_2 == 2),  evidence = as.list(patient[1,1:51]) , method = "lw" )
  patientProb[3] = cpquery(Trainedbn, (SURVIVAL_3 == 1 | SURVIVAL_3 == 2),  evidence = as.list(patient[1,1:51]) , method = "lw" )
  patientProb[4] = cpquery(Trainedbn, (SURVIVAL_4 == 1 | SURVIVAL_4 == 2),  evidence = as.list(patient[1,1:51]) , method = "lw" )
  patientProb[5] = cpquery(Trainedbn, (SURVIVAL_5 == 1 | SURVIVAL_5 == 2),  evidence = as.list(patient[1,1:51]) , method = "lw" )
  patientProb[6] = cpquery(Trainedbn, (SURVIVAL_6 == 1 | SURVIVAL_6 == 2),  evidence = as.list(patient[1,1:51]) , method = "lw" )
  patientProb[7] = cpquery(Trainedbn, (SURVIVAL_7 == 1 | SURVIVAL_7 == 2),  evidence = as.list(patient[1,1:51]) , method = "lw" )
  patientProb[8] = cpquery(Trainedbn, (SURVIVAL_8 == 1 | SURVIVAL_8 == 2),  evidence = as.list(patient[1,1:51]) , method = "lw" )
  patientProb[9] = cpquery(Trainedbn, (SURVIVAL_9 == 1 | SURVIVAL_9 == 2),  evidence = as.list(patient[1,1:51]) , method = "lw" )
  
  
  
  actualProb[1] = as.numeric(as.character(patient$SURVIVAL_1))
  actualProb[2] = as.numeric(as.character(patient$SURVIVAL_2))
  actualProb[3] = as.numeric(as.character(patient$SURVIVAL_3))
  actualProb[4] = as.numeric(as.character(patient$SURVIVAL_4))
  actualProb[5] = as.numeric(as.character(patient$SURVIVAL_5))
  actualProb[6] = as.numeric(as.character(patient$SURVIVAL_6))
  actualProb[7] = as.numeric(as.character(patient$SURVIVAL_7))
  actualProb[8] = as.numeric(as.character(patient$SURVIVAL_8))
  actualProb[9] = as.numeric(as.character(patient$SURVIVAL_9))
  
  
  for (j in 1:interval){
    if (actualProb[j] == 2){
      actualProb[j] = 1
    }
  }  
  
  for (j in 1:interval){
    bScore[j] = bScore[j] + ((patientProb[j]-actualProb[j])*(patientProb[j]-actualProb[j]))
    N = nrow(test)
    bScore[j] = bScore[j]/N
    
  }  
  
  #brierScore = brierScore + ((prob-actualProb)*(prob-actualProb))
  #N = nrow(test)
  #brierScore = brierScore/N
  #bScore[j] = brierScore
}

#}
bScore
###############################################################################################
