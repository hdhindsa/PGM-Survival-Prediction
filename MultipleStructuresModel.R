### General Comments ######################################################################################################################
# Dataset used: NACD dataset which can be found at http://pssp.srv.ualberta.ca/predictors/32
#This file is used to learn the sructure and parameters of a PGM. Then it draws a diagram of the structure and then 
#shows an example plot of the survival curve of a individual patient from the dataset.
#It then calculates the brier score of the learnt model 
#The model here learns different models with one survival node for each model and then enforces an arc between all survival nodes combining the models into one model. 
##############################################################
#Structure 4#####################################


#install.packages("bnlearn")
library(bnlearn)
#install.packages("dplyr")
library(dplyr)
##install.packages("arules")
library(arules)
#install.packages("gplots")
library(gplots)
#install.packages("BiocManager")
#BiocManager::install("Rgraphviz")
library(Rgraphviz)
#install.packages("plyr")
library(plyr)
df <- read.csv("NACD.csv", header = TRUE)


#split data into train and test
data1 = sample(nrow(df), nrow(df)*.7)

train<-df[data1,]

test<-df[-data1,]

#typeof(train)
#is.data.frame(train)
#train
df<-train
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
############################################################
#how to bin


#sort df1 by survival
#number of bin = round(sqrt(nrow(df1)))
#intitialize i = 0
#in a for loop i in 1:bin
#beginInterval[i] = i
#endinterval[i] = i+ bin

###############################################################


finalL <- vector("list", interval)

for (j in 1:interval){
  ls = c()
  df1 = df
  #print("iter")
  #print(j)
  #print("beginInterval = ")
  #print(beginInterval)
  #print("endInterval = ")
  #print(endInterval)
  for (i in 1:nrow(df)){
    
    if ((df$SURVIVAL[i] <= endInterval) && (df$CENSORED[i] == 0) && (df$SURVIVAL[i] > beginInterval)){
      ls = c(ls,i)
      df1$SURVIVAL[i] = 1
      
    }
    #if ((df$SURVIVAL[i] > endInterval) && (df$CENSORED[i] == 1)){
    #ls = c(ls,i)
    # df1$SURVIVAL[i] = 0
    #}
    if ((df$SURVIVAL[i] > endInterval)){
      ls = c(ls,i)
      df1$SURVIVAL[i] = 0
    }
    if ((df$SURVIVAL[i] < endInterval) && (df$CENSORED[i] == 0) && (df$SURVIVAL[i] < beginInterval)){
      ls = c(ls,i)
      df1$SURVIVAL[i] = 2
      
    }
    
  }
  df1 = df1[ls,]
  finalL[[j]] <- df1
  
  beginInterval = endInterval
  endInterval = endInterval+bin
}
#df


#length(ls)
#df1 = df1[ls,]
#df1

#######################################################################


#discretize the categorical variables############################

#lol = finalL[[9]]


#######################################################################


#discretize the categorical variables############################

#lol = finalL[[9]]


#df <- read.csv("NACD.csv", header = TRUE)

bnList <- vector("list", interval)
for (j in 1:interval){
  df2 <- finalL[[j]]
  df2[] <- lapply(df2, as.factor)
  numCol = ncol(df2)
  f = 0
  for (i in 1:numCol){
    levLen = length(sapply(df2[i], levels))
    if (levLen > 15){
      #print(as.character(sapply(df2[, i], names)))
      z = as.matrix(as.data.frame(lapply(df2[i], as.numeric)))
      df2[, i] <- discretize(z, method = "cluster" ,breaks = 10, labels = c(1,2,3,4,5,6,7,8,9,10))
      #levels(df2$BMI) <- c(1,2,3,4,5,6,7,8,9,10)
      
    }
    
  }
  #levels(df2$BMI) <- c(1,2,3,4,5,6,7,8,9,10)
  dfnew2 <- df2[ -c(2) ]
  
  #df2[] = lapply(df2, as.factor)
  
  dfnew2[] = lapply(dfnew2, as.factor)
  finalL[[j]] = dfnew2
  bnList[[j]] = hc(dfnew2)
}


#finalL[[9]]$SURVIVAL








#end of imp shit###############################################################


#########################################################################

#plot the graph structure#####################################

graphviz.plot(bnList[[1]], shape = "ellipse")



##############################################################################################

##############################################################################################

#parameter learning#############################################


TrainedbnList <- vector("list", interval)
for(i in 1:interval){
  #for the entire data
  TrainedbnList[[i]] = bn.fit(bnList[[i]], finalL[[i]], method = "bayes")
  
  #for a single patient
  #TrainedbnList[[i]] = bn.fit(bnList[[i]], singlePatient)
}







################################################################################################
###########################################################################################################
# plotting distribution works
patient = finalL[[3]][4,]


patientProb = 1:interval


for (i in 1:interval){
  patientProb[i] = cpquery(TrainedbnList[[i]], (SURVIVAL == 1 | SURVIVAL == 2),  evidence = as.list(patient[1, -1]) , method = "lw" )
}
patientProb
#TrainedbnList[[3]]

timeInter = 1:interval

plot(timeInter,patientProb, type="l", col="green", lwd=5, xlab="Time", ylab="Probability of Death", main="Individual Survival")

######################################################################################################
#trying brier score

###########################################################################################################
#works


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

finalTest <- vector("list", interval)

for (j in 1:interval){
  ls = c()
  df1 = df
  
  for (i in 1:nrow(df)){
    
    if ((df$SURVIVAL[i] <= endInterval) && (df$CENSORED[i] == 0) && (df$SURVIVAL[i] > beginInterval)){
      ls = c(ls,i)
      df1$SURVIVAL[i] = 1
      
    }
    
    if ((df$SURVIVAL[i] > endInterval)){
      ls = c(ls,i)
      df1$SURVIVAL[i] = 0
    }
    if ((df$SURVIVAL[i] < endInterval) && (df$CENSORED[i] == 0) && (df$SURVIVAL[i] < beginInterval)){
      ls = c(ls,i)
      df1$SURVIVAL[i] = 2
      
    }
    
  }
  df1 = df1[ls,]
  finalTest[[j]] <- df1
  
  beginInterval = endInterval
  endInterval = endInterval+bin
}



#finalTest[[1]]
#df2 <- finalTest[[1]]
#df2[] <- lapply(df2, as.factor)
#length(sapply(df2[6], levels))
#df2[6]
#finalTest[[1]][6]
#bnList <- vector("list", interval)




#discret

for (j in 1:interval){
  df2 <- finalTest[[j]]
  df2[] <- lapply(df2, as.factor)
  numCol = ncol(df2)
  f = 0
  for (i in 1:numCol){
    levLen = length(sapply(df2[i], levels))
    if (levLen > 15){
      #print(as.character(sapply(df2[, i], names)))
      z = as.matrix(as.data.frame(lapply(df2[i], as.numeric)))
      df2[, i] <- discretize(z, method = "cluster" ,breaks = 10, labels = c(1,2,3,4,5,6,7,8,9,10))
      #levels(df2$BMI) <- c(1,2,3,4,5,6,7,8,9,10)
    }
    
  }
  #levels(df2$BMI) <- c(1,2,3,4,5,6,7,8,9,10)
  dfnew2 <- df2[ -c(2) ]
  
  #df2[] = lapply(df2, as.factor)
  
  dfnew2[] = lapply(dfnew2, as.factor)
  finalTest[[j]] = dfnew2
  #bnList[[j]] = hc(dfnew2)
}




#finalTest[[1]]

#bscore for all time points together

brierScore = 0

for(j in 1:interval){
  
  for (i in 1:nrow(finalTest[[j]])){
    
    patient = finalTest[[j]][i,]
    prob = cpquery(TrainedbnList[[j]], (SURVIVAL == 1 | SURVIVAL == 2),  evidence = as.list(patient[1, -1]) , method = "lw" )
    actualProb = as.numeric(as.character(patient$SURVIVAL))
    if (actualProb == 2)
    {actualProb = 1}
    
    
    
    brierScore = brierScore + ((prob-actualProb)*(prob-actualProb))
  }
  
}


N = interval*nrow(test)
brierScore = brierScore/N
brierScore
################################################################################################################
#bscore for separate time points

bScore <- c()
for(j in 1:interval){
  brierScore = 0
  for (i in 1:nrow(finalTest[[j]])){
    
    patient = finalTest[[j]][i,]
    prob = cpquery(TrainedbnList[[j]], (SURVIVAL == 1 | SURVIVAL == 2),  evidence = as.list(patient[1, -1]) , method = "lw" )
    actualProb = as.numeric(as.character(patient$SURVIVAL))
    if (actualProb == 2)
    {actualProb = 1}
    
    
    
    brierScore = brierScore + ((prob-actualProb)*(prob-actualProb))
    N = nrow(test)
    brierScore = brierScore/N
    bScore[j] = brierScore
  }
  
}
bScore





##################################################################################################



##################################################################################################

################################################################################################

#hc learning when bn prior is structure of previous time point###########################################

### Not implemented properly yet

#compare(bnList[[1]], bnList[[2]], arcs = TRUE)
#arcs(bnList[[1]])

bnList <- vector("list", interval)

wl = c()
for (j in 1:interval){
  df2 <- finalL[[j]]
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
  
  #df2[] = lapply(df2, as.factor)
  
  dfnew2[] = lapply(dfnew2, as.factor)
  finalL[[j]] = dfnew2
  bnList[[j]] = hc(dfnew2, whitelist = wl)
  wl = arcs(bnList[[j]])
}


####################################################################################################
