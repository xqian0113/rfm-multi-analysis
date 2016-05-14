setwd("C:/Users/endai/Desktop/Frette/receipt level segment")

install.packages("MASS")
library("MASS")

customer<-read.csv("Receipt Level Segment.csv",header=TRUE)

#Means and Variances Per Group

printMeanAndSdByGroup <- function(variables,groupvariable)
  {
     # find the names of the variables
     variablenames <- c(names(groupvariable),names(as.data.frame(variables)))
     # within each group, find the mean of each variable
     groupvariable <- groupvariable[,1] # ensures groupvariable is not a list
     means <- aggregate(as.matrix(variables) ~ groupvariable, FUN = mean)
     names(means) <- variablenames
     print(paste("Means:"))
     print(means)
     # within each group, find the standard deviation of each variable:
     sds <- aggregate(as.matrix(variables) ~ groupvariable, FUN = sd)
     names(sds) <- variablenames
     print(paste("Standard deviations:"))
     print(sds)
     # within each group, find the number of samples:
     samplesizes <- aggregate(as.matrix(variables) ~ groupvariable, FUN = length)
     names(samplesizes) <- variablenames
     print(paste("Sample sizes:"))
     print(samplesizes)
  }
  
printMeanAndSdByGroup(customer[16:21],customer[1])

#Between-groups Variances and Within-groups Variance for a Variable

calcWithinGroupsVariance <- function(variable,groupvariable)
  {
     # find out how many values the group variable can take
     groupvariable2 <- as.factor(groupvariable[[1]])
     levels <- levels(groupvariable2)
     numlevels <- length(levels)
     # get the mean and standard deviation for each group:
     numtotal <- 0
     denomtotal <- 0
     for (i in 1:numlevels)
     {
        leveli <- levels[i]
        levelidata <- variable[groupvariable==leveli,]
        levelilength <- length(levelidata)
        # get the standard deviation for group i:
        sdi <- sd(levelidata)
        numi <- (levelilength - 1)*(sdi * sdi)
        denomi <- levelilength
        numtotal <- numtotal + numi
        denomtotal <- denomtotal + denomi
     }
     # calculate the within-groups variance
     Vw <- numtotal / (denomtotal - numlevels)
     return(Vw)
  }

#to calculate the within-groups variance of the variable 16 (LTD.Total.Number.of.Transactions), we type: 
calcWithinGroupsVariance(customer[16],customer[1])

calcBetweenGroupsVariance <- function(variable,groupvariable)
  {
     # find out how many values the group variable can take
     groupvariable2 <- as.factor(groupvariable[[1]])
     levels <- levels(groupvariable2)
     numlevels <- length(levels)
     # calculate the overall grand mean:
     grandmean <- mean(variable)
     # get the mean and standard deviation for each group:
     numtotal <- 0
     denomtotal <- 0
     for (i in 1:numlevels)
     {
        leveli <- levels[i]
        levelidata <- variable[groupvariable==leveli,]
        levelilength <- length(levelidata)
        # get the mean and standard deviation for group i:
        meani <- mean(levelidata)
        sdi <- sd(levelidata)
        numi <- levelilength * ((meani - grandmean)^2)
        denomi <- levelilength
        numtotal <- numtotal + numi
        denomtotal <- denomtotal + denomi
     }
     # calculate the between-groups variance
     Vb <- numtotal / (numlevels - 1)
     Vb <- Vb[[1]]
     return(Vb)
  }
  
# to calculate the between-groups variance for a variable such as V16:
calcBetweenGroupsVariance(customer[16],customer[1])

calcSeparations <- function(variables,groupvariable)
  {
     # find out how many variables we have
     variables <- as.data.frame(variables)
     numvariables <- length(variables)
     # find the variable names
     variablenames <- colnames(variables)
     # calculate the separation for each variable
     for (i in 1:numvariables)
     {
        variablei <- variables[i]
        variablename <- variablenames[i]
        Vw <- calcWithinGroupsVariance(variablei, groupvariable)
        Vb <- calcBetweenGroupsVariance(variablei, groupvariable)
        sep <- Vb/Vw
        print(paste("variable",variablename,"Vw=",Vw,"Vb=",Vb,"separation=",sep))
     }
  }

calcSeparations(customer[16:21],customer[1])  

calcWithinGroupsCovariance <- function(variable1,variable2,groupvariable)
  {
     # find out how many values the group variable can take
     groupvariable2 <- as.factor(groupvariable[[1]])
     levels <- levels(groupvariable2)
     numlevels <- length(levels)
     # get the covariance of variable 1 and variable 2 for each group:
     Covw <- 0
     for (i in 1:numlevels)
     {
        leveli <- levels[i]
        levelidata1 <- variable1[groupvariable==leveli,]
        levelidata2 <- variable2[groupvariable==leveli,]
        mean1 <- mean(levelidata1)
        mean2 <- mean(levelidata2)
        levelilength <- length(levelidata1)
        # get the covariance for this group:
        term1 <- 0
        for (j in 1:levelilength)
        {
           term1 <- term1 + ((levelidata1[j] - mean1)*(levelidata2[j] - mean2))
        }
        Cov_groupi <- term1 # covariance for this group
        Covw <- Covw + Cov_groupi
     }
     totallength <- nrow(variable1)
     Covw <- Covw / (totallength - numlevels)
     return(Covw)
  }

calcWithinGroupsCovariance(customer[16],customer[17],customer[1])

 calcBetweenGroupsCovariance <- function(variable1,variable2,groupvariable)
  {
     # find out how many values the group variable can take
     groupvariable2 <- as.factor(groupvariable[[1]])
     levels <- levels(groupvariable2)
     numlevels <- length(levels)
     # calculate the grand means
     variable1mean <- mean(variable1)
     variable2mean <- mean(variable2)
     # calculate the between-groups covariance
     Covb <- 0
     for (i in 1:numlevels)
     {
        leveli <- levels[i]
        levelidata1 <- variable1[groupvariable==leveli,]
        levelidata2 <- variable2[groupvariable==leveli,]
        mean1 <- mean(levelidata1)
        mean2 <- mean(levelidata2)
        levelilength <- length(levelidata1)
        term1 <- (mean1 - variable1mean)*(mean2 - variable2mean)*(levelilength)
        Covb <- Covb + term1
     }
     Covb <- Covb / (numlevels - 1)
     Covb <- Covb[[1]]
     return(Covb)
  }

calcBetweenGroupsCovariance(customer[16],customer[17],customer[1])

cor.test(customer$LTD.Total.Number.of.Transactions, customer$Transactions.score)

mosthighlycorrelated <- function(mydataframe,numtoreport)
  {
     # find the correlations
     cormatrix <- cor(mydataframe)
     # set the correlations on the diagonal or lower triangle to zero,
     # so they will not be reported as the highest ones:
     diag(cormatrix) <- 0
     cormatrix[lower.tri(cormatrix)] <- 0
     # flatten the matrix into a dataframe for easy sorting
     fm <- as.data.frame(as.table(cormatrix))
     # assign human-friendly names
     names(fm) <- c("First.Variable", "Second.Variable","Correlation")
     # sort and print the top n correlations
     head(fm[order(abs(fm$Correlation),decreasing=T),],n=numtoreport)
  }

mosthighlycorrelated(customer[16:21], 10)  

standardised<- as.data.frame(scale(customer[16:21]))
sapply(standardised,mean)
sapply(standardised,sd)

customer.pca <- prcomp(standardised)   
summary(customer.pca)

screeplot(customer.pca, type="lines")

(customer.pca$sdev)^2 #keep PCAs>1

#loadings for the principal components
customer.pca$rotation[,1]
sum((customer.pca$rotation[,1])^2)

calcpc <- function(variables,loadings)
  {
     # find the number of samples in the data set
     as.data.frame(variables)
     numsamples <- nrow(variables)
     # make a vector to store the component
     pc <- numeric(numsamples)
     # find the number of variables
     numvariables <- length(variables)
     # calculate the value of the component for each sample
     for (i in 1:numsamples)
     {
        valuei <- 0
        for (j in 1:numvariables)
        {
           valueij <- variables[i,j]
           loadingj <- loadings[j]
           valuei <- valuei + (valueij * loadingj)
        }
        pc[i] <- valuei
     }
     return(pc)
  }
  
head(calcpc(standardised, customer.pca$rotation[,1]))
head(customer.pca$x[,1])

customer.pca$rotation[,2]

#Scatterplots of the Principal Components
plot(customer.pca$x[,1],customer.pca$x[,3])
text(customer.pca$x[,1],customer.pca$x[,3], customer$Group, cex=0.7, pos=4, col="red") # add labels
printMeanAndSdByGroup(standardised,customer[1])
