require(dplyr)
require(pacman)
require(ggplot2)
require(neural)
library(ISLR)
library(caTools)
require(neuralnet)

maxs <- apply(College[,2:18], 2, max)
mins <- apply(College[,2:18], 2, min)

collegeNormalizado <- as.data.frame(scale(College[,2:18],center = mins, scale = maxs - mins))

#Passando de SIM/NÃO Para 1/0
Private = as.numeric(College$Private)-1
data = cbind(Private,collegeNormalizado)

set.seed(101)
split = sample.split(data$Private, SplitRatio = 0.70)

#Dividir baseado em V ou F
treinoFacul = subset(data, split == TRUE)
testeFacul = subset(data, split == FALSE)

novo <- names(collegeNormalizado)

# Concatenar strings
f <- paste(novo,collapse=' + ')
f <- paste('Private ~',f)

# Converter para formula
f <- as.formula(f)

nn <- neuralnet(f,treinoFacul,hidden=c(10,10,10),linear.output=FALSE)

predizerNN  <- compute ( nn , testeFacul [ 2 : 18 ]) 
print (head ( predizerNN$net.result ))

table(testeFacul$Private,predizerNN$net.result)

#Transformar apenas para 1 ou 0
predizerNN$net.result <- sapply(predizerNN$net.result,round,digits=0)

table(testeFacul$Private,predizerNN$net.result)

plot(nn)
