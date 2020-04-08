require(dplyr)
require(pacman)
require(ggplot2)
require(neural)
library(ISLR)
library(caTools)
require(neuralnet)

caça <- read.csv("/home/bernardo/Documentos/MD2018/Atividades/Atividade 9/Busters/train.csv", header = TRUE, sep = ",")

caça <- select(caça, -c("id","color"))

particionar <- sample(1:nrow(monstros), 310)

treinoMonstros <- monstros[particionar, ]
testeMonstros <- monstros[-particionar,  ]



set.seed(101)



novo <- names(caça)

# Concatenar strings
f <- paste(novo,collapse=' + ')
f <- paste('type ~',f)

# Converter para formula
f <- as.formula(f)

nn <- neuralnet(f,treinoMonstros,hidden=c(10,10,10),linear.output=FALSE)

predizerNN  <- compute ( nn , testeFacul [ 2 : 18 ]) 
print (head ( predizerNN$net.result ))

table(testeFacul$Private,predizerNN$net.result)

#Transformar apenas para 1 ou 0
predizerNN$net.result <- sapply(predizerNN$net.result,round,digits=0)

table(testeFacul$Private,predizerNN$net.result)

plot(nn)