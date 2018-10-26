require(dplyr)
require(ggplot2)
require(e1071)
require(caret)

diamante <- sample_n(diamonds, size = 10000)

#separando dados
testeBruto <- subset(diamante, select = -cut)
resultado <- diamante$cut

#criando modelo SVM
modelo_svm <- svm(cut ~ ., data = diamante, kernel = "radial")

#resumo do modelo
summary(modelo_svm)

#verificar a predição
teste1 <- predict(modelo_svm, testeBruto)
#Matriz de confusão, valores na diagonal são os acertados
table(teste1, resultado)

#Analise com detalhes
confusionMatrix(diamante$cut, teste1)

#Mudar os kernels

modelo_svm2 <- svm(cut ~ ., data = diamante, kernel = "polynomial")
teste2 <- predict(modelo_svm2, testeBruto)

#Usando kernel POLYNOMIAL a acuracia foi muito menor que a de radial
confusionMatrix(diamante$cut, teste2)

#modificar mais algumas coisas
modelo_svm3 <- svm(cut ~ ., data = diamante, kernel = "radial", cost = 5)
teste3 <- predict(modelo_svm3, testeBruto)

#Grande melhora, a predição aumentou pouco, porem o KAPPA(indice de medida mais complexo) dobrou de tamanho
confusionMatrix(diamante$cut, teste3)

