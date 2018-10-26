require(ggplot2)
require(dplyr)
require(pacman)
require(caret)


diamante <- as.data.frame(diamonds)
diamante <- sample_n(diamante, size = 5000)

diamante$price <- as.numeric(diamante$price)
diamante$cut <- as.factor(diamante$cut)
diamante$clarity <- as.factor(diamante$clarity)
diamante$color <- as.factor(diamante$color)


set.seed(107)

septreino <- createDataPartition(y =  diamante$cut , p = 0.80, list = FALSE)

treino <- diamante[ septreino, ]
teste <- diamante[-septreino, ]

nrow(treino)
nrow(teste)

diamantecaret <- train(
  cut ~ .,  
  data = treino ,
  metric = "accuracy",
  method = "pls", 
  preProc = c("center", "scale")
)

testeclasses <- predict(diamantecaret, teste)
table(testeclasses, teste$cut)
confusionMatrix(testeclasses, teste$cut)
