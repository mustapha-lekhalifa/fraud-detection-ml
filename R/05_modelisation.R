data <- read.csv("Processed_data_FD.csv")

dim(x_train)
length(y_train)

dim(x_test)
length(y_test)

names(x_train)
names(x_test)

str(y_train)
table(y_train)

donnees_train = data.frame(
  fraude = y_train,
  x_train
)

model = glm(
  fraude ~ .,
  data = donnees_train,
  family = binomial()
)

summary(model)
model$converged

# Vérifier les coefficients non estimés
sum(is.na(coef(model)))

# Observer les probabilités estimées sur l'entraînement
range(fitted(model))



# Vérifier les valeurs extrêmes des probabilités
min(fitted(model))
max(fitted(model))

#on compte le nombre de proba proche de 1
sum(fitted(model)> 0.999999)

# Examiner la relation entre le montant initial
# et sa transformation logarithmique

cor(
  donnees_train$Transaction.Amount....,
  donnees_train$Transaction.Amount.log
)

donnees_train_B <- donnees_train
donnees_train_B$Transaction.Amount.... <- NULL

model_B <- glm(
  fraude ~ .,
  data = donnees_train_B,
  family = binomial()
)

model_B$converged
range(fitted(model_B))
sum(fitted(model_B)> 0.999999)


summary(model_B)


données_test_B <- data.frame(
  fraude = y_test,
  x_test
)

données_test_B$Transaction.Amount.... <- NULL

dim(donnees_train_B)
dim(données_test_B)

probabilite_B <- predict(
  model_B,
  newdata = données_test_B,
  type = "response"
)

head(probabilite_B)
length(probabilite_B)
summary(probabilite_B)

classe_B <- ifelse(probabilite_B >= 0.5, 1, 0)

table(classe_B)
prop.table(table(classe_B))

matrice_B <- table(
  reel = y_test,
  predict = classe_B
)

TN <- 283953
FN <- 13122
FP <- 352
TP <- 1890


precision <- TP / (TP +FP)
Recall <- TP / ( TP + FN)
F1_score <- (2 * precision * Recall)/(precision + Recall)
Accuracy <- (TP + TN)/(TP + TN + FN + FP)

round(c(
  PR <- precision * 100,
  RC <- Recall * 100,
  F1 <- F1_score * 100,
  ACC <- Accuracy * 100
),2)