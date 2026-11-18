data <- read.csv("Processed_data_FD.csv")

dim(x_train)
length(y_train)

dim(x_test)
length(y_test)

names(x_train)
names(x_test)

str(y_train)
table(y_train)

#----------------------
# Modele 1 : Logistique

#-------------------------------------------
# modelisation avec tous les autre variables "Meilleur performance"
donnees_train = data.frame(
  fraude = y_train,
  x_train
)

model = glm(
  fraude ~ .,
  data = donnees_train,
  family = binomial()
)

donnees_test <- data.frame(
  fraude = y_test,
  x_test
)


probabilite_init <- predict(
  model,
  newdata = donnees_test,
  type = "response"
)


head(probabilite_init)

classe_init <- ifelse(probabilite_init >= 0.5, 1, 0)

table(classe_init)
prop.table(classe_init)

length(probabilite_init)
length(classe_init)

matrice_init <- table(
  reel = y_test,
  predict = classe_init
)

TN <- matrice_init["0", "0"]
FP <- matrice_init["0", "1"]
FN <- matrice_init["1", "0"]
TP <- matrice_init["1", "1"]

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

sum(matrice_init)
length(y_test)


summary(model)

#------------------------------------------
# Modelisation sans le transaction amount.... 
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

TN <- matrice_B["0", "0"]
FP <- matrice_B["0", "1"]
FN <- matrice_B["1", "0"]
TP <- matrice_B["1", "1"]


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

sum(matrice_B)
length(y_test)

#--------------------------------------------
# Modele 2 : Arbre de decision - Decision Tree

library(rpart)

donnees_tree <- data.frame(
  fraude = y_train,
  x_train
)


model_tree <- rpart(
  fraude ~ .,
  data = donnees_tree,
  method = "class"
  
)

donnees_tree_teste <- data.frame(
  fraude = y_test,
  x_test
)

classe_tree <- predict(
  model_tree,
  newdata= x_test,
  type = "class"
)

matrice_tree <- table(
  reel = y_test,
  predict = classe_tree
)

TN <- matrice_tree["0","0"]
FP <- matrice_tree["0","1"]
FN <- matrice_tree["1","0"]
TP <- matrice_tree["1","1"]


precision_tree <- TP / (TP + FP)
recall_tree <- TP / (TP + FN)
F1_score_tree <- (2 * precision_tree * recall_tree)/(precision_tree + recall_tree)
accracy_tree <- (TP + TN)/(TP + TN + FP +FN)


round(c(
  PR_t <- precision_tree * 100,
  RC_t <-recall_tree * 100,
  F1_t <- F1_score_tree * 100,
  AC_t <- accracy_tree * 100
),2)


#--------------------------------------------
# Modele 3 : Random Forest


library(ranger)

donnees_random <- data.frame(
  #transforme les classe 0 et 1 en categories
  fraude = as.factor(y_train),
  x_train
)


model_random <- ranger(
  fraude ~ .,
  data = donnees_random,
  #construire l'arbre 
  num.trees = 100,
  
  
)


donnees_random_test <- data.frame(
  fraude = as.factor(y_test),
  x_test
)

prediction_random <- predict(
  model_random,
  data = x_test
)
#recupere les classes predire
classe_random <- prediction_random$predictions


matrice_random <- table(
  reel = y_test,
  predict = classe_random
)

TN <- matrice_random["0","0"]
FP <- matrice_random["0","1"]
FN <- matrice_random["1","0"]
TP <- matrice_random["1","1"]


precision_random <- TP / ( TP + FP)
recall_random <- TP/ (TP + FN)
F1_score_random <- ( 2* precision_random * recall_random)/(precision_random + recall_random)
Accuracy_random <- (TN + TP)/( TP + TN + FP + FN)

round(c(
  PR_r <- precision_random * 100,
  RC_r <- recall_random * 100,
  F1_r <- F1_score_random * 100,
  AC_r <- Accuracy_random * 100
),2)




#--------------------------------------------
# Modele 4 : Grandient Boosting

library(xgboost)

sapply(x_train, class)


# Transforme les variables explicatives en matrice numérique
x_train_matrix <- model.matrix(
  ~ . - 1,
  data = x_train
)

dim(x_train_matrix)

# On fait le meme tranformation pour celui du test 
x_test$Account.Age..Days.cat <- factor(
  x_test$Account.Age..Days.cat,
  levels = levels(x_train$Account.Age..Days.cat)
)

colnames(x_train_matrix)

x_test_matrix <- model.matrix(
  ~ . - 1,
  data = x_test
)

dim(x_test_matrix)


# Entrainement & Test

dtrain <- xgb.DMatrix(
  data = x_train_matrix,
  label = y_train
)

dtest <- xgb.DMatrix(
  data = x_test_matrix,
  label = y_test
)


set.seed(123)

model_xgb <- xgb.train(
  params = list(
    objective = "binary:logistic",     # classification binaire
    eval_metric = "aucpr",             #la performance en tenant compte du déséquilibre
    max_depth = 6,
    eta = 0.1,                       #vitesse d'apprenti
    subsample = 0.8,                 # 80% d'observation pour l'apprenti (les lignes (transactions))
    colsample_bytree = 0.8           # 80% de varibale pour chaque arbre (les colonnes(variables) )
  ), 
  data = dtrain,
  nrounds = 100
)

proba_xgb <- predict(model_xgb , dtest)


head(proba_xgb, 5)

classe_xgb <- ifelse(proba_xgb >= 0.5, 1, 0)

head(classe_xgb)

matrice_xgb <- table(
  reel = y_test,
  predict = classe_xgb
)

TN <- matrice_xgb["0","0"]
FP <- matrice_xgb["0","1"]
FN <- matrice_xgb["1","0"]
TP <- matrice_xgb["1","1"]

precision_xgb <- TP/(TP + FP)
recall_xgb <- TP/(TP + FN)
F1_score_xgb <- ( 2 * precision_xgb * recall_xgb)/(precision_xgb + recall_xgb)
Accuracy_xgb <- (TN + TP)/(TN + TP + FN + FP)


round(c(
  PR_b <- precision_xgb * 100,
  RC_b <- recall_xgb * 100,
  F1_b <- F1_score_xgb * 100,
  AC_b <- Accuracy_xgb * 100
),2)