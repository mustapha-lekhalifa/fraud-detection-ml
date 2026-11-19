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

#--------------------------------------------------------------
# rougrepement des matrique pour les 4 modele sur le seuils 0.5

comparaison_modeles <- data.frame(
  Modele = c(
    "Regression logistique",
    "Arbre de decision",
    "Random Forest",
    "XGBoost"
  ),
  Precision = c(
    82.76, 77.13, 77.33, 81.15
  ),
  Recall = c(
    13.43, 12.58, 15.57, 15.29
  ),
  F1_score = c(
    23.11, 21.64, 25.92, 25.73
  ),
  Accuracy = c(
    95.52, 95.43, 95.54, 95.57
  )
)
#-------------------------------------------------





# amelioration du seuils 
#les fraudes sont rares (environ 5 %). Il est donc possible que le modèle attribue des probabilités inférieures à 50 % à certaines vraies fraudes.


seuils <- c( 0.5, 0.4, 0.3, 0.2, 0.1)

#------------------------------------
#le seuils de Xgboost de 0.4 à 0.1
#-------------------------------------

classe_xgb_04 <- ifelse(proba_xgb >= 0.4, 1, 0)


matrice_xgb_04 <- table(
  reel = y_test,
  prediction = classe_xgb_04
)

TN <- matrice_xgb_04["0","0"]
FP <- matrice_xgb_04["0","1"]
FN <- matrice_xgb_04["1","0"]
TP <- matrice_xgb_04["1","1"]

precision_xgb_04 <- TP/(TP + FP)
recall_xgb_04 <- TP/(TP + FN)
F1_score_xgb_04 <- ( 2 * precision_xgb_04 * recall_xgb_04)/(precision_xgb_04 + recall_xgb_04)
Accuracy_xgb_04 <- (TN + TP)/(TN + TP + FN + FP)


round(c(
  PR_b <- precision_xgb_04 * 100,
  RC_b <- recall_xgb_04 * 100,
  F1_b <- F1_score_xgb_04 * 100,
  AC_b <- Accuracy_xgb_04 * 100
),2)


classe_xgb_03 <- ifelse(proba_xgb >= 0.3, 1, 0)

matrice_xgb_03 <- table(
  reel = y_test,
  prediction = classe_xgb_03
)

TN <- matrice_xgb_03["0","0"]
FP <- matrice_xgb_03["0","1"]
FN <- matrice_xgb_03["1","0"]
TP <- matrice_xgb_03["1","1"]

precision_xgb_03 <- TP/(TP + FP)
recall_xgb_03 <- TP/(TP + FN)
F1_score_xgb_03 <- ( 2 * precision_xgb_03 * recall_xgb_03)/(precision_xgb_03 + recall_xgb_03)
Accuracy_xgb_03 <- (TN + TP)/(TN + TP + FN + FP)


round(c(
  PR_b <- precision_xgb_03 * 100,
  RC_b <- recall_xgb_03 * 100,
  F1_b <- F1_score_xgb_03 * 100,
  AC_b <- Accuracy_xgb_03 * 100
),2)

classe_xgb_02 <- ifelse(proba_xgb >= 0.2, 1, 0)

matrice_xgb_02 <- table(
  reel = y_test,
  prediction = classe_xgb_02
)


TN <- matrice_xgb_02["0","0"]
FP <- matrice_xgb_02["0","1"]
FN <- matrice_xgb_02["1","0"]
TP <- matrice_xgb_02["1","1"]

precision_xgb_02 <- TP/(TP + FP)
recall_xgb_02 <- TP/(TP + FN)
F1_score_xgb_02 <- ( 2 * precision_xgb_02 * recall_xgb_02)/(precision_xgb_02 + recall_xgb_02)
Accuracy_xgb_02 <- (TN + TP)/(TN + TP + FN + FP)


round(c(
  PR_b <- precision_xgb_02 * 100,
  RC_b <- recall_xgb_02 * 100,
  F1_b <- F1_score_xgb_02 * 100,
  AC_b <- Accuracy_xgb_02 * 100
),2)


classe_xgb_01 <- ifelse(proba_xgb >= 0.1, 1, 0)


matrice_xgb_01 <- table(
  reel = y_test,
  prediction = classe_xgb_01
)

TN <- matrice_xgb_01["0","0"]
FP <- matrice_xgb_01["0","1"]
FN <- matrice_xgb_01["1","0"]
TP <- matrice_xgb_01["1","1"]

precision_xgb_01 <- TP/(TP + FP)
recall_xgb_01 <- TP/(TP + FN)
F1_score_xgb_01 <- ( 2 * precision_xgb_01 * recall_xgb_01)/(precision_xgb_01 + recall_xgb_01)
Accuracy_xgb_01 <- (TN + TP)/(TN + TP + FN + FP)


round(c(
  PR_b <- precision_xgb_01 * 100,
  RC_b <- recall_xgb_01 * 100,
  F1_b <- F1_score_xgb_01 * 100,
  AC_b <- Accuracy_xgb_01 * 100
),2)



# Tableau Xgboost sur le suiles de 0.5 à 0.1 et notre choix c'est 0.2

resultats_xgb <- data.frame(
  Seuil = c(0.5, 0.4, 0.3, 0.2, 0.1),
  Precision = c(81.15, 70.85, 46.20, 41.20, 24.73),
  Recall = c(15.29, 17.84, 31.89, 38.16, 52.87),
  F1_score = c(25.73, 28.50, 37.74, 39.62, 33.70),
  Accuracy = c(95.57, 95.51, 94.72, 94.17, 89.56)
)



#------------------------------------
#le seuils de regression logistique initial qui a meilleur F1_score de 0.4 à 0.1
#-------------------------------------


classe_init_04 <- ifelse(probabilite_init >= 0.4, 1, 0)


matrice_init_04 <- table(
  reel = y_test,
  prediction = classe_init_04
)

TN <- matrice_init_04["0","0"]
FP <- matrice_init_04["0","1"]
FN <- matrice_init_04["1","0"]
TP <- matrice_init_04["1","1"]

precision_init_04 <- TP/(TP + FP)
recall_init_04 <- TP/(TP + FN)
F1_score_init_04 <- ( 2 * precision_init_04 * recall_init_04)/(precision_init_04 + recall_init_04)
Accuracy_init_04 <- (TN + TP)/(TN + TP + FN + FP)


round(c(
  PR_b <- precision_init_04 * 100,
  RC_b <- recall_init_04 * 100,
  F1_b <- F1_score_init_04 * 100,
  AC_b <- Accuracy_init_04 * 100
),2)


classe_init_03 <- ifelse( probabilite_init >= 0.3, 1, 0)


matrice_init_03 <- table(
  reel = y_test,
  prediction = classe_init_03
)

TN <- matrice_init_03["0","0"]
FP <- matrice_init_03["0","1"]
FN <- matrice_init_03["1","0"]
TP <- matrice_init_03["1","1"]

precision_init_03 <- TP/(TP + FP)
recall_init_03 <- TP/(TP + FN)
F1_score_init_03 <- ( 2 * precision_init_03 * recall_init_03)/(precision_init_03 + recall_init_03)
Accuracy_init_03 <- (TN + TP)/(TN + TP + FN + FP)


round(c(
  PR_b <- precision_init_03 * 100,
  RC_b <- recall_init_03 * 100,
  F1_b <- F1_score_init_03 * 100,
  AC_b <- Accuracy_init_03 * 100
),2)



classe_init_02 <- ifelse( probabilite_init >= 0.2, 1, 0)


matrice_init_02 <- table(
  reel = y_test,
  prediction = classe_init_02
)

TN <- matrice_init_02["0","0"]
FP <- matrice_init_02["0","1"]
FN <- matrice_init_02["1","0"]
TP <- matrice_init_02["1","1"]

precision_init_02 <- TP/(TP + FP)
recall_init_02 <- TP/(TP + FN)
F1_score_init_02 <- ( 2 * precision_init_02 * recall_init_02)/(precision_init_02 + recall_init_02)
Accuracy_init_02 <- (TN + TP)/(TN + TP + FN + FP)


round(c(
  PR_b <- precision_init_02 * 100,
  RC_b <- recall_init_02 * 100,
  F1_b <- F1_score_init_02 * 100,
  AC_b <- Accuracy_init_02 * 100
),2)



classe_init_01 <- ifelse( probabilite_init >= 0.1, 1, 0)

matrice_init_01 <- table(
  reel = y_test,
  prediction = classe_init_01
)

TN <- matrice_init_01["0","0"]
FP <- matrice_init_01["0","1"]
FN <- matrice_init_01["1","0"]
TP <- matrice_init_01["1","1"]

precision_init_01 <- TP/(TP + FP)
recall_init_01 <- TP/(TP + FN)
F1_score_init_01 <- ( 2 * precision_init_01 * recall_init_01)/(precision_init_01 + recall_init_01)
Accuracy_init_01 <- (TN + TP)/(TN + TP + FN + FP)


round(c(
  PR_b <- precision_init_01 * 100,
  RC_b <- recall_init_01 * 100,
  F1_b <- F1_score_init_01 * 100,
  AC_b <- Accuracy_init_01 * 100
),2)

# Tableau logistique sur le suiles de 0.5 à 0.1 et notre choix c'est 0.2

resultats_log <- data.frame(
  Seuil = c(0.5, 0.4, 0.3, 0.2, 0.1),
  Precision = c(82.76, 76.26, 64.28, 43.81, 23.15),
  Recall = c(13.43, 15.37, 18.43, 28.64, 46.86),
  F1_score = c(23.11, 25.58, 28.64, 34.63, 30.99),
  Accuracy = c(95.52, 95.52, 95.40, 94.58, 89.53)
)

#------------------------------------
#le seuils de random forest
#-------------------------------------


class(donnees_random$fraude)

model_random_prob <- ranger(
  fraude ~ .,
  data = donnees_random,
  num.trees = 100,
  probability = TRUE,
  seed = 123
)

predict_random_prob <- predict(
  model_random_prob,
  data = x_test
)

proba_random <- predict_random_prob$predictions[, "1"]

head(proba_random, 5)

classe_random <- ifelse(proba_random >= 0.5, 1, 0)

matrice_random_05 <- table(
  reel = y_test,
  prediction = classe_random
)

TN <- matrice_random_05["0","0"]
FP <- matrice_random_05["0","1"]
FN <- matrice_random_05["1","0"]
TP <- matrice_random_05["1","1"]

precision_random_05 <- TP/(TP + FP)
recall_random_05 <- TP/(TP + FN)
F1_score_random_05 <- ( 2 * precision_random_05 * recall_random_05)/(precision_random_05 + recall_random_05)
Accuracy_random_05 <- (TN + TP)/(TN + TP + FN + FP)


round(c(
  PR_b <- precision_random_05 * 100,
  RC_b <- recall_random_05 * 100,
  F1_b <- F1_score_random_05 * 100,
  AC_b <- Accuracy_random_05 * 100
),2)





classe_random <- ifelse(proba_random >= 0.4, 1, 0)

matrice_random_04 <- table(
  reel = y_test,
  prediction = classe_random
)

TN <- matrice_random_04["0","0"]
FP <- matrice_random_04["0","1"]
FN <- matrice_random_04["1","0"]
TP <- matrice_random_04["1","1"]

precision_random_04 <- TP/(TP + FP)
recall_random_04 <- TP/(TP + FN)
F1_score_random_04 <- ( 2 * precision_random_04 * recall_random_04)/(precision_random_04 + recall_random_04)
Accuracy_random_04 <- (TN + TP)/(TN + TP + FN + FP)


round(c(
  PR_b <- precision_random_04 * 100,
  RC_b <- recall_random_04 * 100,
  F1_b <- F1_score_random_04 * 100,
  AC_b <- Accuracy_random_04 * 100
),2)


classe_random <- ifelse(proba_random >= 0.3, 1, 0)

matrice_random_03 <- table(
  reel = y_test,
  prediction = classe_random
)

TN <- matrice_random_03["0","0"]
FP <- matrice_random_03["0","1"]
FN <- matrice_random_03["1","0"]
TP <- matrice_random_03["1","1"]

precision_random_03 <- TP/(TP + FP)
recall_random_03 <- TP/(TP + FN)
F1_score_random_03 <- ( 2 * precision_random_03 * recall_random_03)/(precision_random_03 + recall_random_03)
Accuracy_random_03 <- (TN + TP)/(TN + TP + FN + FP)


round(c(
  PR_b <- precision_random_03 * 100,
  RC_b <- recall_random_03 * 100,
  F1_b <- F1_score_random_03 * 100,
  AC_b <- Accuracy_random_03 * 100
),2)



classe_random <- ifelse(proba_random >= 0.2, 1, 0)

matrice_random_02 <- table(
  reel = y_test,
  prediction = classe_random
)

TN <- matrice_random_02["0","0"]
FP <- matrice_random_02["0","1"]
FN <- matrice_random_02["1","0"]
TP <- matrice_random_02["1","1"]

precision_random_02 <- TP/(TP + FP)
recall_random_02 <- TP/(TP + FN)
F1_score_random_02 <- ( 2 * precision_random_02 * recall_random_02)/(precision_random_02 + recall_random_02)
Accuracy_random_02 <- (TN + TP)/(TN + TP + FN + FP)


round(c(
  PR_b <- precision_random_02 * 100,
  RC_b <- recall_random_02 * 100,
  F1_b <- F1_score_random_02 * 100,
  AC_b <- Accuracy_random_02 * 100
),2)




classe_random <- ifelse(proba_random >= 0.1, 1, 0)

matrice_random_01 <- table(
  reel = y_test,
  prediction = classe_random
)

TN <- matrice_random_01["0","0"]
FP <- matrice_random_01["0","1"]
FN <- matrice_random_01["1","0"]
TP <- matrice_random_01["1","1"]

precision_random_01 <- TP/(TP + FP)
recall_random_01 <- TP/(TP + FN)
F1_score_random_01 <- ( 2 * precision_random_01 * recall_random_01)/(precision_random_01 + recall_random_01)
Accuracy_random_01 <- (TN + TP)/(TN + TP + FN + FP)


round(c(
  PR_b <- precision_random_01 * 100,
  RC_b <- recall_random_01 * 100,
  F1_b <- F1_score_random_01 * 100,
  AC_b <- Accuracy_random_01 * 100
),2)




# tableau de random sur les seuils (0.5 à 0.1) et le choix est 0.2

resultats_random <- data.frame(
  Seuil = c(0.5, 0.4, 0.3, 0.2, 0.1),
  Precision = c(79.09, 61.61, 45.93, 37.82, 21.64),
  Recall = c(15.49, 20.38, 31.61, 39.86, 54.46),
  F1_score = c(25.91, 30.63, 37.45, 38.82, 30.97),
  Accuracy = c(95.56, 95.37, 94.70, 93.70, 87.82)
)



#------------------------------------
#le seuils de arbre de decision
#-------------------------------------

model_proba_tree <- predict(
  model_tree,
  newdata = x_test,
  type="prob"
)

proba_tree <- model_proba_tree[, "1"]

classe_tree <- ifelse(proba_tree >= 0.5, 1, 0)

matrice_tree_05 <- table(
  reel = y_test,
  prediction = classe_tree
)

TN <- matrice_tree_05["0","0"]
FP <- matrice_tree_05["0","1"]
FN <- matrice_tree_05["1","0"]
TP <- matrice_tree_05["1","1"]

precision_tree_05 <- TP/(TP + FP)
recall_tree_05 <- TP/(TP + FN)
F1_score_tree_05 <- ( 2 * precision_tree_05 * recall_tree_05)/(precision_tree_05 + recall_tree_05)
Accuracy_tree_05 <- (TN + TP)/(TN + TP + FN + FP)


round(c(
  PR_b <- precision_tree_05 * 100,
  RC_b <- recall_tree_05 * 100,
  F1_b <- F1_score_tree_05 * 100,
  AC_b <- Accuracy_tree_05 * 100
),2)




classe_tree <- ifelse(proba_tree >= 0.4, 1, 0)

matrice_tree_04 <- table(
  reel = y_test,
  prediction = classe_tree
)

TN <- matrice_tree_04["0","0"]
FP <- matrice_tree_04["0","1"]
FN <- matrice_tree_04["1","0"]
TP <- matrice_tree_04["1","1"]

precision_tree_04 <- TP/(TP + FP)
recall_tree_04 <- TP/(TP + FN)
F1_score_tree_04 <- ( 2 * precision_tree_04 * recall_tree_04)/(precision_tree_04 + recall_tree_04)
Accuracy_tree_04 <- (TN + TP)/(TN + TP + FN + FP)


round(c(
  PR_b <- precision_tree_04 * 100,
  RC_b <- recall_tree_04 * 100,
  F1_b <- F1_score_tree_04 * 100,
  AC_b <- Accuracy_tree_04 * 100
),2)




classe_tree <- ifelse(proba_tree >= 0.3, 1, 0)

matrice_tree_03 <- table(
  reel = y_test,
  prediction = classe_tree
)

TN <- matrice_tree_03["0","0"]
FP <- matrice_tree_03["0","1"]
FN <- matrice_tree_03["1","0"]
TP <- matrice_tree_03["1","1"]

precision_tree_03 <- TP/(TP + FP)
recall_tree_03 <- TP/(TP + FN)
F1_score_tree_03 <- ( 2 * precision_tree_03 * recall_tree_03)/(precision_tree_03 + recall_tree_03)
Accuracy_tree_03 <- (TN + TP)/(TN + TP + FN + FP)


round(c(
  PR_b <- precision_tree_03 * 100,
  RC_b <- recall_tree_03 * 100,
  F1_b <- F1_score_tree_03 * 100,
  AC_b <- Accuracy_tree_03 * 100
),2)




classe_tree <- ifelse(proba_tree >= 0.2, 1, 0)

matrice_tree_02 <- table(
  reel = y_test,
  prediction = classe_tree
)

TN <- matrice_tree_02["0","0"]
FP <- matrice_tree_02["0","1"]
FN <- matrice_tree_02["1","0"]
TP <- matrice_tree_02["1","1"]

precision_tree_02 <- TP/(TP + FP)
recall_tree_02 <- TP/(TP + FN)
F1_score_tree_02 <- ( 2 * precision_tree_02 * recall_tree_02)/(precision_tree_02 + recall_tree_02)
Accuracy_tree_02 <- (TN + TP)/(TN + TP + FN + FP)


round(c(
  PR_b <- precision_tree_02 * 100,
  RC_b <- recall_tree_02 * 100,
  F1_b <- F1_score_tree_02 * 100,
  AC_b <- Accuracy_tree_02 * 100
),2)




classe_tree <- ifelse(proba_tree >= 0.1, 1, 0)

matrice_tree_01 <- table(
  reel = y_test,
  prediction = classe_tree
)

TN <- matrice_tree_01["0","0"]
FP <- matrice_tree_01["0","1"]
FN <- matrice_tree_01["1","0"]
TP <- matrice_tree_01["1","1"]

precision_tree_01 <- TP/(TP + FP)
recall_tree_01 <- TP/(TP + FN)
F1_score_tree_01 <- ( 2 * precision_tree_01 * recall_tree_01)/(precision_tree_01 + recall_tree_01)
Accuracy_tree_01 <- (TN + TP)/(TN + TP + FN + FP)


round(c(
  PR_b <- precision_tree_01 * 100,
  RC_b <- recall_tree_01 * 100,
  F1_b <- F1_score_tree_01 * 100,
  AC_b <- Accuracy_tree_01 * 100
),2)



resultats_tree <- data.frame(
  Seuil = c(0.5, 0.4, 0.3, 0.2, 0.1),
  Precision = c(77.13, 77.13, 77.13, 77.13, 77.13),
  Recall = c(12.58, 12.58, 12.58, 12.58, 12.58),
  F1_score = c(21.64, 21.64, 21.64, 21.64, 21.64),
  Accuracy = c(95.43, 95.43, 95.43, 95.43, 95.43)
)


# Comparaison le choix de meilleur modele dans chaque seuils
comparaison_model <- data.frame(
  Model = c(
    "Regression logistique",
    "Arbre de decision",
    "Random Forest",
    "XGBoost"
  ),
  Seuil_optimal = c(0.2, 0.5, 0.2, 0.2),
  Precision = c(43.81, 77.13, 37.82, 41.20),
  Recall = c(28.64, 12.58, 39.86, 38.16),
  F1_score = c(34.63, 21.64, 38.82, 39.62),
  Accuracy = c(94.58, 95.43, 93.70, 94.17)
)


