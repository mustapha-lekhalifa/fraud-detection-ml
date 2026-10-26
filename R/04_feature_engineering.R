data <- read.csv("Processed_data_FD.csv")

View(data)


names(x_train)

summary(x_train$Transaction.Hour.of.Day)

head(x_train$Transaction.Hour.of.Day)

sort(unique(x_train$Transaction.Hour.of.Day))


heure <- x_train$Transaction.Hour.of.Day *23

head(heure)


teta <- (2 * pi * heure)/24

hour_sin <- sin(teta)


hour_cos <- cos(teta)


head(data.frame(
  heure = heure,
  hour_sin = hour_sin,
  hour_cos = hour_cos
))

sort(unique(heure))



x_train$hour_sin <- sin(2*pi*(x_train$Transaction.Hour.of.Day *23)/24)
x_train$hour_cos <- cos(2*pi*(x_train$Transaction.Hour.of.Day *23)/24)


x_test$hour_sin <- sin(2*pi*(x_test$Transaction.Hour.of.Day *23)/24)
x_test$hour_cos <- cos(2*pi*(x_test$Transaction.Hour.of.Day *23)/24)

dim(x_train)
dim(x_test)



head(x_train[, c(
  "Transaction.Hour.of.Day",
  "hour_sin",
  "hour_cos"
)])

summary(x_train$hour_sin)
summary(x_train$hour_cos)



x_train_original <- x_train[, !names(x_train) %in% c("hour_sin","hour_cos")]

x_test_original <- x_test[, !names(x_test) %in% c("hour_sin","hour_cos")]


dim(x_train_original)
dim(x_test_original)



# modèle A variables originales.


donnees_train_A <- x_train_original
donnees_train_A$fraude <- y_train


names(donnees_train_A)[1:5]
dim(donnees_train_A)
table(donnees_train_A$fraude)


modele_A <- glm(
  fraude ~ .,
  data = donnees_train_A,
  family = binomial()
)


donnees_test_A <- as.data.frame(x_test_original)

proba_A <- predict(
  modele_A,
  newdata = donnees_test_A,
  type = "response"
)

head(proba_A)

summary(proba_A)


pred_A <- ifelse (proba_A >= 0.5,1,0)


table(
  prediction = pred_A,
  Reel = y_test
)




# Extraire les nombres de la matrice de confusion
TN <- 284015
FN <- 13243
FP <- 291
TP <- 1769


precision_A <- TP / (TP + FP)

recall_A <- TP / (TP + FN)

f1_A <- 2 * precision_A * recall_A /(precision_A + recall_A)

accuracy_A <- (TP + TN) / (TP + TN + FP + FN)



round(c(
  Precision = precision_A * 100,
  Recall = recall_A * 100,
  F1_score = f1_A * 100,
  Accuracy = accuracy_A * 100
), 2)



# modèle A + transformation cyclique de l'heure

donnees_train_B <- data.frame(x_train)

donnees_train_B$fraude <- as.integer(y_train)

modele_B <- glm(
  fraude ~ .,
  data = donnees_train_B,
  family = binomial()
)



donnees_test_B <- data.frame(x_test)

proba_B <- predict(
  modele_B,
  newdata = donnees_test_B,
  type = "response"
)

head(proba_B)

pred_B <- ifelse(proba_B>=0.5, 1, 0)


# Donc matrice de confusion

table(
  prediction = pred_B,
  Reel = y_test
)



TN <- 283985
FN <- 13198
FP <- 321
TP <- 1814


precision_B <- TP / (TP + FP)

recall_B <- TP / (TP + FN)

f1_B <- 2 * precision_B *recall_B / (precision_B + recall_B)

accuracy_B <- (TP + TN)/( TP + TN + FN + FP)


round(c(
  Precission <- precision_B * 100,
  Recall <- recall_B * 100,
  F1_score <- f1_B * 100,
  Accuracy <- accuracy_B * 100
),2)



comparaison <- data.frame(
  Modele = c("A - Sans feature engineering",
             "B - Avec feature engineering"),
  Precision = c(precision_A, precision_B) * 100,
  Rappel = c(recall_A, recall_B) * 100,
  F1_score = c(f1_A, f1_B) * 100,
  Accuracy = c(accuracy_A, accuracy_B) * 100
)

comparaison[, -1] <- round(comparaison[, -1], 2)

comparaison


# Comparaison des modèles A et B
# Le modèle B utilise hour_sin et hour_cos.

# Le rappel passe de 11.78 % à 12.08 %.
# Le F1-score passe de 20.72 % à 21.16 %.
# La précision diminue légèrement : 85.87 % à 84.96 %.
# L'accuracy reste à 95.48 % après arrondi.

# Conclusion :
# L'ajout des variables cycliques apporte une amélioration
# modeste du rappel et du F1-score au seuil de 0.5




summary(x_train$Account.Age..Days.)

tapply(
  x_train$Account.Age..Days.,
  y_train,
  mean
)

#le seuil au jeu du test
x_train$Account.Age..Days.cat <- cut(x_train$Account.Age..Days., breaks = c(-Inf,1/3,2/3,Inf), labels = c("recent","intermediaire","ancien"))
# la fonction cut ici permet de transformer le 
# variable numérique à une categorie


table(x_train$Account.Age..Days.cat)


tapply(
  y_train,
  x_train$Account.Age..Days.cat,
  mean 
)

x_test$Account.Age..Days.cat <- cut(x_test$Account.Age..Days., breaks = c(-Inf, 1/3, 2/3, Inf), labels = c("recent", "intermediaire", "ancien"))

table(x_test$Account.Age..Days.cat)

# modèle B + catégorisation de l'âge du compte.

donnees_train_C <- as.data.frame(x_train)
donnees_train_C$fraude <- as.integer(y_train)

modele_C <- glm(
  fraude ~ .,
  data = donnees_train_C,
  family = binomial()
)



donnees_test_C <- as.data.frame(x_test)

proba_C <- predict(
  modele_C,
  newdata = donnees_test_C,
  type = "response"
)

head(proba_C)


pred_C <- ifelse(proba_C >= 0.5, 1, 0)

table(Prediction = pred_C, Reel = y_test) 
# prediction ca contient le prediction des autres variables et y_test plutot le cible !



TN_C <- 283944
FN_C <- 13107
FP_C <- 362
TP_C <- 1905

precision_C <- TP_C / (TP_C + FP_C)

rappel_C <- TP_C / (TP_C + FN_C)

F1_C <- 2 * precision_C * rappel_C / (precision_C + rappel_C)

accuracy_C <- (TP_C + TN_C) / (TP_C + TN_C + FP_C + FN_C)

round(c( 
  precision_C * 100,
  rappel_C *100,
  F1_C * 100,
  accuracy_C * 100),2)


# modèle C + transformation du montant de transaction.


summary(x_train$Transaction.Amount....)


tapply(
  x_train$Transaction.Amount....,
  y_train,
  mean
)




boxplot(
  x_train$Transaction.Amount.... ~ y_train,
  names = c("Non-fraude", "Fraude"),
  xlab = "Type de transaction",
  ylab = "Montant normalisé",
  main = "Montant selon la présence de fraude"
)




mean(x_train$Transaction.Amount.... == 0)

x_train$Transaction.Amount.log <- log1p(x_train$Transaction.Amount....)

x_test$Transaction.Amount.log <- log1p(x_test$Transaction.Amount....)

summary(x_train$Transaction.Amount.log)



donnees_train_D <- as.data.frame(x_train)
donnees_train_D$fraude <- as.integer(y_train)


modele_D <- glm(
  fraude ~ .,
  data = donnees_train_D,
  family = binomial()
)


summary(modele_D)

donnees_test_D <- as.data.frame(x_test)


proba_D <- predict(
  modele_D,
  newdata = donnees_test_D,
  type = "response"
)


head(proba_D)

pred_D <- ifelse(proba_D >= 0.5, 1, 0)
table(Prediction = pred_D, reel = y_test)

TN_D <- 283885
FN_D<- 12995
FP_D <- 421
TP_D <- 2017



precision_D <- TP_D / (TP_D + FP_D)
recall_D <- TP_D / ( TP_D + FN_D)
F1_D <- 2 * precision_D * recall_D / ( precision_D + recall_D)
accuracy_D <- (TP_D + TN_D) / (TN_D + FN_D + FP_D + TP_D)

round(c(
  precision_D * 100,
  recall_D * 100,
  F1_D *100,
  accuracy_D * 100
),2)