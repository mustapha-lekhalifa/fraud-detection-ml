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



# Applique le modéle 


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