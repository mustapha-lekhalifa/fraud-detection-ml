data <- read.csv("Processed_data_FD.csv")


View(data)



names(x_train)

# Résumé statistique de la variable représentant l'heure normalisée.
summary(x_train$Transaction.Hour.of.Day)

# Affichage des premières valeurs de la variable.
head(x_train$Transaction.Hour.of.Day)

# Recherche des différentes valeurs observées pour l'heure normalisée.
sort(unique(x_train$Transaction.Hour.of.Day))


# Reconstruction de l'heure à partir de la variable normalisée.
# Ici, on suppose que la valeur normalisée est multipliée par 23
# pour retrouver une heure comprise entre 0 et 23.
heure <- x_train$Transaction.Hour.of.Day *23

head(heure)


# Conversion de l'heure en angle pour l'encodage cyclique.
# Le facteur 2*pi permet de représenter un cycle complet.
teta <- (2 * pi * heure)/24

# Calcul du sinus de l'angle : première nouvelle variable cyclique.
hour_sin <- sin(teta)


# Calcul du cosinus de l'angle : deuxième nouvelle variable cyclique.
hour_cos <- cos(teta)


# Comparaison de l'heure reconstruite avec ses deux transformations.
head(data.frame(
  heure = heure,
  hour_sin = hour_sin,
  hour_cos = hour_cos
))

# Vérification des heures reconstruites distinctes.
sort(unique(heure))


# Ajout des variables cycliques à l'ensemble d'entraînement.
# Le sinus et le cosinus permettent de représenter le caractère cyclique
# de l'heure : 23 h et 0 h sont des heures proches dans le cycle.
x_train$hour_sin <- sin(2*pi*(x_train$Transaction.Hour.of.Day *23)/24)
x_train$hour_cos <- cos(2*pi*(x_train$Transaction.Hour.of.Day *23)/24)


# Application des mêmes transformations aux données de test.
# Cela garantit que les deux jeux possèdent les mêmes nouvelles variables.
x_test$hour_sin <- sin(2*pi*(x_test$Transaction.Hour.of.Day *23)/24)
x_test$hour_cos <- cos(2*pi*(x_test$Transaction.Hour.of.Day *23)/24)

# Vérification des dimensions après l'ajout des deux variables.
dim(x_train)
dim(x_test)


# Vérification visuelle de la variable d'origine et des transformations.
head(x_train[, c(
  "Transaction.Hour.of.Day",
  "hour_sin",
  "hour_cos"
)])

# Les valeurs des variables cycliques doivent rester entre -1 et 1.
summary(x_train$hour_sin)
summary(x_train$hour_cos)


# Création d'une version des données sans les variables cycliques.
# Cette version permet de construire le modèle A avec les variables originales.
x_train_original <- x_train[, !names(x_train) %in% c("hour_sin","hour_cos")]

x_test_original <- x_test[, !names(x_test) %in% c("hour_sin","hour_cos")]


# Vérification des dimensions des données originales.
dim(x_train_original)
dim(x_test_original)


# Modèle A : régression logistique utilisant les variables originales.
# Ce modèle sert de référence pour mesurer l'apport du feature engineering.

donnees_train_A <- x_train_original
donnees_train_A$fraude <- y_train


# Vérification des premières variables, des dimensions et de la cible.
names(donnees_train_A)[1:5]
dim(donnees_train_A)
table(donnees_train_A$fraude)


# Ajustement de la régression logistique.
# La variable cible fraude est expliquée par toutes les autres variables.
modele_A <- glm(
  fraude ~ .,
  data = donnees_train_A,
  family = binomial()
)


# Préparation des données de test sans la variable cible.
donnees_test_A <- as.data.frame(x_test_original)

# Calcul de la probabilité prédite d'appartenir à la classe fraude.
proba_A <- predict(
  modele_A,
  newdata = donnees_test_A,
  type = "response"
)

# Inspection des probabilités prédites.
head(proba_A)

summary(proba_A)


# Conversion des probabilités en prédictions binaires.
# Une transaction est prédite frauduleuse si sa probabilité est >= 0,5.
pred_A <- ifelse (proba_A >= 0.5,1,0)


# Matrice de confusion du modèle A.
# Elle compare les prédictions aux vraies valeurs de y_test.
table(
  prediction = pred_A,
  Reel = y_test
)


# Extraction des effectifs de la matrice de confusion.
# TN : non-fraudes correctement prédites.
# FN : fraudes prédites à tort comme non-fraudes.
# FP : non-fraudes prédites à tort comme fraudes.
# TP : fraudes correctement détectées.
TN <- 284015
FN <- 13243
FP <- 291
TP <- 1769


# Précision : parmi les transactions prédites frauduleuses,
# quelle proportion est réellement frauduleuse ?
precision_A <- TP / (TP + FP)

# Rappel : parmi toutes les fraudes réelles, quelle proportion est détectée ?
recall_A <- TP / (TP + FN)

# F1-score : moyenne harmonique de la précision et du rappel.
f1_A <- 2 * precision_A * recall_A /(precision_A + recall_A)

# Accuracy : proportion totale de prédictions correctes.
accuracy_A <- (TP + TN) / (TP + TN + FP + FN)


# Affichage des indicateurs en pourcentage, arrondis à deux décimales.
round(c(
  Precision = precision_A * 100,
  Recall = recall_A * 100,
  F1_score = f1_A * 100,
  Accuracy = accuracy_A * 100
), 2)


# Modèle B : modèle A avec les variables cycliques de l'heure.
# L'objectif est de vérifier si cette transformation améliore la détection.

donnees_train_B <- data.frame(x_train)

donnees_train_B$fraude <- as.integer(y_train)

modele_B <- glm(
  fraude ~ .,
  data = donnees_train_B,
  family = binomial()
)


# Calcul des probabilités de fraude sur le même jeu de test que le modèle A.
donnees_test_B <- data.frame(x_test)

proba_B <- predict(
  modele_B,
  newdata = donnees_test_B,
  type = "response"
)

head(proba_B)

# Transformation des probabilités en prédictions binaires au seuil de 0,5.
pred_B <- ifelse(proba_B>=0.5, 1, 0)


# Matrice de confusion du modèle B.
table(
  prediction = pred_B,
  Reel = y_test
)


# Effectifs de la matrice de confusion du modèle B.
TN <- 283985
FN <- 13198
FP <- 321
TP <- 1814


# Calcul des indicateurs de performance du modèle B.
precision_B <- TP / (TP + FP)

recall_B <- TP / (TP + FN)

f1_B <- 2 * precision_B *recall_B / (precision_B + recall_B)

accuracy_B <- (TP + TN)/( TP + TN + FN + FP)


# Affichage des indicateurs du modèle B en pourcentage.
round(c(
  Precission <- precision_B * 100,
  Recall <- recall_B * 100,
  F1_score <- f1_B * 100,
  Accuracy <- accuracy_B * 100
),2)


# Tableau comparatif des modèles A et B.
# Les deux modèles sont évalués sur le même jeu de test.
comparaison <- data.frame(
  Modele = c("A - Sans feature engineering",
             "B - Avec feature engineering"),
  Precision = c(precision_A, precision_B) * 100,
  Rappel = c(recall_A, recall_B) * 100,
  F1_score = c(f1_A, f1_B) * 100,
  Accuracy = c(accuracy_A, accuracy_B) * 100
)

# Arrondi des indicateurs à deux décimales pour faciliter la lecture.
comparaison[, -1] <- round(comparaison[, -1], 2)

comparaison


# Comparaison des modèles A et B.
# Le modèle B utilise hour_sin et hour_cos.
#
# Le rappel passe de 11.78 % à 12.08 %.
# Le F1-score passe de 20.72 % à 21.16 %.
# La précision diminue légèrement : 85.87 % à 84.96 %.
# L'accuracy reste à 95.48 % après arrondi.
#
# Conclusion :
# L'ajout des variables cycliques apporte une amélioration
# modeste du rappel et du F1-score au seuil de 0.5.


# Exploration de l'ancienneté du compte.
# On examine sa distribution et sa moyenne selon la classe cible.
summary(x_train$Account.Age..Days.)

tapply(
  x_train$Account.Age..Days.,
  y_train,
  mean
)


# Création de catégories d'ancienneté à partir de la variable normalisée.
# Les seuils 1/3 et 2/3 divisent l'intervalle normalisé en trois groupes.
# Cette catégorisation est une hypothèse de travail à évaluer empiriquement.
x_train$Account.Age..Days.cat <- cut(x_train$Account.Age..Days., breaks = c(-Inf,1/3,2/3,Inf), labels = c("recent","intermediaire","ancien"))

# La fonction cut permet de transformer une variable numérique
# en une variable catégorielle à partir de seuils définis.


# Vérification du nombre d'observations dans chaque catégorie.
table(x_train$Account.Age..Days.cat)


# Calcul du taux de fraude dans chaque catégorie d'ancienneté.
# Comme la cible vaut 0 ou 1, sa moyenne correspond à la proportion de fraudes.
tapply(
  y_train,
  x_train$Account.Age..Days.cat,
  mean 
)


# Application des mêmes seuils aux données de test.
x_test$Account.Age..Days.cat <- cut(x_test$Account.Age..Days., breaks = c(-Inf, 1/3, 2/3, Inf), labels = c("recent", "intermediaire", "ancien"))

table(x_test$Account.Age..Days.cat)


# Modèle C : modèle B auquel on ajoute la catégorie d'ancienneté du compte.
# L'objectif est de vérifier si cette représentation apporte de l'information
# supplémentaire pour distinguer les transactions frauduleuses.

donnees_train_C <- as.data.frame(x_train)
donnees_train_C$fraude <- as.integer(y_train)

modele_C <- glm(
  fraude ~ .,
  data = donnees_train_C,
  family = binomial()
)


# Prédiction des probabilités de fraude sur le jeu de test.
donnees_test_C <- as.data.frame(x_test)

proba_C <- predict(
  modele_C,
  newdata = donnees_test_C,
  type = "response"
)

head(proba_C)


# Conversion des probabilités en prédictions avec un seuil de 0,5.
pred_C <- ifelse(proba_C >= 0.5, 1, 0)

# Comparaison entre les prédictions et les vraies classes.
# La colonne Prediction correspond aux classes prédites.
# La colonne Reel correspond aux valeurs observées dans y_test.
table(Prediction = pred_C, Reel = y_test)


# Effectifs de la matrice de confusion du modèle C.
TN_C <- 283944
FN_C <- 13107
FP_C <- 362
TP_C <- 1905

# Calcul des indicateurs de performance du modèle C.
precision_C <- TP_C / (TP_C + FP_C)

rappel_C <- TP_C / (TP_C + FN_C)

F1_C <- 2 * precision_C * rappel_C / (precision_C + rappel_C)

accuracy_C <- (TP_C + TN_C) / (TP_C + TN_C + FP_C + FN_C)

# Affichage des indicateurs en pourcentage.
round(c( 
  precision_C * 100,
  rappel_C *100,
  F1_C * 100,
  accuracy_C * 100),2)


# Modèle D : modèle C avec une transformation logarithmique du montant.
# L'objectif est de réduire l'asymétrie éventuelle de la distribution
# et de vérifier si cette représentation améliore la détection des fraudes.

# Exploration de la distribution du montant normalisé.
summary(x_train$Transaction.Amount....)

# Comparaison du montant moyen selon la classe cible.
tapply(
  x_train$Transaction.Amount....,
  y_train,
  mean
)


# Comparaison graphique du montant entre transactions frauduleuses et non frauduleuses.
boxplot(
  x_train$Transaction.Amount.... ~ y_train,
  names = c("Non-fraude", "Fraude"),
  xlab = "Type de transaction",
  ylab = "Montant normalisé",
  main = "Montant selon la présence de fraude"
)


# Proportion d'observations pour lesquelles le montant normalisé est nul.
mean(x_train$Transaction.Amount.... == 0)


# Application de log1p(x), équivalent à log(1 + x).
# Cette transformation peut réduire l'influence des valeurs élevées.
# Elle est appliquée de la même manière aux données d'entraînement et de test.
x_train$Transaction.Amount.log <- log1p(x_train$Transaction.Amount....)

x_test$Transaction.Amount.log <- log1p(x_test$Transaction.Amount....)

# Vérification de la distribution après transformation.
summary(x_train$Transaction.Amount.log)


# Préparation des données d'entraînement du modèle D.
donnees_train_D <- as.data.frame(x_train)
donnees_train_D$fraude <- as.integer(y_train)


# Ajustement de la régression logistique avec les variables disponibles.
modele_D <- glm(
  fraude ~ .,
  data = donnees_train_D,
  family = binomial()
)


# Affichage des coefficients et des diagnostics du modèle.
# Un avertissement de probabilités ajustées à 0 ou 1 a été observé
# lors de cet ajustement ; il faudra examiner ce point avant de conclure
# définitivement sur la fiabilité du modèle.
summary(modele_D)

donnees_test_D <- as.data.frame(x_test)


# Calcul des probabilités de fraude pour le modèle D.
proba_D <- predict(
  modele_D,
  newdata = donnees_test_D,
  type = "response"
)


head(proba_D)

# Prédictions binaires au seuil de 0,5 et matrice de confusion.
pred_D <- ifelse(proba_D >= 0.5, 1, 0)
table(Prediction = pred_D, reel = y_test)


# Effectifs de la matrice de confusion du modèle D.
TN_D <- 283885
FN_D<- 12995
FP_D <- 421
TP_D <- 2017


# Calcul des indicateurs de performance du modèle D.
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


# Exploration de Quantity.Purchased.
# L'objectif est de déterminer si cette variable présente une relation
# visible avec la fraude qui justifierait une transformation supplémentaire.

summary(x_train$Quantity.Purchased)

tapply(
  x_train$Quantity.Purchased,
  y_train,
  mean
)


# Comparaison graphique de la quantité achetée selon la classe cible.
boxplot(x_train$Quantity.Purchased ~ y_train, names =c("Non-fraude", "Fraude"), xlab = "Type de transaction", ylab = "quantité acheter", main="quantité acheté selon le fraude")

# Nombre de valeurs distinctes observées.
length(unique(x_train$Quantity.Purchased))

# Valeurs distinctes de la quantité achetée.
sort(unique(x_train$Quantity.Purchased))

# Taux de fraude pour chaque valeur de quantité achetée.
# Cette analyse descriptive aide à repérer d'éventuelles différences.
tapply(
  y_train,
  x_train$Quantity.Purchased,
  mean
)


# Aucun élément descriptif évident ne justifie ici une catégorisation
# ou une transformation supplémentaire de Quantity.Purchased.
# La variable originale est conservée pour les étapes suivantes.


# Modèle E : modèle D avec les variables cycliques du jour.
# Avant de construire ces variables, on explore la distribution du jour
# et les taux de fraude observés pour les différentes valeurs.

summary(x_train$Transaction.Day)

tapply(
  x_train$Transaction.Day,
  y_train,
  mean
)


# Comparaison graphique du jour de transaction selon la classe cible.
boxplot(x_train$Transaction.Day ~ y_train, names= c("Non fraude", "Fraude"), xlab = "Type de transaction", ylab = "Transaction du jour", main = "Transaction Day")


# Vérification du nombre de valeurs distinctes et de leur répartition.
length(unique(x_train$Transaction.Day))
sort(unique(x_train$Transaction.Day))

# Taux de fraude observé pour chaque valeur du jour normalisé.
tapply(
  y_train,
  x_train$Transaction.Day,
  mean
)


# Reconstruction indicative du jour à partir de la variable normalisée.
# Cette opération suppose une normalisation de type (jour - 1) / 30.
# Cette hypothèse doit être confirmée par la documentation du jeu de données.
head(
  data.frame(
    jour_normalise = sort(unique(x_train$Transaction.Day)) * 30 + 1
  ),
  5
)


# Reconstruction du jour pour les jeux d'entraînement et de test.
jour_train <- (x_train$Transaction.Day) * 30 + 1
jour_test <- (x_test$Transaction.Day) *30 + 1


# Encodage cyclique du jour du mois.
# Le dénominateur 31 représente un cycle de 31 jours.
# Le sinus et le cosinus permettent de représenter la proximité
# entre les jours situés de part et d'autre de la fin du cycle.
x_train$day_sin <- sin(2 * pi * (jour_train - 1)/31)
x_train$day_cos <- cos(2 * pi * (jour_train - 1)/31)

# Application des mêmes transformations au jeu de test.
x_test$day_sin <- sin(2 * pi * (jour_test -1)/31)
x_test$day_cos <- cos(2 * pi * (jour_test -1)/31)


# Vérification des dimensions des nouvelles variables.
dim(x_train$day_sin)
dim(x_train$day_cos)

# Comparaison du jour normalisé avec ses deux transformations cycliques.
head(
  x_train[, c(
    "Transaction.Day",
    "day_sin",
    "day_cos"
  )]
)

# Vérification des valeurs des variables cycliques.
summary(x_train$day_sin)
summary(x_train$day_cos)


# Ajustement du modèle E, qui comprend les variables du modèle D
# ainsi que day_sin et day_cos.
donnees_train_E <- as.data.frame(x_train)
donnees_train_E$fraude <- as.integer(y_train)

modele_E <- glm(
  fraude ~ .,
  data = donnees_train_E,
  family = binomial()
)

# Prédiction des probabilités de fraude sur le jeu de test.
donnees_test_E <- as.data.frame(x_test)

proba_E <- predict(
  modele_E,
  newdata = donnees_test_E,
  type = "response"
)

# Prédictions binaires au seuil de 0,5 et matrice de confusion.
pred_E <- ifelse(proba_E >= 0.5, 1, 0)
table(Prediction=pred_E, Reel = y_test)

# Calcul des indicateurs de performance du modèle E.
precision_E <- 2016 / (2016 + 420)
rappel_E <- 2016 / (2016 + 12996)
f1_E <- 2 * precision_E * rappel_E /
  (precision_E + rappel_E)

round(c(
  Precision = precision_E * 100,
  Rappel = rappel_E * 100,
  F1 = f1_E * 100
),2)


# Comparaison exploratoire : les performances du modèle E sont presque
# identiques à celles du modèle D au seuil de 0,5.
# L'ajout des variables cycliques du jour n'apporte donc pas de gain
# visible sur ces indicateurs dans cette comparaison.
# Cela ne démontre pas que le jour ne contient aucune information,
# mais ne justifie pas de conserver ces transformations pour leur gain
# de performance observé ici.


# Exploration de l'âge du client.
# L'objectif est de déterminer si une catégorisation ou une transformation
# paraît utile au vu des distributions et des taux de fraude.

summary(x_train$Customer.Age..Years.)

# Nombre de valeurs distinctes de l'âge normalisé.
length(unique(x_train$Customer.Age..Years.))

# Comparaison de la moyenne de l'âge normalisé selon la classe cible.
tapply(
  x_train$Customer.Age..Years.,
  y_train,
  mean
)

# Comparaison graphique de l'âge selon la classe cible.
boxplot(x_train$Customer.Age..Years. ~ y_train, names= c("Non fraude", "Fraude"), xlab = "Type de customers", ylab = "Age de client", main="Age de client")


# Inspection des premières valeurs distinctes.
head(sort(unique(x_train$Customer.Age..Years.)),10)

# Exploration d'une reconstruction indicative de l'âge.
# Le facteur 102 est une hypothèse basée sur les valeurs observées ;
# la normalisation exacte doit être confirmée par la documentation.
sort(unique(round(x_train$Customer.Age..Years. * 102)))


# Reconstruction indicative de l'âge en années.
age_reel <- round(x_train$Customer.Age..Years. * 102)


# Création de tranches d'âge pour une comparaison descriptive.
# Les seuils ont été choisis pour explorer des groupes d'âge interprétables.
tranche_age <- cut(
  age_reel,
  breaks = c(-Inf, 25, 40, 60, Inf),
  labels = c("25 ans ou moins", "26-40 ans", "41-60 ans", "Plus de 60 ans")
)

# Calcul du taux de fraude pour chaque tranche d'âge.
taux_fraude_age <- tapply(
  y_train,
  tranche_age,
  mean
)

# Affichage des taux de fraude en pourcentage.
taux_fraude_age * 100

# Les taux observés dans les tranches sont proches du taux global de fraude.
# Cette exploration ne met pas en évidence de gain évident justifiant
# une catégorisation de l'âge pour le moment.
# La variable originale est donc conservée pour la suite.
