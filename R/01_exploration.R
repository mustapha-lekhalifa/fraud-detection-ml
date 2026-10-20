# ============================================================
# PARTIE 1 — EXPLORATION DES DONNÉES
# ============================================================
#
# Objectif :
# Comprendre la structure, la qualité et les principales
# caractéristiques du jeu de données avant de passer à la
# préparation des données et à la modélisation.
#
# L'exploration permet notamment de vérifier :
# - le nombre d'observations et de variables ;
# - la structure et le type des variables ;
# - la variable cible et sa répartition ;
# - la qualité des données ;
# - les valeurs possibles des variables binaires ;
# - la cohérence des variables catégorielles encodées ;
# - la distribution des variables numériques.



data <- read.csv("data/raw/Processed_data_FD.csv")



View(data)





dim(data)





str(data)




names(data)




table(data$Fraudulent.Transaction..Yes.No.)




prop.table(table(data$Fraudulent.Transaction..Yes.No.))








colSums(is.na(data))




sum(duplicated(data))





summary(data)


binary_vars <- c(
  "Fraudulent.Transaction..Yes.No.",
  "Product.Category...Clothing",
  "Product.Category...Electronics",
  "Product.Category...Health...Beauty",
  "Product.Category...Home...Garden",
  "Product.Category...Toys...Games",
  "Payment.Method...PayPal",
  "Payment.Method...Bank.Transfer",
  "Payment.Method...Credit.Card",
  "Payment.Method...Debit.Card",
  "Device.Used...Desktop",
  "Device.Used...Mobile",
  "Device.Used...Tablet",
  "Same.ship.and.bill"
)




lapply(data[binary_vars], unique)




product_vars <- c(
  "Product.Category...Clothing",
  "Product.Category...Electronics",
  "Product.Category...Health...Beauty",
  "Product.Category...Home...Garden",
  "Product.Category...Toys...Games"
)

table(rowSums(data[product_vars]))



payment_vars <- c(
  "Payment.Method...PayPal",
  "Payment.Method...Bank.Transfer",
  "Payment.Method...Credit.Card",
  "Payment.Method...Debit.Card"
)

table(rowSums(data[payment_vars]))





device_vars <- c(
  "Device.Used...Desktop",
  "Device.Used...Mobile",
  "Device.Used...Tablet"
)

table(rowSums(data[device_vars]))





table(data$Same.ship.and.bill)





prop.table(table(data$Same.ship.and.bill))





colSums(data[product_vars])





prop.table(colSums(data[product_vars]))





colSums(data[payment_vars])





prop.table(colSums(data[payment_vars]))



colSums(data[device_vars])





prop.table(colSums(data[device_vars]))




png("results/figures/distribution_transaction_amount.png",
    width = 1200,
    height = 800)

hist(data$Transaction.Amount....,
     main = "Distribution du montant des transactions",
     xlab = "Transaction Amount",
     breaks = 50)

dev.off()





png("results/figures/distribution_quantity_purchased.png",
    width = 1200,
    height = 800)

hist(data$Quantity.Purchased,
     main = "Distribution de la quantité achetée",
     xlab = "Quantity Purchased",
     breaks = 50)

dev.off()





unique(data$Quantity.Purchased)





table(data$Quantity.Purchased)





png("results/figures/distribution_age_client.png",
    width = 1200,
    height = 800)

hist(data$Customer.Age..Years.,
     main = "Distribution de l'âge des clients",
     xlab = "Customer Age",
     breaks = 50)

dev.off()





png("results/figures/Distribution de l'âge des comptes.png",
    width = 1200,
    height = 800)

hist(data$Account.Age..Days.,
     main = "Distribution de l'âge des comptes",
     xlab = "Account Age",
     breaks = 50)

dev.off()





png("results/figures/Distribution de l'heure de transaction.png",
    width = 1200,
    height = 800)

hist(data$Transaction.Hour.of.Day,
     main = "Distribution de l'heure de transaction",
     xlab = "Transaction Hour of Day",
     breaks = 50)

dev.off()





png("results/figures/Distribution de jour de transaction.png",
    width = 1200,
    height = 800)

hist(data$Transaction.Day,
     main = "Distribution du jour de transaction",
     xlab = "Transaction Day",
     breaks = 50)

dev.off()





png("results/figures/Distribution de la fréquence de localisation.png",
    width = 1200,
    height = 800)

hist(data$Location_Freq,
     main = "Distribution de la fréquence de localisation",
     xlab = "Location_Freq",
     breaks = 50)

dev.off()




product_vars <- c(
  "Product.Category...Clothing",
  "Product.Category...Electronics",
  "Product.Category...Health...Beauty",
  "Product.Category...Home...Garden",
  "Product.Category...Toys...Games"
)



for (var in product_vars) {
  
  cat("\nCatégorie :", var, "\n")
  
  print(
    table(
      data[[var]],
      data$Fraudulent.Transaction..Yes.No.
    )
  )
}



product_fraud_rates <- data.frame(
  Categorie = product_vars,
  
  Taux_fraude = sapply(product_vars, function(var) {
    
    # Nombre de transactions frauduleuses dans la catégorie
    nb_fraudes <- sum(
      data[[var]] == 1 &
        data$Fraudulent.Transaction..Yes.No. == 1
    )
    
    # Nombre total de transactions dans la catégorie
    nb_transactions <- sum(data[[var]] == 1)
    
    # Calcul du taux de fraude en pourcentage
    nb_fraudes / nb_transactions * 100
  })
)


# Affichage des taux calculés

print(product_fraud_rates)




product_fraud_rates$Categorie <- c(
  "Clothing",
  "Electronics",
  "Health & Beauty",
  "Home & Garden",
  "Toys & Games"
)




png(
  "results/figures/fraud_rate_by_product_category.png",
  width = 1000,
  height = 700
)

barplot(
  product_fraud_rates$Taux_fraude,
  names.arg = product_fraud_rates$Categorie,
  main = "Taux de fraude selon la catégorie de produit",
  xlab = "Catégorie de produit",
  ylab = "Taux de fraude (%)",
  col = "steelblue",
  ylim = c(0, 6),
  las = 2
)

dev.off()





# 1. Identification des variables correspondant aux moyens
#    de paiement

payment_vars <- c(
  "Payment.Method...PayPal",
  "Payment.Method...Bank.Transfer",
  "Payment.Method...Credit.Card",
  "Payment.Method...Debit.Card"
)


# 2. Tableaux croisés entre chaque moyen de paiement et la fraude
#
# Ces tableaux permettent d'observer le nombre de transactions
# frauduleuses et non frauduleuses pour chaque moyen de paiement.

for (var in payment_vars) {
  
  cat("\nMoyen de paiement :", var, "\n")
  
  print(
    table(
      data[[var]],
      data$Fraudulent.Transaction..Yes.No.
    )
  )
}


# 3. Calcul du taux de fraude pour chaque moyen de paiement
#
# Nous appliquons le même principe que précédemment :
# nombre de fraudes dans le groupe / nombre total de transactions
# utilisant le moyen de paiement concerné.

payment_fraud_rates <- data.frame(
  Moyen_paiement = payment_vars,
  
  Taux_fraude = sapply(payment_vars, function(var) {
    
    # Nombre de transactions frauduleuses utilisant ce moyen
    nb_fraudes <- sum(
      data[[var]] == 1 &
        data$Fraudulent.Transaction..Yes.No. == 1
    )
    
    # Nombre total de transactions utilisant ce moyen
    nb_transactions <- sum(data[[var]] == 1)
    
    # Taux de fraude en pourcentage
    nb_fraudes / nb_transactions * 100
  })
)


# Affichage des résultats

print(payment_fraud_rates)


# 4. Remplacement des noms techniques par des noms lisibles

payment_fraud_rates$Moyen_paiement <- c(
  "PayPal",
  "Bank Transfer",
  "Credit Card",
  "Debit Card"
)


# 5. Graphique des taux de fraude selon le moyen de paiement

png(
  "results/figures/fraud_rate_by_payment_method.png",
  width = 1000,
  height = 700
)

barplot(
  payment_fraud_rates$Taux_fraude,
  names.arg = payment_fraud_rates$Moyen_paiement,
  main = "Taux de fraude selon le moyen de paiement",
  xlab = "Moyen de paiement",
  ylab = "Taux de fraude (%)",
  col = "steelblue",
  ylim = c(0, 6),
  las = 2
)

dev.off()





# 1. Identification des variables correspondant aux appareils

device_vars <- c(
  "Device.Used...Desktop",
  "Device.Used...Mobile",
  "Device.Used...Tablet"
)


# 2. Tableaux croisés entre chaque appareil et la fraude

for (var in device_vars) {
  
  cat("\nType d'appareil :", var, "\n")
  
  print(
    table(
      data[[var]],
      data$Fraudulent.Transaction..Yes.No.
    )
  )
}


# 3. Calcul du taux de fraude pour chaque type d'appareil

device_fraud_rates <- data.frame(
  Type_appareil = device_vars,
  
  Taux_fraude = sapply(device_vars, function(var) {
    
    # Nombre de transactions frauduleuses effectuées
    # depuis cet appareil
    nb_fraudes <- sum(
      data[[var]] == 1 &
        data$Fraudulent.Transaction..Yes.No. == 1
    )
    
    # Nombre total de transactions effectuées
    # depuis cet appareil
    nb_transactions <- sum(data[[var]] == 1)
    
    # Taux de fraude en pourcentage
    nb_fraudes / nb_transactions * 100
  })
)


# Affichage des résultats

print(device_fraud_rates)


# 4. Noms lisibles pour le graphique

device_fraud_rates$Type_appareil <- c(
  "Desktop",
  "Mobile",
  "Tablet"
)


# 5. Graphique des taux de fraude selon le type d'appareil

png(
  "results/figures/fraud_rate_by_device.png",
  width = 1000,
  height = 700
)

barplot(
  device_fraud_rates$Taux_fraude,
  names.arg = device_fraud_rates$Type_appareil,
  main = "Taux de fraude selon le type d'appareil",
  xlab = "Type d'appareil",
  ylab = "Taux de fraude (%)",
  col = "steelblue",
  ylim = c(0, 6),
  las = 2
)

dev.off()





# 1. Tableau croisé entre la correspondance des adresses
#    et la variable cible

table(
  data$Same.ship.and.bill,
  data$Fraudulent.Transaction..Yes.No.
)


# 2. Calcul du taux de fraude selon la correspondance
#
# Pour chaque situation, nous calculons la proportion
# de transactions frauduleuses parmi toutes les transactions
# de la situation concernée.

address_fraud_rates <- data.frame(
  
  Correspondance = c(
    "Adresses différentes",
    "Adresses identiques"
  ),
  
  Taux_fraude = c(
    
    # Taux de fraude lorsque les adresses sont différentes
    sum(
      data$Same.ship.and.bill == 0 &
        data$Fraudulent.Transaction..Yes.No. == 1
    ) /
      sum(data$Same.ship.and.bill == 0) * 100,
    
    # Taux de fraude lorsque les adresses sont identiques
    sum(
      data$Same.ship.and.bill == 1 &
        data$Fraudulent.Transaction..Yes.No. == 1
    ) /
      sum(data$Same.ship.and.bill == 1) * 100
  )
)


# Affichage des résultats

print(address_fraud_rates)


# 3. Graphique du taux de fraude selon la correspondance
#    des adresses

png(
  "results/figures/fraud_rate_by_address_match.png",
  width = 1000,
  height = 700
)

barplot(
  address_fraud_rates$Taux_fraude,
  names.arg = address_fraud_rates$Correspondance,
  main = "Taux de fraude selon la correspondance des adresses",
  xlab = "Correspondance des adresses",
  ylab = "Taux de fraude (%)",
  col = "steelblue",
  ylim = c(0, 6),
  las = 2
)

dev.off()





# 1. Statistiques descriptives du montant selon la fraude
#
# aggregate() permet ici de séparer les observations selon
# la variable cible, puis de calculer les statistiques
# dans chacun des deux groupes.
#
# Nous calculons :
# - la moyenne ;
# - la médiane ;
# - l'écart-type ;
# - le minimum ;
# - le maximum.

amount_by_fraud <- aggregate(
  Transaction.Amount.... ~ Fraudulent.Transaction..Yes.No.,
  data = data,
  
  FUN = function(x) {
    
    c(
      Moyenne = mean(x),
      Mediane = median(x),
      Ecart_type = sd(x),
      Minimum = min(x),
      Maximum = max(x)
    )
  }
)


# Affichage des statistiques descriptives

print(amount_by_fraud)


# 2. Comparaison des distributions avec un boxplot
#
# Le boxplot permet de comparer visuellement :
# - la médiane ;
# - la dispersion des valeurs ;
# - les quartiles ;
# - l'étendue représentée par les moustaches.
#
# Les deux groupes sont représentés séparément :
# 0 = transactions non frauduleuses ;
# 1 = transactions frauduleuses.

png(
  "results/figures/transaction_amount_by_fraud.png",
  width = 1000,
  height = 700
)

boxplot(
  Transaction.Amount.... ~ Fraudulent.Transaction..Yes.No.,
  data = data,
  names = c("Non frauduleuses", "Frauduleuses"),
  main = "Montant des transactions selon la présence de fraude",
  xlab = "Type de transaction",
  ylab = "Montant normalisé",
  col = c("lightblue", "salmon"),
  outline = FALSE
)

dev.off()





# 1. Tableau croisé entre la quantité achetée et la fraude
#
# Les lignes représentent les différentes valeurs de quantité.
# Les colonnes représentent les transactions non frauduleuses
# et frauduleuses.

quantity_table <- table(
  data$Quantity.Purchased,
  data$Fraudulent.Transaction..Yes.No.
)

print(quantity_table)


# 2. Calcul du taux de fraude pour chaque quantité
#
# aggregate() regroupe les observations selon la quantité.
#
# Comme la variable cible prend uniquement les valeurs 0 et 1,
# sa moyenne correspond à la proportion de transactions
# frauduleuses dans chaque groupe.
#
# Nous multiplions ensuite cette moyenne par 100 pour obtenir
# un taux exprimé en pourcentage.

quantity_fraud_rates <- aggregate(
  Fraudulent.Transaction..Yes.No. ~ Quantity.Purchased,
  data = data,
  FUN = mean
)


# Conversion du taux en pourcentage

quantity_fraud_rates$Fraudulent.Transaction..Yes.No. <-
  quantity_fraud_rates$Fraudulent.Transaction..Yes.No. * 100


# Renommage des colonnes pour faciliter la lecture

names(quantity_fraud_rates) <- c(
  "Quantite",
  "Taux_fraude"
)


# Affichage des taux de fraude

print(quantity_fraud_rates)


# 3. Graphique du taux de fraude selon la quantité achetée

png(
  "results/figures/fraud_rate_by_quantity.png",
  width = 1000,
  height = 700
)

barplot(
  quantity_fraud_rates$Taux_fraude,
  names.arg = quantity_fraud_rates$Quantite,
  main = "Taux de fraude selon la quantité achetée",
  xlab = "Quantité achetée (valeur normalisée)",
  ylab = "Taux de fraude (%)",
  col = "steelblue",
  ylim = c(0, 6)
)

dev.off()





# 1. Statistiques descriptives de l'âge selon la fraude

age_by_fraud <- aggregate(
  Customer.Age..Years. ~ Fraudulent.Transaction..Yes.No.,
  data = data,
  
  FUN = function(x) {
    
    c(
      Moyenne = mean(x),
      Mediane = median(x),
      Ecart_type = sd(x),
      Minimum = min(x),
      Maximum = max(x)
    )
  }
)


# Affichage des résultats

print(age_by_fraud)


# 2. Boxplot de l'âge selon la présence de fraude
#
# Ce graphique permet de comparer la position et la dispersion
# de l'âge dans les deux groupes.

png(
  "results/figures/customer_age_by_fraud.png",
  width = 1000,
  height = 700
)

boxplot(
  Customer.Age..Years. ~ Fraudulent.Transaction..Yes.No.,
  data = data,
  names = c("Non frauduleuses", "Frauduleuses"),
  main = "Âge des clients selon la présence de fraude",
  xlab = "Type de transaction",
  ylab = "Âge normalisé",
  col = c("lightblue", "salmon"),
  outline = FALSE
)

dev.off()





# 1. Statistiques descriptives selon la présence de fraude

account_age_by_fraud <- aggregate(
  Account.Age..Days. ~ Fraudulent.Transaction..Yes.No.,
  data = data,
  
  FUN = function(x) {
    
    c(
      Moyenne = mean(x),
      Mediane = median(x),
      Ecart_type = sd(x),
      Minimum = min(x),
      Maximum = max(x)
    )
  }
)


# Affichage des résultats

print(account_age_by_fraud)


# 2. Boxplot de l'ancienneté du compte
#
# La comparaison visuelle des médianes et des boîtes permet
# d'observer si les distributions sont décalées entre les
# deux groupes.

png(
  "results/figures/account_age_by_fraud.png",
  width = 1000,
  height = 700
)

boxplot(
  Account.Age..Days. ~ Fraudulent.Transaction..Yes.No.,
  data = data,
  names = c("Non frauduleuses", "Frauduleuses"),
  main = "Ancienneté du compte selon la présence de fraude",
  xlab = "Type de transaction",
  ylab = "Ancienneté du compte (valeur normalisée)",
  col = c("lightblue", "salmon"),
  outline = FALSE
)

dev.off()








# 1. Statistiques descriptives selon la présence de fraude

hour_by_fraud <- aggregate(
  Transaction.Hour.of.Day ~ Fraudulent.Transaction..Yes.No.,
  data = data,
  
  FUN = function(x) {
    
    c(
      Moyenne = mean(x),
      Mediane = median(x),
      Ecart_type = sd(x),
      Minimum = min(x),
      Maximum = max(x)
    )
  }
)


# Affichage des résultats

print(hour_by_fraud)


# 2. Boxplot de l'heure de transaction

png(
  "results/figures/transaction_hour_by_fraud.png",
  width = 1000,
  height = 700
)

boxplot(
  Transaction.Hour.of.Day ~ Fraudulent.Transaction..Yes.No.,
  data = data,
  names = c("Non frauduleuses", "Frauduleuses"),
  main = "Heure de transaction selon la présence de fraude",
  xlab = "Type de transaction",
  ylab = "Heure normalisée",
  col = c("lightblue", "salmon"),
  outline = FALSE
)

dev.off()




# 1. Statistiques descriptives selon la présence de fraude

day_by_fraud <- aggregate(
  Transaction.Day ~ Fraudulent.Transaction..Yes.No.,
  data = data,
  
  FUN = function(x) {
    
    c(
      Moyenne = mean(x),
      Mediane = median(x),
      Ecart_type = sd(x),
      Minimum = min(x),
      Maximum = max(x)
    )
  }
)


# Affichage des résultats

print(day_by_fraud)


# 2. Boxplot du jour de transaction

png(
  "results/figures/transaction_day_by_fraud.png",
  width = 1000,
  height = 700
)

boxplot(
  Transaction.Day ~ Fraudulent.Transaction..Yes.No.,
  data = data,
  names = c("Non frauduleuses", "Frauduleuses"),
  main = "Jour de transaction selon la présence de fraude",
  xlab = "Type de transaction",
  ylab = "Jour normalisé",
  col = c("lightblue", "salmon"),
  outline = FALSE
)

dev.off()





# 1. Statistiques descriptives selon la présence de fraude

location_by_fraud <- aggregate(
  Location_Freq ~ Fraudulent.Transaction..Yes.No.,
  data = data,
  
  FUN = function(x) {
    
    c(
      Moyenne = mean(x),
      Mediane = median(x),
      Ecart_type = sd(x),
      Minimum = min(x),
      Maximum = max(x)
    )
  }
)


# Affichage des résultats

print(location_by_fraud)


# 2. Boxplot de la fréquence de localisation
#
# Ce graphique permet de comparer la médiane et la dispersion
# de la fréquence de localisation dans les deux groupes.

png(
  "results/figures/location_frequency_by_fraud.png",
  width = 1000,
  height = 700
)

boxplot(
  Location_Freq ~ Fraudulent.Transaction..Yes.No.,
  data = data,
  names = c("Non frauduleuses", "Frauduleuses"),
  main = "Fréquence de localisation selon la présence de fraude",
  xlab = "Type de transaction",
  ylab = "Fréquence de localisation (valeur normalisée)",
  col = c("lightblue", "salmon"),
  outline = FALSE
)

dev.off()



# Nous rappelons le nombre d'observations et de variables
# afin de présenter les dimensions du jeu de données.

cat("\n========== SYNTHÈSE GÉNÉRALE DE L'EDA ==========\n")

cat("\nDimensions du jeu de données :\n")

print(dim(data))




cat("\nRépartition de la variable cible :\n")

target_counts <- table(
  data$Fraudulent.Transaction..Yes.No.
)

print(target_counts)


cat("\nProportions des classes en pourcentage :\n")

target_percentages <- prop.table(target_counts) * 100

print(round(target_percentages, 2))




cat("\nNombre total de valeurs manquantes :\n")

print(sum(is.na(data)))


cat("\nNombre total de doublons exacts :\n")

print(sum(duplicated(data)))


# ------------------------------------------------------------
# 9.4 — Principales caractéristiques observées

#
# Les analyses précédentes ont permis de retenir les éléments
# suivants :
#
# 1. Le jeu de données contient environ 1,5 million
#    de transactions et 21 variables.
#
# 2. La variable cible est binaire :
#    0 pour les transactions non frauduleuses ;
#    1 pour les transactions frauduleuses.
#
# 3. Les transactions frauduleuses représentent environ 5 %
#    de l'ensemble des observations.
#
# 4. Aucune valeur manquante ni aucun doublon exact
#    n'ont été détectés.
#
# 5. Les variables catégorielles sont déjà représentées
#    sous forme de variables binaires (one-hot encoding).
#
# 6. Plusieurs variables numériques sont normalisées
#    entre 0 et 1.
#
# 7. Des différences descriptives ont notamment été
#    observées pour le montant de transaction,
#    l'ancienneté du compte et l'heure de transaction.
#
# Ces observations décrivent le jeu de données.
# Elles ne démontrent pas à elles seules une relation
# causale ou une capacité de prédiction.


# ------------------------------------------------------------
# 9.5 — Conséquences pour la phase de modélisation
#
# Plusieurs points devront être pris en compte pour la suite :
#
# - Vérifier les types et le rôle statistique des variables.
# - Déterminer si des transformations supplémentaires
#   sont nécessaires.
# - Préserver la séparation entre les données d'entraînement
#   et les données de test.
# - Tenir compte du déséquilibre entre les classes.
# - Comparer plusieurs modèles de classification.
#
# Pour évaluer les modèles, nous ne nous limiterons pas
# à l'accuracy.
#
# Nous étudierons notamment :
# - la précision (Precision) ;
# - le rappel (Recall) ;
# - le F1-score ;
# - la ROC-AUC ;
# - la PR-AUC.
#
# Ces indicateurs permettront d'étudier plus précisément
# la capacité des modèles à détecter les transactions
# frauduleuses.


# ------------------------------------------------------------
# 9.6 — Limite liée à la nature du jeu de données

#
# Le jeu de données utilisé est synthétique.
#
# Les résultats obtenus lors de l'exploration et de la
# modélisation devront donc être interprétés dans le cadre
# de ce jeu de données.
#
# Les performances des modèles ne pourront pas être
# directement généralisées à des transactions réelles
# sans validation sur des données réelles indépendantes.



# CONCLUSION GÉNÉRALE DE L'EXPLORATION DES DONNÉES
# 
#
# Cette phase d'exploration nous a permis de comprendre
# la structure du jeu de données, d'examiner sa qualité
# et d'étudier les distributions des variables.
#
# Elle a également permis d'observer les relations descriptives
# entre les variables explicatives et la présence de fraude.
#
# Les résultats obtenus constituent une base pour la prochaine
# phase du projet : la préparation des données (preprocessing),
# suivie de la construction et de l'évaluation des modèles
# de classification.