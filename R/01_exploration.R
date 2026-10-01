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
# ============================================================


# ============================================================
# 1. CHARGEMENT DES DONNÉES
# ============================================================
#
# Objectif :
# Importer le jeu de données CSV dans R afin de pouvoir
# réaliser les différentes analyses.
#
# read.csv() permet de lire un fichier CSV et de le transformer
# en data.frame, ici nommé "data".
# ============================================================

data <- read.csv("data/raw/Processed_data_FD.csv")


# View() permet d'afficher le jeu de données sous forme de
# tableau dans RStudio afin d'observer directement les lignes
# et les colonnes.
#
# Cette commande est surtout utile pour une première vérification
# visuelle des données.
# ============================================================

View(data)



# ============================================================
# 2. DIMENSIONS DU JEU DE DONNÉES
# ============================================================
#
# Objectif :
# Déterminer le nombre d'observations (lignes) et de variables
# (colonnes) présentes dans le jeu de données.
#
# dim() retourne :
# - le nombre de lignes ;
# - le nombre de colonnes.
#
# Ici, cela permet notamment de vérifier que le jeu de données
# contient bien environ 1,5 million de transactions et
# 21 variables.
# ============================================================

dim(data)



# ============================================================
# 3. STRUCTURE ET VARIABLES DU JEU DE DONNÉES
# ============================================================
#
# Objectif :
# Comprendre comment les variables sont stockées dans R et
# identifier leur structure.
#
# str() permet d'obtenir :
# - le nombre d'observations ;
# - le nombre de variables ;
# - le nom de chaque variable ;
# - le type de stockage de chaque variable ;
# - quelques premières valeurs.
#
# Attention :
# le type affiché par R (num, int...) correspond au type de
# stockage informatique et ne suffit pas à déterminer le rôle
# statistique de la variable.
# ============================================================

str(data)



# names() permet d'afficher uniquement les noms des variables.
#
# Cette commande est utile pour :
# - connaître précisément les noms utilisés dans R ;
# - repérer la variable cible ;
# - sélectionner certaines variables pour les analyses suivantes.
# ============================================================

names(data)



# ============================================================
# 4. EXPLORATION DE LA VARIABLE CIBLE
# ============================================================
#
# Objectif :
# Identifier la répartition entre les deux classes de la variable
# cible "Fraudulent Transaction".
#
# La variable cible prend deux valeurs :
# - 0 : transaction légitime ;
# - 1 : transaction frauduleuse.
#
# Le problème étudié est donc un problème de classification
# binaire.
# ============================================================

table(data$Fraudulent.Transaction..Yes.No.)



# prop.table() permet de transformer les effectifs en proportions.
#
# Cette commande permet donc de mesurer la proportion de chaque
# classe dans l'ensemble des transactions.
#
# Elle est particulièrement importante ici pour vérifier si les
# transactions frauduleuses sont minoritaires par rapport aux
# transactions légitimes.
# ============================================================

prop.table(table(data$Fraudulent.Transaction..Yes.No.))



# ============================================================
# 5. QUALITÉ DES DONNÉES
# ============================================================
#
# Objectif :
# Vérifier si le jeu de données contient des problèmes de qualité
# avant de poursuivre l'analyse.
#
# Nous vérifions ici notamment :
# - les valeurs manquantes ;
# - les lignes dupliquées ;
# - les valeurs générales des variables.
# ============================================================


# ------------------------------------------------------------
# 5.1 Valeurs manquantes
# ------------------------------------------------------------
#
# is.na() identifie les valeurs manquantes.
#
# colSums() compte ensuite le nombre de valeurs manquantes
# dans chaque colonne.
#
# Cette vérification permet de savoir si certaines variables
# nécessitent un traitement des données manquantes.
# ============================================================

colSums(is.na(data))



# ------------------------------------------------------------
# 5.2 Lignes dupliquées
# ------------------------------------------------------------
#
# duplicated() identifie les lignes qui sont des duplications
# exactes d'une ligne précédente.
#
# sum() permet ensuite de compter le nombre total de lignes
# dupliquées.
#
# Cette vérification permet de détecter d'éventuels doublons
# exacts dans le jeu de données.
# ============================================================

sum(duplicated(data))



# ------------------------------------------------------------
# 5.3 Résumé statistique général
# ------------------------------------------------------------
#
# summary() fournit un résumé statistique des variables.
#
# Pour les variables numériques, on obtient notamment :
# - minimum ;
# - premier quartile ;
# - médiane ;
# - moyenne ;
# - troisième quartile ;
# - maximum.
#
# Pour les variables binaires, cela permet également d'observer
# leur répartition.
#
# Cette commande donne donc une première vision globale des
# distributions et des valeurs prises par les variables.
# ============================================================

summary(data)



# ============================================================
# 6. VÉRIFICATION DES VARIABLES BINAIRES ET CATÉGORIELLES
# ============================================================
#
# Objectif :
# Vérifier les variables codées sous forme binaire (0/1) et
# contrôler que leurs valeurs sont cohérentes.
#
# Dans ce jeu de données, certaines variables catégorielles ont
# été transformées en plusieurs variables binaires (encodage
# one-hot).
#
# Exemple :
# Product Category est représentée par plusieurs colonnes :
# Clothing, Electronics, Health & Beauty, etc.
# ============================================================


# Liste des variables qui doivent contenir uniquement 0 et 1.
# ============================================================

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



# unique() permet d'identifier les différentes valeurs présentes
# dans chaque variable.
#
# Ici, nous vérifions que les variables binaires contiennent
# uniquement les valeurs 0 et 1 et qu'il n'existe pas de valeur
# inattendue.
# ============================================================

lapply(data[binary_vars], unique)



# ------------------------------------------------------------
# 6.1 Cohérence des catégories de produits
# ------------------------------------------------------------
#
# Les cinq variables suivantes représentent les catégories
# de produits sous forme one-hot.
#
# Pour chaque transaction, une seule catégorie de produit doit
# être sélectionnée.
#
# rowSums() calcule, pour chaque ligne, le nombre de catégories
# ayant la valeur 1.
#
# On utilise ensuite table() pour observer la distribution
# de cette somme.
#
# Si toutes les lignes donnent 1, cela signifie que chaque
# transaction possède exactement une catégorie de produit.
# ============================================================

product_vars <- c(
  "Product.Category...Clothing",
  "Product.Category...Electronics",
  "Product.Category...Health...Beauty",
  "Product.Category...Home...Garden",
  "Product.Category...Toys...Games"
)

table(rowSums(data[product_vars]))



# ------------------------------------------------------------
# 6.2 Cohérence des méthodes de paiement
# ------------------------------------------------------------
#
# Les méthodes de paiement sont également encodées en one-hot.
#
# Chaque transaction doit correspondre à une seule méthode
# de paiement.
#
# On vérifie donc que la somme des variables de paiement vaut
# exactement 1 pour chaque transaction.
# ============================================================

payment_vars <- c(
  "Payment.Method...PayPal",
  "Payment.Method...Bank.Transfer",
  "Payment.Method...Credit.Card",
  "Payment.Method...Debit.Card"
)

table(rowSums(data[payment_vars]))



# ------------------------------------------------------------
# 6.3 Cohérence du type d'appareil
# ------------------------------------------------------------
#
# Les appareils utilisés sont représentés par trois variables :
# Desktop, Mobile et Tablet.
#
# Une transaction doit être associée à un seul type d'appareil.
#
# On vérifie donc que la somme des trois variables vaut 1
# pour chaque transaction.
# ============================================================

device_vars <- c(
  "Device.Used...Desktop",
  "Device.Used...Mobile",
  "Device.Used...Tablet"
)

table(rowSums(data[device_vars]))



# ------------------------------------------------------------
# 6.4 Répartition de Same.ship.and.bill
# ------------------------------------------------------------
#
# Cette variable indique si l'adresse de livraison et l'adresse
# de facturation sont identiques.
#
# - 0 : les adresses sont différentes ;
# - 1 : les adresses sont identiques.
#
# table() donne les effectifs de chaque modalité.
# ============================================================

table(data$Same.ship.and.bill)



# prop.table() donne les proportions correspondantes.
#
# Cela permet de voir quelle proportion des transactions possède
# une adresse de livraison identique à l'adresse de facturation.
# ============================================================

prop.table(table(data$Same.ship.and.bill))



# ------------------------------------------------------------
# 6.5 Répartition des catégories de produits
# ------------------------------------------------------------
#
# colSums() additionne chaque variable one-hot.
#
# Comme chaque colonne représente une catégorie de produit,
# la somme correspond au nombre de transactions appartenant
# à cette catégorie.
# ============================================================

colSums(data[product_vars])



# prop.table() permet ensuite d'obtenir la proportion de chaque
# catégorie dans l'ensemble des transactions.
# ============================================================

prop.table(colSums(data[product_vars]))



# ------------------------------------------------------------
# 6.6 Répartition des méthodes de paiement
# ------------------------------------------------------------
#
# Même principe que précédemment :
# colSums() donne le nombre de transactions pour chaque méthode
# de paiement.
# ============================================================

colSums(data[payment_vars])



# Les proportions permettent de comparer la fréquence des
# différentes méthodes de paiement.
# ============================================================

prop.table(colSums(data[payment_vars]))



# ------------------------------------------------------------
# 6.7 Répartition des appareils utilisés
# ------------------------------------------------------------
#
# colSums() donne le nombre de transactions effectuées depuis
# chaque type d'appareil.
# ============================================================

colSums(data[device_vars])



# prop.table() donne les proportions correspondantes.
# ============================================================

prop.table(colSums(data[device_vars]))



# ============================================================
# 7. EXPLORATION UNIVARIÉE
# ============================================================
#
# Objectif :
# Étudier chaque variable individuellement afin de comprendre
# sa distribution et les valeurs qu'elle prend.
#
# Cette étape permet notamment d'identifier :
# - les variables continues ou discrètes ;
# - les variables concentrées sur certaines valeurs ;
# - les distributions asymétriques ;
# - les distributions relativement homogènes ;
# - les variables possédant peu de niveaux ;
# - d'éventuelles valeurs inhabituelles.
#
# Les variables numériques du jeu de données sont normalisées
# entre 0 et 1. Les valeurs observées ne correspondent donc pas
# directement aux unités originales (euros, années, jours, etc.).
# ============================================================


# ------------------------------------------------------------
# 7.1 Distribution du montant des transactions
# ------------------------------------------------------------
#
# hist() permet de représenter graphiquement la distribution
# du montant des transactions.
#
# Le graphique permet d'observer la concentration des valeurs
# et la forme générale de la distribution.
#
# png() ouvre un fichier image dans lequel le graphique sera
# enregistré.
#
# dev.off() ferme le fichier graphique et finalise son écriture.
# ============================================================

png("results/figures/distribution_transaction_amount.png",
    width = 1200,
    height = 800)

hist(data$Transaction.Amount....,
     main = "Distribution du montant des transactions",
     xlab = "Transaction Amount",
     breaks = 50)

dev.off()



# ------------------------------------------------------------
# 7.2 Distribution de la quantité achetée
# ------------------------------------------------------------
#
# On représente la distribution de Quantity.Purchased afin
# d'observer les valeurs effectivement présentes dans cette
# variable.
# ============================================================

png("results/figures/distribution_quantity_purchased.png",
    width = 1200,
    height = 800)

hist(data$Quantity.Purchased,
     main = "Distribution de la quantité achetée",
     xlab = "Quantity Purchased",
     breaks = 50)

dev.off()



# unique() permet d'identifier les différentes valeurs
# réellement présentes dans Quantity.Purchased.
#
# Cette commande est particulièrement utile ici car une variable
# stockée comme "num" peut néanmoins ne prendre qu'un nombre
# limité de valeurs.
# ============================================================

unique(data$Quantity.Purchased)



# table() permet de compter le nombre d'observations pour
# chacune des valeurs de Quantity.Purchased.
#
# Cela permet de vérifier la fréquence de chaque niveau.
# ============================================================

table(data$Quantity.Purchased)



# ------------------------------------------------------------
# 7.3 Distribution de l'âge des clients
# ------------------------------------------------------------
#
# On représente la distribution de Customer Age afin d'observer
# comment les valeurs sont réparties sur l'intervalle [0,1].
# ============================================================

png("results/figures/distribution_age_client.png",
    width = 1200,
    height = 800)

hist(data$Customer.Age..Years.,
     main = "Distribution de l'âge des clients",
     xlab = "Customer Age",
     breaks = 50)

dev.off()



# ------------------------------------------------------------
# 7.4 Distribution de l'âge des comptes
# ------------------------------------------------------------
#
# L'histogramme permet d'observer la répartition de Account Age
# et d'identifier une éventuelle concentration des valeurs.
# ============================================================

png("results/figures/Distribution de l'âge des comptes.png",
    width = 1200,
    height = 800)

hist(data$Account.Age..Days.,
     main = "Distribution de l'âge des comptes",
     xlab = "Account Age",
     breaks = 50)

dev.off()



# ------------------------------------------------------------
# 7.5 Distribution de l'heure de transaction
# ------------------------------------------------------------
#
# On examine la distribution de Transaction Hour of Day afin
# d'observer comment les transactions sont réparties sur
# l'intervalle étudié.
# ============================================================

png("results/figures/Distribution de l'heure de transaction.png",
    width = 1200,
    height = 800)

hist(data$Transaction.Hour.of.Day,
     main = "Distribution de l'heure de transaction",
     xlab = "Transaction Hour of Day",
     breaks = 50)

dev.off()



# ------------------------------------------------------------
# 7.6 Distribution du jour de transaction
# ------------------------------------------------------------
#
# L'histogramme permet d'observer la répartition de la variable
# Transaction Day.
# ============================================================

png("results/figures/Distribution de jour de transaction.png",
    width = 1200,
    height = 800)

hist(data$Transaction.Day,
     main = "Distribution du jour de transaction",
     xlab = "Transaction Day",
     breaks = 50)

dev.off()



# ------------------------------------------------------------
# 7.7 Distribution de Location_Freq
# ------------------------------------------------------------
#
# On représente la distribution de Location_Freq afin d'observer
# sa concentration et sa forme générale.
# ============================================================

png("results/figures/Distribution de la fréquence de localisation.png",
    width = 1200,
    height = 800)

hist(data$Location_Freq,
     main = "Distribution de la fréquence de localisation",
     xlab = "Location_Freq",
     breaks = 50)

dev.off()


# ============================================================
# PARTIE 8 — EXPLORATION BIVARIÉE
# ============================================================
#
# OBJECTIF :
#
# Après avoir étudié chaque variable individuellement dans la
# partie 7 (exploration univariée), nous allons maintenant
# étudier les relations entre les variables explicatives et
# notre variable cible :
#
# Fraudulent.Transaction..Yes.No.
#
# Cette variable cible indique :
# - 0 : transaction non frauduleuse ;
# - 1 : transaction frauduleuse.
#
# L'objectif est de rechercher d'éventuelles différences entre
# les transactions frauduleuses et non frauduleuses.
#
# Pour les variables catégorielles, nous utiliserons :
# - des tableaux croisés ;
# - des taux de fraude par catégorie ;
# - des graphiques en barres.
#
# Pour les variables numériques, nous utiliserons :
# - des statistiques descriptives ;
# - des boxplots pour comparer les distributions.
#
# IMPORTANT :
# Un taux de fraude est calculé en divisant le nombre de
# transactions frauduleuses d'une catégorie par le nombre total
# de transactions de cette même catégorie.
#
# Cela permet de comparer les catégories équitablement,
# même si leurs effectifs sont différents.
# ============================================================


# ============================================================
# 8.1 — FRAUDE SELON LA CATÉGORIE DE PRODUIT
# ============================================================
#
# OBJECTIF :
#
# Étudier si le taux de fraude varie selon la catégorie
# de produit achetée.
#
# Les catégories de produits sont représentées par des
# variables binaires (one-hot encoding).
#
# Pour chaque catégorie :
# - 1 signifie que la transaction concerne cette catégorie ;
# - 0 signifie qu'elle ne concerne pas cette catégorie.
#
# Nous allons comparer chacune de ces catégories avec
# la variable cible.
# ============================================================


# 1. Identification des variables de catégories de produits

product_vars <- c(
  "Product.Category...Clothing",
  "Product.Category...Electronics",
  "Product.Category...Health...Beauty",
  "Product.Category...Home...Garden",
  "Product.Category...Toys...Games"
)


# 2. Tableaux croisés entre chaque catégorie et la fraude
#
# Pour chaque catégorie, le tableau présente :
# - les transactions non frauduleuses (0) ;
# - les transactions frauduleuses (1).
#
# Les lignes correspondent à l'absence (0) ou à la présence (1)
# de la catégorie.
#
# Ces tableaux permettent de connaître les effectifs,
# mais ne suffisent pas à comparer directement les taux
# de fraude entre les catégories.

for (var in product_vars) {
  
  cat("\nCatégorie :", var, "\n")
  
  print(
    table(
      data[[var]],
      data$Fraudulent.Transaction..Yes.No.
    )
  )
}


# 3. Calcul du taux de fraude pour chaque catégorie
#
# Pour chaque catégorie, nous calculons :
#
# Taux de fraude =
# Nombre de transactions frauduleuses dans la catégorie
# ------------------------------------------------------ x 100
# Nombre total de transactions dans la catégorie
#
# La condition data[[var]] == 1 permet de sélectionner
# uniquement les transactions appartenant à la catégorie.
#
# La condition Fraudulent.Transaction..Yes.No. == 1 permet
# ensuite de compter uniquement les fraudes.

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


# 4. Préparation des noms pour le graphique
#
# Les noms des variables dans le jeu de données sont assez longs.
# Nous les remplaçons par des noms plus lisibles pour faciliter
# l'interprétation du graphique.

product_fraud_rates$Categorie <- c(
  "Clothing",
  "Electronics",
  "Health & Beauty",
  "Home & Garden",
  "Toys & Games"
)


# 5. Représentation graphique des taux de fraude
#
# Chaque barre représente le taux de fraude d'une catégorie.
# La hauteur des barres permet de comparer les catégories.

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


# ============================================================
# 8.2 — FRAUDE SELON LE MOYEN DE PAIEMENT
# ============================================================
#
# OBJECTIF :
#
# Étudier si certains moyens de paiement présentent des taux
# de fraude différents.
#
# Comme pour les catégories de produits, les moyens de paiement
# sont représentés par des variables binaires.
#
# Chaque transaction possède un moyen de paiement parmi les
# catégories étudiées.
# ============================================================


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


# ============================================================
# 8.3 — FRAUDE SELON LE TYPE D'APPAREIL
# ============================================================
#
# OBJECTIF :
#
# Vérifier si le type d'appareil utilisé pour effectuer
# une transaction est associé à des différences de taux
# de fraude.
#
# Trois catégories sont disponibles :
# - Desktop ;
# - Mobile ;
# - Tablet.
#
# Nous allons comparer leurs taux de fraude.
# ============================================================


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


# ============================================================
# 8.4 — FRAUDE SELON LA CORRESPONDANCE DES ADRESSES
# ============================================================
#
# OBJECTIF :
#
# Étudier la relation entre la correspondance de l'adresse
# de livraison et de l'adresse de facturation et la présence
# de fraude.
#
# La variable Same.ship.and.bill est binaire :
# - 0 : les adresses sont différentes ;
# - 1 : les adresses sont identiques.
#
# Nous allons comparer les taux de fraude dans ces deux groupes.
# ============================================================


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


# ============================================================
# 8.5 — FRAUDE SELON LE MONTANT DE LA TRANSACTION
# ============================================================
#
# OBJECTIF :
#
# Contrairement aux variables précédentes, le montant de la
# transaction est une variable numérique.
#
# Nous voulons déterminer si les montants des transactions
# frauduleuses présentent une distribution différente de
# celle des transactions non frauduleuses.
#
# Pour cela, nous allons calculer plusieurs statistiques
# descriptives et représenter les distributions avec un boxplot.
# ============================================================


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


# ============================================================
# 8.6 — FRAUDE SELON LA QUANTITÉ ACHETÉE
# ============================================================
#
# OBJECTIF :
#
# Étudier si le taux de fraude varie selon la quantité achetée.
#
# Bien que Quantity.Purchased soit stockée comme une variable
# numérique, nous avons observé dans la partie univariée
# qu'elle possède seulement cinq valeurs distinctes :
#
# 0 ; 0.25 ; 0.50 ; 0.75 ; 1
#
# Nous pouvons donc comparer directement les taux de fraude
# associés à chacune de ces valeurs.
# ============================================================


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


# ============================================================
# 8.7 — FRAUDE SELON L'ÂGE DU CLIENT
# ============================================================
#
# OBJECTIF :
#
# Comparer l'âge des clients associés aux transactions
# frauduleuses et non frauduleuses.
#
# La variable Customer.Age..Years. est numérique et normalisée.
#
# Nous allons utiliser des statistiques descriptives pour
# comparer les deux groupes, puis un boxplot pour visualiser
# leurs distributions.
# ============================================================


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


# ============================================================
# 8.8 — FRAUDE SELON L'ANCIENNETÉ DU COMPTE
# ============================================================
#
# OBJECTIF :
#
# Vérifier si l'ancienneté du compte présente des différences
# entre les transactions frauduleuses et non frauduleuses.
#
# Cette variable peut être intéressante pour notre problème :
# un compte récemment créé pourrait présenter un comportement
# différent d'un compte utilisé depuis longtemps.
#
# Il s'agit ici d'une piste à explorer, et non d'une conclusion
# sur les causes de la fraude.
# ============================================================


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


# ============================================================
# 8.9 — FRAUDE SELON L'HEURE ET LE JOUR DE TRANSACTION
# ============================================================
#
# OBJECTIF :
#
# Étudier les caractéristiques temporelles des transactions.
#
# Nous allons distinguer deux variables :
# - Transaction.Hour.of.Day : heure de transaction ;
# - Transaction.Day : jour de transaction.
#
# Nous comparerons séparément leurs distributions selon
# la présence ou l'absence de fraude.
# ============================================================


# ------------------------------------------------------------
# 8.9.1 — FRAUDE SELON L'HEURE DE TRANSACTION
# ------------------------------------------------------------
#
# Nous cherchons à observer si les transactions frauduleuses
# sont davantage concentrées sur certaines valeurs de l'heure.
# ============================================================


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


# ------------------------------------------------------------
# 8.9.2 — FRAUDE SELON LE JOUR DE TRANSACTION
# ------------------------------------------------------------
#
# Nous étudions maintenant le jour de transaction.
#
# L'objectif est de vérifier si la distribution de cette
# variable présente des différences selon la présence de fraude.
# ============================================================


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


# ============================================================
# 8.10 — FRAUDE SELON LA FRÉQUENCE DE LOCALISATION
# ============================================================
#
# OBJECTIF :
#
# Étudier la variable Location_Freq selon la présence de fraude.
#
# Nous cherchons à déterminer si la fréquence de localisation
# présente une distribution différente entre les deux groupes.
#
# Nous utiliserons des statistiques descriptives et un boxplot.
# ============================================================


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


# ============================================================
# 8.11 — SYNTHÈSE DE L'EXPLORATION BIVARIÉE
# ============================================================
#
# L'exploration bivariée a permis de comparer chaque variable
# explicative avec la variable cible.
#
# Les résultats descriptifs montrent notamment :
#
# - Des taux de fraude relativement proches selon la catégorie
#   de produit.
# - Des taux de fraude relativement proches selon le moyen
#   de paiement.
# - Des taux de fraude relativement proches selon le type
#   d'appareil.
# - Des taux de fraude proches selon la correspondance
#   des adresses.
# - Des différences de distribution pour le montant
#   des transactions.
# - Des taux de fraude proches selon la quantité achetée.
# - Des distributions d'âge du client relativement proches.
# - Une différence descriptive marquée pour l'ancienneté
#   du compte.
# - Une différence descriptive marquée pour l'heure
#   de transaction.
# - Des distributions proches pour le jour de transaction.
# - Des distributions proches pour la fréquence de localisation.
#
# Ces résultats permettent d'identifier des pistes pour
# la modélisation.
#
# Cependant, une variable qui présente peu de différence
# lorsqu'elle est étudiée seule peut devenir informative
# lorsqu'elle est combinée avec d'autres variables.
#
# Nous ne devons donc pas supprimer automatiquement les
# variables qui semblent peu discriminantes individuellement.
#
# ============================================================


# ============================================================
# PARTIE 9 — SYNTHÈSE GÉNÉRALE ET CONCLUSION DE L'EDA
# ============================================================
#
# OBJECTIF :
#
# Faire le bilan de l'exploration univariée et bivariée.
#
# Cette synthèse permet de conserver les principales
# informations sur le jeu de données et de préparer
# la phase suivante : le preprocessing.
# ============================================================


# ------------------------------------------------------------
# 9.1 — Dimensions du jeu de données
# ------------------------------------------------------------
#
# Nous rappelons le nombre d'observations et de variables
# afin de présenter les dimensions du jeu de données.

cat("\n========== SYNTHÈSE GÉNÉRALE DE L'EDA ==========\n")

cat("\nDimensions du jeu de données :\n")

print(dim(data))


# ------------------------------------------------------------
# 9.2 — Répartition de la variable cible
# ------------------------------------------------------------
#
# Nous rappelons les effectifs et les proportions des deux
# classes afin de mettre en évidence le déséquilibre éventuel.
#
# Cette information sera importante lors de la modélisation
# et de l'évaluation des performances.

cat("\nRépartition de la variable cible :\n")

target_counts <- table(
  data$Fraudulent.Transaction..Yes.No.
)

print(target_counts)


cat("\nProportions des classes en pourcentage :\n")

target_percentages <- prop.table(target_counts) * 100

print(round(target_percentages, 2))


# ------------------------------------------------------------
# 9.3 — Bilan de la qualité des données
# ------------------------------------------------------------
#
# Nous rappelons les vérifications effectuées au cours
# de l'exploration :
# - valeurs manquantes ;
# - doublons exacts.
#
# Ces contrôles permettent de conserver une trace de la
# qualité générale des données avant leur préparation.

cat("\nNombre total de valeurs manquantes :\n")

print(sum(is.na(data)))


cat("\nNombre total de doublons exacts :\n")

print(sum(duplicated(data)))


# ------------------------------------------------------------
# 9.4 — Principales caractéristiques observées
# ------------------------------------------------------------
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
# ------------------------------------------------------------
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
# ------------------------------------------------------------
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


# ============================================================
# CONCLUSION GÉNÉRALE DE L'EXPLORATION DES DONNÉES
# ============================================================
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
#
# ============================================================
# FIN DU SCRIPT 01 — EXPLORATION DES DONNÉES
# ============================================================