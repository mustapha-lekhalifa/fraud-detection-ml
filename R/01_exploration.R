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
# FIN DE LA PARTIE 1 — EXPLORATION UNIVARIÉE
# ============================================================
#
# À ce stade, nous avons :
#
# - chargé et inspecté les données ;
# - vérifié les dimensions et la structure ;
# - identifié la variable cible ;
# - étudié la répartition fraude / non-fraude ;
# - vérifié les valeurs manquantes ;
# - vérifié les doublons exacts ;
# - contrôlé les variables binaires ;
# - vérifié la cohérence des variables one-hot ;
# - étudié la distribution de plusieurs variables ;
# - enregistré les graphiques dans results/figures/.
#
# La prochaine étape sera l'exploration des relations entre
# les variables et la variable cible, notamment entre les
# transactions frauduleuses et non frauduleuses.
# ============================================================