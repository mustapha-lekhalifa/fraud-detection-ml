# Détection de transactions frauduleuses dans le commerce électronique

## Présentation du projet

Ce projet est réalisé dans le cadre du Master 2 **Statistique et Données du Vivant (STDV)** à l'Université de Poitiers.

L'objectif est d'étudier la détection automatique des transactions frauduleuses dans le commerce électronique à partir de caractéristiques transactionnelles, comportementales et contextuelles.

## Problématique

> Peut-on détecter automatiquement les transactions frauduleuses dans le commerce électronique à partir des caractéristiques transactionnelles, comportementales et contextuelles des clients ?

## Données

Le projet utilise le jeu de données **Fraud Detection Data Set**, disponible sur Kaggle.

Le jeu de données contient environ **1,49 million de transactions** et **21 variables**.

La variable cible est :

- `Fraudulent Transaction` : 0 pour une transaction légitime et 1 pour une transaction frauduleuse.

Les variables décrivent notamment :

- le montant et la quantité de la transaction ;
- l'âge du client et l'ancienneté du compte ;
- l'heure et le jour de la transaction ;
- la catégorie du produit ;
- le moyen de paiement ;
- le type d'appareil utilisé ;
- la concordance entre l'adresse de livraison et l'adresse de facturation ;
- la fréquence associée à la localisation.

## Objectifs

Le projet vise à :

1. Explorer et comprendre les données ;
2. Identifier les caractéristiques associées aux transactions frauduleuses ;
3. Préparer les données pour la modélisation ;
4. Construire des modèles de classification binaire ;
5. Comparer leurs performances ;
6. Identifier les variables importantes pour la détection de la fraude ;
7. Interpréter les résultats et leurs limites.

## Méthodologie

Le projet suivra les principales étapes suivantes :

```text
Compréhension des données
        ↓
Exploration des données (EDA)
        ↓
Prétraitement
        ↓
Feature engineering
        ↓
Modélisation
        ↓
Évaluation
        ↓
Interprétation
        ↓
Rapport et présentation

projet_machine/
├── data/
│   ├── raw/
│   └── processed/
├── notebooks/
├── R/
├── results/
│   ├── figures/
│   └── tables/
├── report/
├── .gitignore
└── README.md




