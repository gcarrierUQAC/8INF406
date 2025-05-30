# 8INF406 — Application Shiny : Consommation Énergétique Mondiale

Cette application Shiny permet de visualiser et d’analyser les données mondiales des centrales électriques de la base GPPD (Global Power Plant Database).

## Liens utiles

* **Déploiement Connect Cloud** : [https://connect.posit.cloud/gcarrieruqac](https://connect.posit.cloud/gcarrieruqac)
* **Code source GitHub** : [https://github.com/gcarrierUQAC/8INF406/tree/main](https://github.com/gcarrierUQAC/8INF406/tree/main)

## Structure du dépôt

```
8INF406/
├── app.R              # Point d’entrée de l’application
├── global.R           # Chargement et nettoyage des données
├── R/                 # Modules de visualisation
│   ├── bubble_chart.R
│   ├── heatmap.R
│   ├── hist.R
│   └── bubble_map1.R
├── manifest.json      # Liste des dépendances et version de R pour Connect Cloud
└── README.md          # Ce fichier
```

## Installation locale

1. **Cloner le dépôt :**

   ```bash
   git clone https://github.com/gcarrierUQAC/8INF406.git
   cd 8INF406
   ```

2. **Installer les dépendances :**

   ```r
   # Depuis R ou RStudio
   install.packages("jsonlite")

   # Installer tous les packages listés dans manifest.json
   manifest <- jsonlite::fromJSON("manifest.json")
   install.packages(names(manifest$packages))
   ```

3. **Lancer l’application :**

   ```r
   # Depuis le dossier du projet
   shiny::runApp()
   ```

   ou cliquez sur **Run App** dans RStudio.

## Déploiement sur Connect Cloud

Connect Cloud utilise le fichier `manifest.json` pour installer la bonne version de R et les packages requis. Pour publier :

1. Générez (si nécessaire) ou mettez à jour votre manifest :

   ```r
   install.packages("rsconnect")
   rsconnect::writeManifest()
   ```
2. **Commit** et **push** sur GitHub :

   ```bash
   git add manifest.json
   git commit -m "Mettre à jour manifest pour Connect Cloud"
   git push
   ```
3. Sur [https://connect.posit.cloud/gcarrieruqac](https://connect.posit.cloud/gcarrieruqac), créez ou mettez à jour votre projet Git en pointant vers ce dépôt.

Connect Cloud détectera automatiquement `manifest.json` et préparera l’environnement sans renv.
