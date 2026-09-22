set.seed(20241206)

# Création du dataframe avec un effet traitement et un effet genre
hypertension_data <- data.frame(
  patient_id = 1:50,
  age = round(runif(50, 45, 75), 0),
  sexe = sample(c("h", "f"), 50, replace = TRUE),
  groupe = sample(c("traitement", "placebo"), 50, replace = TRUE)
)

# Ajout de la pression artérielle avec des effets différenciés
hypertension_data$systolique <- sapply(1:50, function(i) {
  base = runif(1, 120, 160)  # Pression de base
  
  # Effet du traitement A : augmentation significative
  if(hypertension_data$groupe[i] == "traitement") {
    base = base - runif(1, 5, 20)  # Effet hypotenseur du traitement
  }
  
  # Effet du genre
  if(hypertension_data$sexe[i] == "h") {
    base = base + runif(1, 3, 7)  # Hommes généralement plus hypertendus
  }
  
  round(base, 1)
})

# Sauvegarde du fichier
write.table(hypertension_data, "hypertension.txt", row.names = FALSE)

# Affichage des premières lignes et statistiques
print(head(hypertension_data))
print(summary(hypertension_data$systolique))
