# Script pour réaliser toutes les analyses de données / figures du cours
# sur les tests non paramétriques de comparaison de moyenne
#
# Packages ---------------------------------------------------------------------
library("dplyr")
library("ggplot2")


# Test de comparaison de 2 échantillons indépendants ---------------------------
set.seed(20240228)
n1 = 6
n2 = 8
usine1 = round(rnorm(n1, 5, 1.7), 2)
usine2 = round(rnorm(n2, 5, 1), 2)

# Replace dots by commas for showing values
gsub(".", ",", paste0(usine1, collapse = " ; "), fixed = TRUE)
gsub(".", ",", paste0(usine2, collapse = " ; "), fixed = TRUE)

usine2[n2] = 12

shapiro.test(usine1)
shapiro.test(usine2)

full_data = data.frame(
    usine = c(rep("usine1", n1), rep("usine2", n2)),
    principe_actif = c(usine1, usine2)
)

ggplot(full_data, aes(x = usine, y = principe_actif, fill = usine)) +
    geom_boxplot(color = "black") +
    labs(x = "Usine", y = "Poids de principe actif (mg)") +
    scale_fill_manual(
        values = c(usine1 = "darkorchid4", usine2 = "darkorange3")
    ) +
    theme_bw(18) +
    theme(legend.position = "none")

wilcox.test(usine1, usine2)


# Test de comparaison de 2 échantillons appariés -------------------------------
n_personnes = 12
avant_exercice = round(rnorm(n_personnes, 50, sd = 4), 1)
apres_exercice = round(avant_exercice + rnorm(n_personnes, mean = 2, sd = 3), 1)

gsub(".", ",", paste0(avant_exercice, collapse = " ; "), fixed = TRUE)
gsub(".", ",", paste0(apres_exercice, collapse = " ; "), fixed = TRUE)


difference = apres_exercice - avant_exercice
gsub(".", ",", paste0(difference, collapse = " ; "), fixed = TRUE)

wilcox.test(avant_exercice, apres_exercice, paired = TRUE)


# Test de comparaison de plus de 2 échantillons --------------------------------


# Kruskal-Wallis
donnees = data.frame(
    reglage = c(rep("standard", 5), rep("modification_1", 3),
                rep("modification_2", 4)),
    comprimes = c(340, 345, 330, 342, 338, 339, 333, 344, 347, 375, 380, 360)
)

boxplot(comprimes ~ reglage, data = donnees)

ks = kruskal.test(donnees$comprimes, donnees$reglage)

pairwise.wilcox.test(donnees$comprimes, donnees$reglage)

# Friedman
donnees = data.frame(
    patient = rep(paste0("patient", 1:10), each = 3),
    medicament = rep(paste0("medicament", 1:3), 10),
    temps = c(4, 5, 2,  6, 6, 4,  3, 8, 4,  4, 7, 3,  3, 7, 2,  2, 8, 2,
              2, 4, 1,  7, 6, 4,  6, 4, 3,  5, 5, 2)
)

donnees |>
    ggplot(aes(medicament, temps, group = patient)) +
    geom_point(alpha = 1/2) +
    geom_line(alpha = 1/2) +
    labs(x = "Médicament", y = "Temps de réaction") +
    theme_bw()

friedman.test(donnees$temps, donnees$medicament, donnees$patient)
