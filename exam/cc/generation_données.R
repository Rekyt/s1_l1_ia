# Script pour générer les données pour le CC

# Packages ---------------------------------------------------------------------

library("dplyr")
library("ggplot2")


# Fonction ---------------------------------------------------------------------

data_generation = function(experience_type, n_obs, seed, filename) {
	set.seed(seed)
	#  Génération de données paramétrables selon le type d'expérience
	if (experience_type == "enzyme") {
		# Données d'activité enzymatique
		data <- data.frame(
			echantillon = paste0("e", 1:n_obs),
			souche = sample(c("souche_a", "souche_b", "souche_c", "souche_d"), 
											n_obs, replace = TRUE),
			activite_ph6 = rnorm(n_obs, mean = 45 + seed * 3e-8, sd = 8),
			activite_ph7 = rnorm(n_obs, mean = 52 + seed * 3e-8, sd = 7.5)
		)
		
		data$souche = factor(data$souche)
		
		data$activite_ph6 = data$activite_ph6 +
			rnorm(n_obs, sd = c(12, 0.2, 0.01, 7)[as.integer(data$souche)])
		data$activite_ph7 = data$activite_ph7 +
			rnorm(n_obs, sd = c(12, 0.2, 0.01, 7)[as.integer(data$souche)])
		
		# Certaines souches sont plus actives quand le pH est acide versus neutre
		supp_acide = c(5, -2, 2, 0)[as.integer(data$souche)]
		supp_neutre   = c(-5, 4, 0, 2)[as.integer(data$souche)]
		
		data$activite_ph6 <- round(pmax(data$activite_ph6, 10), 2)
		data$activite_ph7 <- round(pmax(data$activite_ph7, 10), 2)
		
		var_principale <- "activite_ph6"
		var_secondaire <- "activite_ph7"
		var_categorielle <- "souche"
		unite <- "U/mL"
		contexte <- "l'activité enzymatique"
		var_label <- "Activité enzymatique à pH 6"
		var2_label <- "Activité enzymatique à pH 7"
		categorie_label <- "souche bactérienne"
		
	} else if (experience_type == "culture") {
		# Données de croissance cellulaire
		data <- data.frame(
			echantillon = paste0("C", 1:n_obs),
			milieu = sample(c("LB", "M9", "TSB", "BHI"), n_obs, replace = TRUE),
			DO_24h = rnorm(n_obs, mean = 0.8 + seed * 1e-8, sd = 0.15),
			DO_48h = rnorm(n_obs, mean = 1.2 + seed * 1e-8, sd = 0.18),
			temperature = sample(c(28, 30, 37), n_obs, replace = TRUE),
			glucose = round(runif(n_obs, 1, 10), 1)
		)
		data$DO_24h <- round(pmax(data$DO_24h, 0.1), 3)
		data$DO_48h <- round(pmax(data$DO_48h, 0.1), 3)
		
		var_principale <- "DO_24h"
		var_secondaire <- "DO_48h"
		var_categorielle <- "milieu"
		unite <- "unités DO₆₀₀"
		contexte <- "la densité optique"
		var_label <- "DO à 24h"
		var2_label <- "DO à 48h"
		categorie_label <- "type de milieu"
		
	} else {
		# Données de fermentation
		data <- data.frame(
			echantillon = paste0("f", 1:n_obs),
			levure = sample(c("S_cerevisiae", "S_bayanus", "K_marxianus", "K_pastoris"), 
											n_obs, replace = TRUE),
			ethanol_anaerobie = rnorm(n_obs, mean = 8.5 + seed * 5e-8, sd = 1.2),
			ethanol_aerobie = rnorm(n_obs, mean = 6.2 + seed * 5e-8, sd = 1.1),
			temperature = sample(c(28.5, 28, 29, 29.5), n_obs, replace = TRUE),
			sucre_initial = round(runif(n_obs, 100, 200), 1)
		)
		
		data$levure = factor(data$levure)
		
		data$ethanol_anaerobie = data$ethanol_anaerobie +
			rnorm(n_obs, sd = c(5, 1, 2, 0.5)[as.integer(data$levure)])
		
		data$ethanol_aerobie = data$ethanol_aerobie +
			rnorm(n_obs, sd = c(5, 1, 2, 0.5)[as.integer(data$levure)])
		
		# Certaines bactéries s'en sortent mieux en anéarobie qu'en aérobie
		supp_anaerobie = c(0.7, -0.3, 0.2, 0.1)[as.integer(data$levure)]
		supp_aerobie   = c(-0.1, 0.5, 0.2, 0.9)[as.integer(data$levure)]
		
		data$ethanol_anaerobie <- round(
			pmax(data$ethanol_anaerobie + supp_anaerobie, 2), 2
		) 
		data$ethanol_aerobie <- round(
			pmax(data$ethanol_aerobie + supp_aerobie, 1), 2
		)
		
		var_principale <- "ethanol_anaerobie"
		var_secondaire <- "ethanol_aerobie"
		var_categorielle <- "levure"
		unite <- "g/L"
		contexte <- "la production d'éthanol"
		var_label <- "Production d'éthanol (anaérobie)"
		var2_label <- "Production d'éthanol (aérobie)"
		categorie_label <- "souche de levure"
	}
	
	# Sauvegarde du dataset
	write.table(data, filename, row.names = FALSE)
}


# Génération de CC -------------------------------------------------------------

# CC habituel
data_generation("enzyme", 80, 20251207, "exam/cc/enzyme.txt")
quarto::quarto_render(
	"exam/cc/2025_2026_s1_l1_cc.qmd",
	output_file = "2025_2026_s1_l1_cc.pdf",
	metadata = list(date = "2025-12-05"),
	execute_params = list(n_obs = 80, experience_type = "enzyme")
)

# CC SHN
data_generation("fermentation", 72, 20251201, "exam/cc/fermentation.txt")
quarto::quarto_render(
	"exam/cc/2025_2026_s1_l1_cc.qmd",
	output_file = "2025_2026_s1_l1_cc_shn.pdf",
	metadata = list(date = "2025-12-01"),
	execute_params = list(n_obs = 72, experience_type = "fermentation")
)
	
