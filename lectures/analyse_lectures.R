# Make figures -----------------------------------------------------------------

sante = read.csv("exemple-donnees-sante-csv.txt")

table(sante$GroupeSanguin)

barplot(table(sante$GroupeSanguin))

pie(table(sante$GroupeSanguin))

hist(sante$Poids, main = "Histogramme du poids", xlab = "Poids", ylab = "Effectifs")
hist(sante$Poids, main = "Histogramme du poids", xlab = "Poids", ylab = "Effectifs", n = 4)

boxplot(sante$Poids, ylab = "Poids")


# Figure intervalle confiance --------------------------------------------------

set.seed(14102025)

library("dplyr")
library("ggplot2")


freq_card = lapply(1:100, \(x) round(rnorm(10, mean = 64, sd = 7)))

freq_ci = freq_card |>
	purrr::map_dfr(
		\(x) t.test(x)[["conf.int"]] |>
			tibble::enframe() |>
			tidyr::pivot_wider(),
		.id = "number"
	) |>
	dplyr::rename(low_ci = `1`, high_ci = `2`) |>
	dplyr::mutate(
		avg = (high_ci + low_ci) / 2,
		contains_true_mean = ifelse(low_ci <= 64 & high_ci >= 64, TRUE, FALSE)
	)

freq_ci |>
	ggplot(aes(y = number)) +
	geom_pointrange(
		aes(x = avg, xmin = low_ci, xmax = high_ci, color = contains_true_mean)
	) +
	geom_vline(xintercept = 64, linetype = 2, color = "darkred", linewidth = 1) +
	scale_color_manual(
		labels = c(`TRUE` = "Oui",     `FALSE` = "Non"),
		values = c(`TRUE` = "#56B4E9", `FALSE` = "#E69F00")
	) +
	lims(x = c(50, 80)) +
	labs(
		x = "Fréquence cardiaque (bpm)",
		y = "Échantillons",
		color = "Contient la vraie valeur de la moyenne"
	) +
	theme_bw(18) +
	theme(
		axis.text.y = element_blank(),
		axis.ticks.y = element_blank(),
		panel.grid = element_blank(),
		legend.position = "top"
	)



# Introduction p-value ---------------------------------------------------------
set.seed(20241015)

medicament = rnorm(20, mean = 10, sd = 1)
placebo    = rnorm(20, mean = 7, sd = 1.5)

df = data.frame(medicament, placebo)

t.test(df$medicament, df$placebo)


# Lecture 2 -------------------------------------------------------------------
set.seed(20251022)

freq_card = round(rnorm(50, mean = 60, sd = 7))

hist(freq_card, main = "Échantillon de Fréquences Cardiaques", xlab = "Fréquence Cardiaque (bpm)", ylab = "Effectifs")
abline(v = 64, lty = 2, col = "darkred", lwd = 2)


hist(rt(10000, 49), n = 100, main = NULL, xlab = NULL, ylab = NULL)
abline(v = -2.8, lty = 2, col = "purple", lwd = 2)

actual_test = t.test(freq_card, mu = 64)

# Quantile-quantile

ko = rnorm(25)
qqnorm(ko, xlab = "Valeurs théoriques", ylab = "Valeur observées",
			 main = "Diagramme Quantile-Quantile")
qqline(ko, col = "darkred", lwd = 2, lty = 2)


ko = rexp(1e2)

qqnorm(ko)
qqline(ko)

ko = runif(1e2)
qqnorm(ko)
qqline(ko)

ko = rnorm(1e2)
qqnorm(ko)
qqline(ko)


qqnorm(freq_card)
qqline(freq_card)

# Test de comparaison de deux échantillons

femmes = round(rnorm(50, mean = 64, sd = 3))
hommes = round(rnorm(50, mean = 62, sd = 3))

new_freq = data.frame(
	values = c(femmes, hommes),
	genre  = rep(c("femmes", "hommes"), each = 50)
)

ggplot(new_freq) +
	geom_density(aes(values, fill = genre), alpha = 2/5) +
	scale_fill_brewer(type = "qual") +
	labs(x = "Fréquence Cardiaque (bpm)", y = "Densité") +
	theme_classic() +
	theme(legend.position = "top")

t.test(femmes, hommes)

var.test(femmes, hommes)

# Comparaison de moyenne appariés

chol_avant = c(111, 116, 114, 115, 119, 123, 108, 106, 112, 107)
chol_apres = c(106, 89, 107, 93, 101, 100, 112, 110, 110, 99)

t.test(chol_avant, chol_apres, paired = TRUE)

t.test(femmes, hommes, alternative = "greater")
