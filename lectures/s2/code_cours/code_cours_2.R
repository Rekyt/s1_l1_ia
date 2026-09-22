# Script pour réaliser toutes les analyses de données / figures du cours
# sur les tests non paramétriques de comparaison de moyenne
#
# Packages ---------------------------------------------------------------------
library("dplyr")


# Tests d'ajustements ----------------------------------------------------------
set.seed(20250319)

tailles_femmes = round(rnorm(12, mean = 162.5, sd = 5))

shapiro.test(tailles_femmes)

# Exemple diagramme quantile-quantile
ppoints(12)[1]
qnorm(ppoints(12)[1])

qqnorm(tailles_femmes)
qqline(tailles_femmes)
abline(v = qnorm(ppoints(12)[1]), lty = 2, col = "darkred", lwd = 2)
abline(h = min(tailles_femmes), lty = 2, col = "darkblue", lwd = 2)

# Fonction de répartition
x = seq(-5, 5, length.out = 1000)
plot(
    x, pnorm(x), type = "l",
    main = "Fonction de répartition\nde la distribution normale centrée-réduite",
    xlab = "Valeur de l'échantillon", ylab = "Probabilité que X ≤ x"
)
segments(x0 = -5.2, x1 = qnorm(0.2), y0 = 0.2, lty = 2, col = "darkred", lwd = 3)
segments(qnorm(0.2), y0 = -0.2, y1 = 0.2, lty = 2, col = "darkred", lwd = 3)

# Distributions sur R
par(mfrow = c(2, 2))
plot(x, dnorm(x), main = "Distribution normale (= pnorm())", type = "l", col = "#1B9E77", ylab = NA)
plot(x, dunif(x), main = "Distribution uniforme (= punif())", type = "l", col = "#D95F02", ylab = NA)
plot(x, dexp(x),  main = "Distribution exponetielle (= pexp())", type = "l", col = "#7570B3", ylab = NA)
plot(x, dt(x, df = 3), main = "Distribution t (= pt())", type = "l", col = "#E7298A", ylab = NA)

# Chi² -------------------------------------------------------------------------

# Chi² d'ajustement

chisq.test(
    c(140, 40, 20),          # Effectifs observés
    p = c(0.74, 0.23, 0.03)  # Proportions théoriques
)

regimes = chisq.test(
    c(140, 40, 20),          # Effectifs observés
    p = c(0.74, 0.23, 0.03)  # Proportions théoriques
)

250*0.15

regimes$expected


# Chi² d'homogénéité
chisq.test(matrix(c(140, 40, 20, 148, 60, 42), nrow = 2, byrow = TRUE))


# Chi² d'indépendance

diplome_genre = matrix(
    c(18.3, 28.7, 20.7, 27.1, 18.3, 17.3, 16.6, 13.4, 26.1, 23.5), byrow = TRUE,
    ncol = 2,
    dimnames = list(
        diplome = c(
            "brevet_ou_moins", "bep_cap", "bac", "bac_plus_2", "sup_bac_plus_2"
        ),
        genre   = c("femme", "homme")
    )
)

diplome_genre = diplome_genre*10

chisq.test(diplome_genre)$expected

chisq.test(diplome_genre)
