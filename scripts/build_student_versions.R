#!/usr/bin/env Rscript
# Génère les versions "étudiant" (sans solutions) des TD à partir des
# notebooks du dossier `solutions/`.
#
# Convention : dans les notebooks de `solutions/`, taguer les cellules à
# retirer avec le tag "solution" (Positron/VS Code : clic sur la cellule
# > "..." > "Edit Cell Tags", ou palette de commandes >
# "Notebook: Add Cell Tag").
#
# - Une cellule de code taguée "solution" est remplacée par un espace
#   réservé (ses sorties/exécutions sont effacées).
# - Une cellule markdown taguée "solution" est retirée du notebook.
#
# Usage :
#   Rscript scripts/build_student_versions.R
#   Rscript scripts/build_student_versions.R solutions/TD1_Introduction_R.ipynb

library(jsonlite)

PLACEHOLDER_CODE <- "# À vous de compléter !\n"

strip_solution_cells <- function(cells) {
  keep <- vector("list", 0)

  for (cell in cells) {
    tags <- cell$metadata$tags
    is_solution <- !is.null(tags) && "solution" %in% unlist(tags)

    if (!is_solution) {
      keep[[length(keep) + 1]] <- cell
      next
    }

    if (identical(cell$cell_type, "markdown")) {
      # Cellule markdown de solution : on la retire entièrement.
      next
    }

    if (identical(cell$cell_type, "code")) {
      # Cellule de code de solution : on vide le contenu et les sorties.
      cell$source <- list(PLACEHOLDER_CODE)
      cell$outputs <- list()
      cell$execution_count <- NULL
      keep[[length(keep) + 1]] <- cell
      next
    }

    # Autre type de cellule taguée solution : on la garde telle quelle
    # par prudence.
    keep[[length(keep) + 1]] <- cell
  }

  keep
}

build_student_version <- function(solution_path, output_path) {
  nb <- fromJSON(solution_path, simplifyVector = FALSE)
  nb$cells <- strip_solution_cells(nb$cells)

  json <- toJSON(
    nb,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null",
    na = "null"
  )
  writeLines(json, output_path)
  message(sprintf("Généré : %s", output_path))
}

main <- function(args) {
  if (length(args) == 0) {
    solution_files <- list.files("solutions", pattern = "\\.ipynb$", full.names = TRUE)
  } else {
    solution_files <- args
  }

  for (sol in solution_files) {
    # La version étudiant est écrite à la racine du projet, avec le même nom de fichier.
    output_path <- file.path(".", basename(sol))
    build_student_version(sol, output_path)
  }
}

if (sys.nframe() == 0) {
  main(commandArgs(trailingOnly = TRUE))
}
