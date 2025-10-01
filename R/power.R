#' Simulation de puissance pour un test t (Welch ou pooled)
#'
#' Cette fonction effectue une simulation Monte-Carlo de la puissance
#' pour un test t comparant deux traitements.
#'
#' @param n_rep Nombre de répétitions par traitement
#' @param n_sims Nombre d'itérations de simulation
#' @param mu Rendement moyen (t/ha) du témoin
#' @param sd Écart-type (t/ha)
#' @param effet Effet attendu (additif ou multiplicatif)
#' @param alpha Niveau de signification (ex.: 0.05)
#' @param alternative "two.sided", "greater" ou "less"
#' @param equal_var TRUE pour test avec variances égales (pooled),
#'                  FALSE pour test de Welch
#' @param effect_mode "add" (additif, t/ha) ou "mult" (multiplicatif, proportion)
#'
#' @return Proportion de rejets de H0 = puissance estimée (entre 0 et 1)
#' @export
simuler_puissance <- function(n_rep, n_sims, mu, sd, effet, alpha,
                              alternative = "two.sided",
                              equal_var = FALSE,
                              effect_mode = "add") {
  # Ajustement de la moyenne du traitement selon l'effet
  mu_trt <- if (effect_mode == "mult") mu * (1 + effet) else mu + effet

  # Génération des échantillons simulés
  ctrl <- matrix(stats::rnorm(n_sims * n_rep, mean = mu, sd = sd), nrow = n_sims)
  trt  <- matrix(stats::rnorm(n_sims * n_rep, mean = mu_trt, sd = sd), nrow = n_sims)

  mC <- rowMeans(ctrl)
  mT <- rowMeans(trt)
  vC <- apply(ctrl, 1, stats::var)
  vT <- apply(trt, 1, stats::var)

  # Calcul de l'erreur standard et ddl
  if (equal_var) {
    sp2 <- ((n_rep - 1) * vC + (n_rep - 1) * vT) / (2 * n_rep - 2)
    se  <- sqrt(sp2 * (1 / n_rep + 1 / n_rep))
    df  <- 2 * n_rep - 2
  } else {
    se  <- sqrt(vC / n_rep + vT / n_rep)
    df  <- (vC / n_rep + vT / n_rep)^2 /
      ((vC^2 / (n_rep^2 * (n_rep - 1))) + (vT^2 / (n_rep^2 * (n_rep - 1))))
  }

  # Statistique t et p-valeur
  tval <- (mT - mC) / se
  pval <- switch(
    alternative,
    "greater"   = 1 - stats::pt(tval, df = df),
    "less"      = stats::pt(tval, df = df),
    "two.sided" = 2 * stats::pt(-abs(tval), df = df)
  )

  mean(pval <= alpha)
}


#' Simulation de puissance pour un test d'équivalence (TOST)
#'
#' Cette fonction estime la puissance du test TOST, en vérifiant si
#' l'écart entre traitements reste dans ± delta.
#'
#' @inheritParams simuler_puissance
#' @param delta_abs Marge d'équivalence (t/ha)
#'
#' @return Puissance estimée du TOST (entre 0 et 1)
#' @export
simuler_puissance_tost <- function(n_rep, n_sims, mu, sd, alpha, delta_abs,
                                   equal_var = FALSE) {
  # Génération des échantillons simulés
  ctrl <- matrix(stats::rnorm(n_sims * n_rep, mean = mu, sd = sd), nrow = n_sims)
  trt  <- matrix(stats::rnorm(n_sims * n_rep, mean = mu, sd = sd), nrow = n_sims)

  mC <- rowMeans(ctrl)
  mT <- rowMeans(trt)
  vC <- apply(ctrl, 1, stats::var)
  vT <- apply(trt, 1, stats::var)

  # Calcul de l'erreur standard et ddl
  if (equal_var) {
    sp2 <- ((n_rep - 1) * vC + (n_rep - 1) * vT) / (2 * n_rep - 2)
    se  <- sqrt(sp2 * (1 / n_rep + 1 / n_rep))
    df  <- 2 * n_rep - 2
  } else {
    se  <- sqrt(vC / n_rep + vT / n_rep)
    df  <- (vC / n_rep + vT / n_rep)^2 /
      ((vC^2 / (n_rep^2 * (n_rep - 1))) + (vT^2 / (n_rep^2 * (n_rep - 1))))
  }

  d <- mT - mC

  # TOST = deux tests unilatéraux
  tL <- (d + delta_abs) / se
  pL <- 1 - stats::pt(tL, df = df)

  tU <- (d - delta_abs) / se
  pU <- stats::pt(tU, df = df)

  mean(pL <= alpha & pU <= alpha)
}
