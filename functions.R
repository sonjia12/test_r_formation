
library(dplyr)
library(ggplot2)
library(forcats)

#' Calcul de la décennie à partir de l'année
#'
#' @param annee \code{numeric} - Année pour laquelle on veut calculer la décennie
#'
#' @return Décennie (\code{numeric})
#' @export
#'
#' @examples decennie_a_partir_annee(2019) # vaut 2010
decennie_a_partir_annee <- function(annee) {
  return(annee - annee %% 10)
}


#' Calcul d'une statistique agrégée (moyenne, écart-type, variance) pour un vecteur de données
#'
#' @param vecteur_donnees vecteur \code{numeric} - 
#' @param stat \code{string} - statistique à calculer. Doit prendre la valeur "moyenne", "ecart-type", "sd" ou "variance"
#' @param ... Voir les paramètre de \code{mean}, \code{sd} ou \code{var}
#'
#' @return Moyenne, écart-type ou  variance de \code{vecteur_donnees} (\code{numeric})
#' @export
#'
#' @examples calcul_stat_agregee(rnorm(10), "ecart-type")
calcul_stat_agregee <- function(vecteur_donnees, stat = "moyenne", ...) {
  if (stat == "moyenne") {
    valeur_stat <- mean(vecteur_donnees, na.rm = TRUE, ...)
  } else if (stat == "ecart-type" || stat == "sd") {
    valeur_stat <- sd(vecteur_donnees, na.rm = TRUE, ...)
  } else if (stat == "variance") {
    valeur_stat <- var(vecteur_donnees, na.rm = TRUE, ...)
  }
  return(valeur_stat)
}

read_yaml_secret <- function(path, key) {
  return(yaml::read_yaml(path)[[key]])
}
read_from_parquet <- function(path) {
  df <- arrow::read_parquet(
    path,
    col_select  = c(
      "region", "aemm", "aged", "anai", "catl", "cs1", "cs2", "cs3",
      "couple", "na38", "naf08", "pnai12", "sexe", "surf", "tp",
      "trans", "ur"
    )
  )
  return(df)
}

retraitement_donnees <- function(df){
  df <- df %>%
    mutate(aged = as.numeric(aged))
  df$sexe <- df$sexe %>%
    as.character() %>%
    fct_recode(Homme = "1", Femme = "2")
  return(df)
}

retraitement_donnees <- function(df){
  df <- df %>%
    mutate(aged = as.numeric(aged))
  df$sexe <- df$sexe %>%
    as.character() %>%
    fct_recode(Homme = "1", Femme = "2")
  return(df)
}


produce_table_age <- function(df){
  stats_age <- df %>%
    group_by(decennie = decennie_a_partir_annee(aged)) %>%
    summarise(n())
  
  table_age <- gt::gt(stats_age) %>%
    gt::tab_header(
      title = "Distribution des âges dans notre population"
    ) %>%
    gt::fmt_number(
      columns = `n()`,
      sep_mark = " ",
      decimals = 0
    ) %>%
    gt::cols_label(
      decennie = "Tranche d'âge",
      `n()` = "Population"
    )
  return(table_age)
}
