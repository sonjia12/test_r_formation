library(targets)
source("functions.R")
tar_option_set(packages = c("dplyr", "ggplot2", "forcats"))
list(
  tar_target(file_token , "secrets.yaml", format = "file"),
  tar_target(file_data , "individu_reg.parquet", format="file"),
  tar_target(token , read_yaml_secret(file_token,"api_token")),
  tar_target(data , read_from_parquet(file_data), format = 'parquet'),
  tar_target(clean_data  , retraitement_donnees (data), format = 'parquet'),
  tar_target(table_age , produce_table_age(clean_data))
)
