# 01_treatment.R 
# Data treatment and validation

# 1.Dataframes 

# Survey sample 

survey <- read.csv("data/smith_boas_2019.csv", stringsAsFactors = FALSE, encoding = "UTF-8")

# PNAD (population reference)

load("data/estratos.rda")  

# 2. Basic verification 

# Survey Sample

cat("\nDimensions:\n")
cat("Rows:", nrow(survey), "\n")
cat("Columns:", ncol(survey), "\n")

cat("\nColumn names:\n")
print(names(survey))

cat("\nColumn classes:\n")
print(sapply(survey, class))

cat("\nCategorias:\n")
categorical_vars <- survey |>
  select(where(\(x) is.character(x) | is.factor(x))) |>
  select(-id) # remove id from the list

categories_list <- lapply(categorical_vars, function(x) {
  sort(unique(na.omit(x)))
})

categories_list

# renaming 
survey <- survey |>
  rename(votou_bolsonaro_1t_2018 = votou_bolsonato_1t_2018)

# b) PNAD 

cat("\nDimensões:\n")
cat("Linhas:", nrow(estratos), "\n")
cat("Colunas:", ncol(estratos), "\n")

cat("\nNome das colunas:\n")
print(names(estratos))

cat("\nCategorias:\n")
categorical_vars_e <- estratos |>
  select(where(\(x) is.character(x) | is.factor(x))) 

categories_list_e <- lapply(categorical_vars_e, function(x) {
  sort(unique(na.omit(x)))
})

categories_list_e

# check Nas and removing in Survey

str(survey)
colSums 

n_antes <- nrow(survey)

survey <- survey %>%
  filter(!is.na(uf), !is.na(sexo), uf != "", sexo != "")

cat("Removed Nas in uf/sexo:", n_antes - nrow(survey),
    "de", n_antes, "\n")

# Standardization by factors  

# a) Survey sample 

niveis_idade        <- c("18 a 24 anos", "25 a 34 anos", "35 a 44 anos", "45 a 59 anos", "60 anos ou mais")
niveis_sexo         <- c("Homem", "Mulher")
niveis_cor_raca     <- c("Branca", "Preta", "Parda", "Outras")
niveis_escolaridade <- c("Fundamental", "Médio", "Superior")

survey <- survey %>%
  mutate(
    uf       = factor(uf),
    idade    = factor(idade,    levels = niveis_idade),
    sexo     = factor(sexo,     levels = niveis_sexo),
    cor_raca = factor(cor_raca, levels = niveis_cor_raca),
    escolaridade = case_when(
      str_starts(escolaridade, "Fund")  ~ "Fundamental",
      str_starts(escolaridade, "Super") ~ "Superior",
      TRUE ~ "Médio"
    ),
    escolaridade  = factor(escolaridade,  levels = niveis_escolaridade),
    contra_aborto = factor(contra_aborto, levels = c(0, 1), labels = c("Não", "Sim")),
    votou_bolsonaro_1t_2018 = factor(votou_bolsonaro_1t_2018, levels = c(0, 1), labels = c("Não", "Sim"))
  )

cat("Nas verification after standardization:")
survey %>%
  summarise(across(c(idade, sexo, cor_raca, escolaridade), ~ sum(is.na(.)))) %>%
  print()

# PNAD 

estratos <- estratos %>%
  mutate(
    uf       = factor(uf),
    idade    = factor(idade,    levels = niveis_idade),
    sexo     = factor(sexo,     levels = niveis_sexo),
    cor_raca = factor(cor_raca, levels = niveis_cor_raca),
    escolaridade = case_when(
      str_starts(escolaridade, "Fund")  ~ "Fundamental",
      str_starts(escolaridade, "Super") ~ "Superior",
      TRUE ~ "Médio"
    ),
    escolaridade = factor(escolaridade, levels = niveis_escolaridade)
  )

# counting different categories between dataframes 

setdiff(levels(survey$uf), levels(estratos$uf))
setdiff(levels(survey$idade), levels(estratos$idade))

# Creating v.a region (Região)

regioes <- tibble::tribble(
  ~uf,~regiao,
  c("AC", "AP", "AM", "PA", "RO", "RR", "TO"),   "Norte",
  c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE"), "Nordeste",
  c("DF", "GO", "MT", "MS"),                     "Centro-Oeste",
  c("ES", "MG", "RJ", "SP"),                     "Sudeste",
  c("PR", "RS", "SC"),                           "Sul"
) %>%
  tidyr::unnest(uf)

survey <- survey %>%
  left_join(regioes, by = "uf")

estratos <- estratos %>%
  left_join(regioes, by = "uf")

# cheking Nas in region 

sum(is.na(survey$regiao))
sum(is.na(estratos$regiao))


