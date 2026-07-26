# 03_specification.R 

survey <- survey %>% mutate(y_contra_aborto = ifelse(contra_aborto == "Sim", 1, 0))

vars_candidatas <- c("sexo", "idade", "escolaridade", "cor_raca")

# All subconjuncts possibles of var_candidatas, from none (only intercept)
# to complete conjuct (2^4 =16 models)

combinacoes_vars <- unlist(
  lapply(0:length(vars_candidatas), combn, x = vars_candidatas, simplify = FALSE),
  recursive = FALSE
)

ajustar_modelo <- function(vars_incluidas) {
  formula_str <- if (length(vars_incluidas) == 0) {
    "y_contra_aborto ~ 1 + (1 | uf)"
  } else {
    paste("y_contra_aborto ~", paste(vars_incluidas, collapse = " + "), "+ (1 | uf)")
  }
  glmer(as.formula(formula_str), data = survey, family = binomial(link = "logit"))
}

modelos <- lapply(combinacoes_vars, ajustar_modelo)
nomes_modelos <- paste0("M", seq_along(modelos))
names(modelos) <- nomes_modelos

# Fit statistic: ordered from best AIC (lower AIC) to the worst

estatisticas <- purrr::map_dfr(seq_along(modelos), function(i) {
  m <- modelos[[i]]
  tibble(
    modelo    = nomes_modelos[i],
    variaveis = if (length(combinacoes_vars[[i]]) == 0) "(nulo)" else paste(combinacoes_vars[[i]], collapse = " + "),
    AIC       = AIC(m),
    BIC       = BIC(m),
    logLik    = as.numeric(logLik(m)),
    n         = nobs(m)
  )
}) %>%
  arrange(AIC)

print(estatisticas, n = Inf)
write.csv(estatisticas, "output/tables/03_modelo_by_AIC.csv", row.names = FALSE)
cat("\n Best model by AIC, fixed effects:", estatisticas$modelo[1], "->", estatisticas$variaveis[1], "\n")

# Models table coeficients, p-value and standard error

estrela <- function(p) dplyr::case_when(
  p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", p < 0.1 ~ ".", TRUE ~ ""
)

ordem_modelos <- estatisticas$modelo

coefs <- purrr::map_dfr(seq_along(modelos), function(i) {
  broom.mixed::tidy(modelos[[i]], effects = "fixed") %>%
    mutate(
      modelo = nomes_modelos[i],
      celula = sprintf("%.2f%s\n(%.2f)", estimate, estrela(p.value), std.error)
    ) %>%
    select(modelo, term, celula)
})

ordem_termos <- c(
  "(Intercept)", "sexoMulher",
  "idade25 a 34 anos", "idade35 a 44 anos", "idade45 a 59 anos", "idade60 anos ou mais",
  "escolaridadeMédio", "escolaridadeSuperior",
  "cor_racaPreta", "cor_racaParda", "cor_racaOutras"
)
rotulos_termos <- c(
  "(Intercept)" = "Intercepto", "sexoMulher" = "Sexo: Mulher",
  "idade25 a 34 anos" = "Idade: 25-34", "idade35 a 44 anos" = "Idade: 35-44",
  "idade45 a 59 anos" = "Idade: 45-59", "idade60 anos ou mais" = "Idade: 60+",
  "escolaridadeMédio" = "Escolaridade: Médio", "escolaridadeSuperior" = "Escolaridade: Superior",
  "cor_racaPreta" = "Cor/raça: Preta", "cor_racaParda" = "Cor/raça: Parda",
  "cor_racaOutras" = "Cor/raça: Outras"
)

tabela_coef <- coefs %>%
  mutate(modelo = factor(modelo, levels = ordem_modelos)) %>%
  pivot_wider(names_from = modelo, values_from = celula, values_fill = "") %>%
  mutate(term = factor(term, levels = ordem_termos)) %>%
  arrange(term) %>%
  mutate(term = rotulos_termos[as.character(term)]) %>%
  select(term, all_of(ordem_modelos))

indicador_vars <- purrr::map_dfr(seq_along(combinacoes_vars), function(i) {
  tibble(
    modelo = nomes_modelos[i],
    Sexo = ifelse("sexo" %in% combinacoes_vars[[i]], "X", ""),
    Idade = ifelse("idade" %in% combinacoes_vars[[i]], "X", ""),
    Escolaridade = ifelse("escolaridade" %in% combinacoes_vars[[i]], "X", ""),
    `Cor/raça` = ifelse("cor_raca" %in% combinacoes_vars[[i]], "X", "")
  )
}) %>%
  mutate(modelo = factor(modelo, levels = ordem_modelos)) %>%
  arrange(modelo)

indicador_wide <- as.data.frame(t(indicador_vars %>% select(-modelo)))
colnames(indicador_wide) <- ordem_modelos
indicador_wide$term <- rownames(indicador_wide)
indicador_wide <- indicador_wide[, c("term", ordem_modelos)]

tabela_stats <- estatisticas %>%
  mutate(modelo = factor(modelo, levels = ordem_modelos)) %>%
  arrange(modelo) %>%
  transmute(AIC = sprintf("%.0f", AIC), BIC = sprintf("%.0f", BIC), N = as.character(n))
tabela_stats_wide <- as.data.frame(t(tabela_stats))
colnames(tabela_stats_wide) <- ordem_modelos
tabela_stats_wide$term <- rownames(tabela_stats_wide)
tabela_stats_wide <- tabela_stats_wide[, c("term", ordem_modelos)]

tabela_final <- bind_rows(indicador_wide, tabela_coef, tabela_stats_wide)
tabela_final[is.na(tabela_final)] <- ""
write.csv(tabela_final, "output/tables/03_complete_model_tables.csv", row.names = FALSE)

# Rendering image 

n_cols <- ncol(tabela_final)
cores_linha <- c(
  rep("#DCE6F1", 4),                               # variable indicator
  rep(c("#FFFFFF", "#F5F5F5"), length.out = 11),   # coeficients
  rep("#E8E8E8", 3)                                # AIC / BIC / N
)

tema_tabela <- ttheme_minimal(
  core = list(
    fg_params = list(fontsize = 8, hjust = 0.5, x = 0.5),
    bg_params = list(fill = matrix(rep(cores_linha, n_cols), ncol = n_cols), col = "grey85", lwd = 0.5)
  ),
  colhead = list(
    fg_params = list(fontsize = 9, fontface = "bold", col = "white"),
    bg_params = list(fill = "#2C3E50")
  ),
  rowhead = list(fg_params = list(fontsize = 8, fontface = "bold", hjust = 0, x = 0.02))
)

grob_tabela <- tableGrob(
  tabela_final[, -1],
  rows = tabela_final$term,
  cols = colnames(tabela_final)[-1],
  theme = tema_tabela
)

png("output/figures/03_table_models.png", width = 20.5, height = 9, units = "in", res = 220)
grid::grid.newpage()
grid::grid.draw(grob_tabela)
dev.off()

# * p<0.05, ** p<0.01, *** p<0.001. Models ordered left to right (lower AIC = better fit) 
