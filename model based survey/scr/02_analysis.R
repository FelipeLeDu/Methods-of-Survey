# 02_analysis.R 
# Some data exploratory and descriptive analysis

# 1. Proportion sample vs population

# Function 

comparar_amostra_populacao <- function(var) {
  amostra <- survey %>%
    count(categoria = .data[[var]]) %>%
    mutate(prop_amostra = n / sum(n)) %>%
    select(categoria, prop_amostra)
  
  populacao <- estratos %>%
    group_by(categoria = .data[[var]]) %>%
    summarise(n_pop = sum(n), .groups = "drop") %>%
    mutate(prop_populacao = n_pop / sum(n_pop)) %>%
    select(categoria, prop_populacao)
  
  full_join(amostra, populacao, by = "categoria") %>%
    mutate(variavel = var)
}

variaveis_demograficas <- c("sexo", "idade", "cor_raca", "escolaridade", "uf")

# table final  

tab_comparacao <- purrr::map_dfr(variaveis_demograficas, comparar_amostra_populacao)
tab_comparacao_pct <- tab_comparacao %>%
  transmute(
    variavel,
    categoria,
    `Amostra (%)`   = round(100 * prop_amostra, 1),
    `População (%)` = round(100 * prop_populacao, 1),
    `Diferença (p.p.)` = round(100 * (prop_amostra - prop_populacao), 1)
  ) %>%
  arrange(variavel, categoria)

print(tab_comparacao_pct)

# Plot proportion

dados_grafico <- tab_comparacao %>%
  pivot_longer(
    cols = c(prop_amostra, prop_populacao),
    names_to = "fonte", values_to = "proporcao"
  ) %>%
  mutate(
    fonte = recode(fonte, prop_amostra = "Sample (survey)", prop_populacao = "Population (PNAD)"),
    variavel = recode(variavel,
                      sexo = "Sexo", idade = "Idade", cor_raca = "Cor/raça",
                      escolaridade = "Escolaridade", uf = "UF"
    )
  )

dados_grafico <- dados_grafico %>%
  group_by(variavel) %>%
  mutate(categoria = if (first(variavel) == "UF") fct_reorder(categoria, proporcao) else categoria) %>%
  ungroup()

grafico_comparacao <- ggplot(dados_grafico, aes(x = categoria, y = proporcao, group = fonte, linetype = fonte)) +
  geom_line(color = "grey30") +
  geom_point(size = 1.6, color = "grey20") +
  facet_wrap(~ variavel, scales = "free_x", nrow = 1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = NULL, y = NULL, linetype = NULL,
       title = "Sample (survey) x Population (PNAD 1T 2019)") +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text = element_text(face = "bold")
  )

ggsave("output/figures/02_sample_vs_population.png", grafico_comparacao, width = 12, height = 5, dpi = 300)
print(grafico_comparacao)

# 2. Correlation 

# HEATMAP

cramers_v <- function(x, y) {
  tab <- table(x, y)
  qui2 <- suppressWarnings(chisq.test(tab, correct = FALSE)$statistic)
  n <- sum(tab)
  k <- min(nrow(tab), ncol(tab))
  as.numeric(sqrt(qui2 / (n * (k - 1))))
}

vars_correlacao <- c("uf", "idade", "sexo", "cor_raca", "escolaridade",
                     "contra_aborto", "votou_bolsonaro_1t_2018")

combinacoes <- expand.grid(var1 = vars_correlacao, var2 = vars_correlacao, stringsAsFactors = FALSE)

combinacoes <- combinacoes %>%
  rowwise() %>%
  mutate(v = if (var1 == var2) 1 else cramers_v(survey[[var1]], survey[[var2]])) %>%
  ungroup()

grafico_correlacao <- ggplot(combinacoes, aes(x = var1, y = var2, fill = v)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", v)), size = 3) +
  scale_fill_gradient(low = "#cbeef3", high = "#880d1e", limits = c(0, 1), name = "V de Cramér") +
  labs(x = NULL, y = NULL, title = "Association between variables") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("output/figures/02_correlation_vas.png", grafico_correlacao, width = 7, height = 6, dpi = 300)
print(grafico_correlacao)

