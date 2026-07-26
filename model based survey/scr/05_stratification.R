# 5_stratification

estratos_pred <- estratos %>%
  mutate(
    p_contra_aborto = predict(modelo, newdata = ., type = "response", allow.new.levels = TRUE)
  )

# % against abortion by UF - Mrp  

resultado_uf <- estratos_pred %>%
  group_by(uf) %>%
  summarise(pct_contra_aborto = 100 * weighted.mean(p_contra_aborto, w = n), .groups = "drop") %>%
  arrange(desc(pct_contra_aborto))

print(resultado_uf, n = Inf)
write.csv(resultado_uf, "output/tables/05_mrp_results_uf.csv", row.names = FALSE)


# % against abortion region - Mrp 

resultado_regiao <- estratos_pred %>%
  group_by(regiao) %>%
  summarise(pct_contra_aborto = 100 * weighted.mean(p_contra_aborto, w = n), .groups = "drop") %>%
  arrange(desc(pct_contra_aborto))

print(resultado_regiao)
write.csv(resultado_regiao, "output/tables/05_mrp_results_region.csv", row.names = FALSE)


# Plot: % against abortion by UF 

bruto_uf <- survey %>%
  group_by(uf) %>%
  summarise(pct_bruto = 100 * mean(y_contra_aborto), .groups = "drop")

dados_uf <- resultado_uf %>%
  rename(pct_ajustado = pct_contra_aborto) %>%
  left_join(bruto_uf, by = "uf") %>%
  mutate(uf = fct_reorder(uf, pct_ajustado))

grafico_uf <- ggplot(dados_uf) +
  geom_segment(aes(x = pct_bruto, xend = pct_ajustado, y = uf, yend = uf),
               color = "grey60") +
  geom_point(aes(x = pct_bruto, y = uf, shape = "Pré-MRP"),
             size = 2.5, color = "black", fill = "white", stroke = 0.8) +
  geom_point(aes(x = pct_ajustado, y = uf, shape = "Pós-MRP"),
             size = 2.5, color = "black") +
  scale_shape_manual(name = NULL, values = c("Pré-MRP" = 21, "Pós-MRP" = 16)) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100)) +
  labs(
    x = "Percentual %", y = NULL,
    title = "Original estimation vs MRP",
    subtitle = "Against abortion by UF"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

ggsave("output/figures/05_mrp_results_uf.png", grafico_uf, width = 7, height = 8, dpi = 300)
print(grafico_uf)

# Heat Map by UF 

mapa_uf <- st_read(
  "https://github.com/giuliano-macedo/geodata-br-states/raw/refs/heads/main/geojson/br_states.json",
  quiet = TRUE
) %>%
  select(uf = SIGLA, geometry)

dados_mapa <- dados_uf %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  select(uf, `Bruta (sem ajuste)` = pct_bruto, `Ajustada (MRP)` = pct_ajustado) %>%
  pivot_longer(-uf, names_to = "tipo", values_to = "percentual") %>%
  mutate(tipo = factor(tipo, levels = c("Bruta (sem ajuste)", "Ajustada (MRP)"))) %>%
  left_join(mapa_uf, by = "uf") %>%
  st_as_sf()

mapa_comparacao <- ggplot(dados_mapa) +
  geom_sf(aes(fill = percentual), color = "white", linewidth = 0.1) +
  facet_wrap(~ tipo) +
  scale_fill_gradient(low = "#edf67d", high = "#564592", name = "% contra\no aborto") +
  labs(title = "Oposição ao aborto por UF: estimativa bruta x MRP") +
  theme_minimal(base_size = 11) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold", size = 12)
  )

ggsave("output/figures/05_mrp_heat_map.png", mapa_comparacao, width = 10, height = 5.5, dpi = 300)
print(mapa_comparacao)
