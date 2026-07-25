
# df coparison

df_comparacao <- bind_rows(
  df_means_aas  |> mutate(desenho = "AAS"),
  df_mean_asse  |> mutate(desenho = "AASE Strata"),
  df_mean_cong  |> mutate(desenho = "Clusters")
)

# mean per method 

media_por_metodo <- df_comparacao |>
  group_by(desenho) |>
  summarise(
    mean_method = mean(media),
    .groups = "drop"
  ) |>
  mutate(
    desenho_label = paste0(
      desenho,
      " (mean = ",
      round(mean_method, 4),
      ")"
    )
  )

print(media_por_metodo)

# Mean labels into df 

df_comparacao <- df_comparacao |>
  left_join(
    media_por_metodo,
    by = "desenho"
  )

# Plot result 

plot_sampling_methods_comparison <- function() {
  
  ggplot(
    df_comparacao,
    aes(
      x = media,
      fill = desenho_label,
      color = desenho_label
    )
  ) +
    geom_density(
      alpha = 0.35,
      linewidth = 0.9
    ) +
    geom_vline(
      data = media_por_metodo,
      aes(
        xintercept = mean_method,
        color = desenho_label
      ),
      linetype = "dashed",
      linewidth = 0.8,
      show.legend = FALSE
    ) +
    labs(
      title = "Sampling Distributions by Sampling Method",
      subtitle = paste0(B, " simulations per method, n = ", n),
      x = "Sample literacy rate",
      y = "Density",
      fill = "Sampling method",
      color = "Sampling method"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(color = "gray40"),
      legend.title = element_text(face = "bold"),
      legend.position = "right",
      panel.grid.minor = element_blank()
    )
}

plot_sampling_methods_comparison()

ggsave(
  filename = "output/figures/06_sampling_methods_comparison.png",
  plot = plot_sampling_methods_comparison(),
  width = 9,
  height = 5.5,
  dpi = 300
)

# Design effects 

var_ass <- var(df_means_ass$media)     
var_asse <- var(df_mean_asse$media)
var_cong <- var(df_mean_cong$media)

deff_asse <- var_asse / var_ass
deff_cong <- var_cong / var_ass

deff_asse
deff_cong