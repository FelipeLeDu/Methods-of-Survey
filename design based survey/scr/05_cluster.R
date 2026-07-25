# 05_cluster.R 
# Cluster Group Sampling Method 
#
# This script simulates a cluster 2 stage sampling design using census sectors
# as clusters. In cluster sampling, the population is divided into groups called
# clusters. In the one stage: use all observations in the cluster simulated. 
# In the two stage: sample a fixed number of observation in each cluster simulated. 
# 
# We will use: 
# sample size: n = 1200, as in the previous samples methods 
# observatios per cluster: ni = 15 
# total clusters: n_conglo = n /ni = 1200/15 
# simulations: B = 1000

set.seed(789)

# Unlike strata method, in cluster sampling we need high within + low between variances
# We will use census tract as our clusters 

ni <- 15 
n_conglo <- n / ni   
n_conglo


# Filtering sectors: we will only use census that have sufficient observations for the ni sample

setores_elegiveis <- setores |> filter(Mi >= ni)

nrow(setores_elegiveis)          # sectors candidates
nrow(setores) - nrow(setores_elegiveis)  #  into trash 

# Population sector

pop_setor <- split(df$alfabetizada, df$codigo_setor)

# Sampling simulation 

mean_cong <- numeric(B)

for (i in 1:B) {
  
  # sampling n_cong sectors candidates, same prob (pi_i = n/N)
  setores_sorteados <- sample(
    setores_elegiveis$codigo_setor,
    size = n_conglo,
    replace = FALSE)
  
  # sampling inside sector without reposition 
  amostra_cong <- unlist( 
    lapply(setores_sorteados, function(s) {
      sample(pop_setor[[as.character(s)]], size = ni, replace = FALSE)
    })
  )
  
  # sampled sector's mean  (n = 1200 total)
  mean_cong[i] <- mean(amostra_cong)
}

# dataframe

df_mean_cong <- data.frame(amostra = 1:B, media = mean_cong)

mean_m_cong <- mean(df_mean_cong$media)
var_cong    <- var(df_mean_cong$media)   # sample variance  

# Distribution plot

plot_cluste_distribution <- function() {
  
  # Histogram plot
  p_hist <- ggplot(df_mean_cong, aes(x = media)) +
    geom_histogram(
      bins = 30,
      fill = "steelblue",
      color = "white",
      alpha = 0.9
    ) +
    geom_vline(
      xintercept = mean_m_cong,
      linetype = "dashed",
      color = "orange",
      linewidth = 1
    ) +
    annotate(
      geom = "text",
      x = mean_m_cong,
      y = Inf,
      label = paste0("Mean = ", round(mean_m_cong, 4)),
      color = "orange",
      vjust = 1.5,
      hjust = -0.05,
      fontface = "bold",
      size = 4
    ) +
    labs(
      title = "Histogram",
      subtitle = paste0(B, " SRS simulations, n = ", n),
      x = "Sample literacy rate",
      y = "Frequency"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "gray40"),
      panel.grid.minor = element_blank()
    )
  
  # Density plot
  p_density <- ggplot(df_mean_cong, aes(x = media)) +
    geom_density(
      fill = "steelblue",
      color = "darkblue",
      alpha = 0.35,
      linewidth = 1
    ) +
    geom_vline(
      xintercept = mean_m_cong,
      linetype = "dashed",
      color = "orange",
      linewidth = 1
    ) +
    annotate(
      geom = "text",
      x = mean_m_cong,
      y = Inf,
      label = paste0("Mean = ", round(mean_m_cong, 4)),
      color = "orange",
      vjust = 1.5,
      hjust = -0.05,
      fontface = "bold",
      size = 4
    ) +
    labs(
      title = "Density",
      subtitle = "Smoothed sampling distribution",
      x = "Sample literacy rate",
      y = "Density"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "gray40"),
      panel.grid.minor = element_blank()
    )
  
  # Combine plots side by side
  gridExtra::grid.arrange(
    p_hist,
    p_density,
    ncol = 2,
    top = "Sampling Distribution of Sample Means — Cluster Sampling"
  )
}

plot_cluste_distribution()

ggsave(
  filename = "output/figures/05_cluster_sampling_distribution.png",
  plot = plot_cluste_distribution(),
  width = 8,
  height = 5,
  dpi = 300
)
