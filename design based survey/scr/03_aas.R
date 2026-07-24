# 03_simple_random_sampling.R
# Simple Random Sampling 
#
# This script simulates Simple Random Sampling without replacement
# using the Macaé population dataset. In Simple Random Sampling (SRS),
# every individual in the population has the same probability of being 
# selected into the sample.
#
# The goal is to estimate the population literacy rate using repeated
# samples and analyze the sampling distribution of the sample mean.

set.seed(123)

# Parameters

n <- 1200
B <- 1000

# Simple Random Sampling simulation

mean_ass <- numeric(B)

aas <- for(i in 1:B){
  x <- sample(df$alfabetizada, size = n, replace = FALSE) # sem reposição
  mean_ass[i] <- mean(x)
  print(mean(x))
}


# Dataframe das medias 

df_means_aas <- data.frame(
  amostra = 1:B,
  media = mean_ass
)

# Mean of Means

mean_m <- mean(df_means_aas$media)

# Sample's variance 

var_aas <- mean((df_means_aas$media - mean_m)^2)

# Distribution plot

plot_aas_distribution <- function() {
  
  # Histogram plot
  p_hist <- ggplot(df_means_aas, aes(x = media)) +
    geom_histogram(
      bins = 30,
      fill = "steelblue",
      color = "white",
      alpha = 0.9
    ) +
    geom_vline(
      xintercept = mean_m,
      linetype = "dashed",
      color = "orange",
      linewidth = 1
    ) +
    annotate(
      geom = "text",
      x = mean_m,
      y = Inf,
      label = paste0("Mean = ", round(mean_m, 4)),
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
  p_density <- ggplot(df_means_aas, aes(x = media)) +
    geom_density(
      fill = "steelblue",
      color = "darkblue",
      alpha = 0.35,
      linewidth = 1
    ) +
    geom_vline(
      xintercept = mean_m,
      linetype = "dashed",
      color = "orange",
      linewidth = 1
    ) +
    annotate(
      geom = "text",
      x = mean_m,
      y = Inf,
      label = paste0("Mean = ", round(mean_m, 4)),
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
    top = "Sampling Distribution of Sample Means — Simple Random Sampling"
  )
}

plot_aas_distribution()

ggsave(
  filename = "output/figures/03_aas_sampling_distribution.png",
  plot = plot_aas_distribution(),
  width = 8,
  height = 5,
  dpi = 300
)
