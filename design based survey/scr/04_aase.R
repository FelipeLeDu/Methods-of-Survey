# 04_aase.R
# Stratified Simple Random Sampling
# 
# This script simulates Stratified Simple Random Sampling without
# replacement using the Macaé population dataset.
#
# In stratified sampling, the population is divided into groups
# called strata. Then, a Simple Random Sample is drawn within each
# stratum. The goal is to estimate the population literacy rate using repeated
# stratified samples and analyze the sampling distribution. 

# Identify potential stratification variables
#
# As wee cloclude in 02_analysis, a good stratification variable 
# should create groups that are internally homogeneous with respect 
# to the variable of interest. Neighborhood has the lowest
# within-group variance and the highest between-group variance.
# However, neighborhood's smallest group has 103 observation. 
# Subdistrict can be used as a more practical intermediate option, alos has 
# an similar within variance as neighborhood.

set.seed(321)

# Stratas's sizes 

N <- nrow(df)

tabela <- df |>
  group_by(subdistrito) |>
  summarise(Nh = n(),.groups = "drop")

# Weigh of each subdistric 

tabela <- tabela |>
  mutate(Wh = Nh / N)

# n = 1200 between stratas  

tabela <- tabela |>
  mutate(nh = round(n * Wh)) # rounf value

tabela

total <- sum(tabela$nh) # total should be n = 1200
total 

# Split population by subdistrics

pop <- split( df$alfabetizada, df$subdistrito)

# Simulation

mean_aase <- numeric(B)

for (i in 1:B) {
  
  # subdistrics's means vector  
  
  means_h <- numeric(nrow(tabela))
  
  # aleatorization 
  for (h in 1:nrow(tabela)) {
    stratum_name <- as.character(
      tabela$subdistrito[h])
    
    # subdistrics's population 
    
    population_h <- pop[[stratum_name]]
    
    # AAS without replase in each subdistric 
    
    sample_h <- sample(
      population_h,
      size = tabela$nh[h],
      replace = FALSE)
    
    # Sample subdistric's mean 
    
    means_h[h] <- mean(sample_h)
  }
  
  # Sample strata's mean 
  
  mean_aase[i] <- sum(tabela$Wh * means_h)
}

# dataframe 

df_mean_aase <- data.frame(amostra = 1:B,media = mean_aase)

# Means of means and variance 

mean_m_aase <- mean(df_mean_aase$media)

# Distribution Plot 

plot_aase_distribution <- function() {
  
  # Histogram plot
  
  p_hist <- ggplot(df_mean_aase, aes(x = media)) +
    geom_histogram(
      bins = 30,
      fill = "steelblue",
      color = "white",
      alpha = 0.9
    ) +
    geom_vline(
      xintercept = mean_m_aase,
      linetype = "dashed",
      color = "orange",
      linewidth = 1
    ) +
    annotate(
      geom = "text",
      x = mean_m_aase,
      y = Inf,
      label = paste0("Mean = ", round(mean_m_aase, 4)),
      color = "orange",
      vjust = 1.5,
      hjust = -0.05,
      fontface = "bold",
      size = 4
    ) +
    labs(
      title = "Histogram",
      subtitle = paste0(B, " stratified SRS simulations, n = ", n),
      x = "Stratified sample literacy rate",
      y = "Frequency"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "gray40"),
      panel.grid.minor = element_blank()
    )
  
  # Density plot
  
  p_density <- ggplot(df_mean_aase, aes(x = media)) +
    geom_density(
      fill = "steelblue",
      color = "darkblue",
      alpha = 0.35,
      linewidth = 1
    ) +
    geom_vline(
      xintercept = mean_m_aase,
      linetype = "dashed",
      color = "orange",
      linewidth = 1
    ) +
    annotate(
      geom = "text",
      x = mean_m_aase,
      y = Inf,
      label = paste0("Mean = ", round(mean_m_aase, 4)),
      color = "orange",
      vjust = 1.5,
      hjust = -0.05,
      fontface = "bold",
      size = 4
    ) +
    labs(
      title = "Density",
      subtitle = "Smoothed sampling distribution",
      x = "Stratified sample literacy rate",
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
    top = "Sampling Distribution of Sample Means — Stratified Simple Random Sampling"
  )
}


plot_aase_distribution()


ggsave(
  filename = "output/figures/04_asse_sampling_distribution.png",
  plot = plot_aase_distribution(),
  width = 8,
  height = 5,
  dpi = 300
)


