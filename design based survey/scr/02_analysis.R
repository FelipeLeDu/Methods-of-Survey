# 02_analysis.R 
# Some data exploratory and descriptive analysis

# Geographic distribution

distrito_distri <- df |>
  count(distrito, name = "N") |>
  mutate(
    share = N / sum(N),
    share_percent = round(share * 100, 2)
  ) |>
  arrange(desc(N))

subdistrito_distri <- df |>
  count(subdistrito, name = "N") |>
  mutate(
    share = N / sum(N),
    share_percent = round(share * 100, 2)
  ) |>
  arrange(desc(N))

bairro_distri <- df |>
  count(bairro, name = "N") |>
  mutate(
    share = N / sum(N),
    share_percent = round(share * 100, 2)
  ) |>
  arrange(desc(N))

# a) population distribution by district

plot_distrito_distri <- function() {
  
  ggplot(
    distrito_distri,
    aes(x = reorder(distrito, N), y = N)
  ) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(
      title = "Population distribution by district",
      subtitle = "Number of individuals by district",
      x = "District",
      y = "Number of individuals"
    ) +
    theme_minimal()
}

plot_distrito_distri()

ggsave(
  filename = "output/figures/02_district_distribution.png",
  plot = plot_distrito_distri(),
  width = 8,
  height = 5,
  dpi = 300
)

# b) population distribution by subdistrict

plot_subdistrito_distri <- function() {
  
  ggplot(
    subdistrito_distri,
    aes(x = reorder(subdistrito, N), y = N)
  ) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(
      title = "Population distribution by subdistrict",
      subtitle = "Number of individuals by subdistrict",
      x = "Subdistrict",
      y = "Number of individuals"
    ) +
    theme_minimal()
}

plot_subdistrito_distri()

ggsave(
  filename = "output/figures/02_subdistrict_distribution.png",
  plot = plot_subdistrito_distri(),
  width = 8,
  height = 5,
  dpi = 300
)

# Literacy rates by geographic groups

alfa_distrito <- df |>
  group_by(distrito) |>
  summarise(
    N = n(),
    literacy_rate = mean(alfabetizada, na.rm = TRUE),
    variance = var(alfabetizada, na.rm = TRUE), # Variance in the group: heterogeneity
    .groups = "drop"
  ) |>
  arrange(desc(N))

alfa_subdistrito <- df |>
  group_by(subdistrito) |>
  summarise(
    N = n(),
    literacy_rate = mean(alfabetizada, na.rm = TRUE),
    variance = var(alfabetizada, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(N))

alfa_bairro <- df |>
  group_by(bairro) |>
  summarise(
    N = n(),
    literacy_rate = mean(alfabetizada, na.rm = TRUE),
    variance = var(alfabetizada, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(N))

# a) literacy rate by district

plot_alfa_distrito <- function() {
  
  ggplot(
    alfa_distrito,
    aes(x = reorder(distrito, literacy_rate), y = literacy_rate)
  ) +
    geom_col(fill = "darkred") +
    coord_flip() +
    labs(
      title = "Literacy rate by district",
      subtitle = "Mean of the binary literacy variable by district",
      x = "District",
      y = "Literacy rate"
    ) +
    theme_minimal()
}

plot_alfa_distrito()

ggsave(
  filename = "output/figures/02_literacy_by_district.png",
  plot = plot_alfa_distrito(),
  width = 8,
  height = 5,
  dpi = 300
)

# b) Literacy rate by subdistrict

plot_alfa_subdistr <- function() {
  
  ggplot(
    alfa_subdistrito,
    aes(x = reorder(subdistrito, literacy_rate), y = literacy_rate)
  ) +
    geom_col(fill = "darkred") +
    coord_flip() +
    labs(
      title = "Literacy rate by subdistrict",
      subtitle = "Mean of the binary literacy variable by subdistrict",
      x = "Subdistrict",
      y = "Literacy rate"
    ) +
    theme_minimal()
}

plot_alfa_subdistr()

ggsave(
  filename = "output/figures/02_literacy_by_subdistrict.png",
  plot = plot_alfa_subdistr(),
  width = 8,
  height = 5,
  dpi = 300
)

# Literacy distribution

alfa_distri <- df |>
  count(alfabetizada, name = "N") |>
  mutate(
    share = N / sum(N),
    share_percent = round(share * 100, 2),
    literacy_status = ifelse(
      alfabetizada == 1,
      "Literate",
      "Not literate"
    )
  )

print(alfa_distri)

plot_alfa_distri <- function() {
  
  ggplot(
    alfa_distri,
    aes(x = literacy_status, y = N)
  ) +
    geom_col(fill = "steelblue") +
    geom_text(
      aes(label = paste0(share_percent, "%")),
      vjust = -0.4,
      size = 4
    ) +
    labs(
      title = "Literacy distribution in the population",
      subtitle = "Macaé population dataset",
      x = "Literacy status",
      y = "Number of individuals"
    ) +
    theme_minimal()
}

plot_alfa_distri()

ggsave(
  filename = "output/figures/02_literacy_distribution.png",
  plot = plot_alfa_distri(),
  width = 8,
  height = 5,
  dpi = 300
)

# Comparison of potential stratification/cluster variables  

# for stratification we need homogeneity inside the groups and 
# heterogeneity between the groups for a better sampling
#
# Stratification: 
# low within + high between variances
# similar observations within the group
# groups different between 
#
# Clusters: 
# high within + low between variances
# difference observations within the group
# similar groups between 
 
# Variance decomposition by grouping variable

variance_decomposition <- function(data, group_var, group_name) {
  
  group_table <- data |>
    filter(
      !is.na({{ group_var }}),
      !is.na(alfabetizada)
    ) |>
    group_by(group = {{ group_var }}) |>
    summarise(
      Nh = n(),
      group_literacy_rate = mean(alfabetizada, na.rm = TRUE),
      within_variance_h = mean(
        (alfabetizada - mean(alfabetizada, na.rm = TRUE))^2,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    mutate(
      Wh = Nh / sum(Nh)
    )
  
  overall_mean <- weighted.mean(
    group_table$group_literacy_rate,
    group_table$Nh
  )
  
  within_variance <- sum(
    group_table$Wh * group_table$within_variance_h,
    na.rm = TRUE
  )
  
  between_variance <- sum(
    group_table$Wh *
      (group_table$group_literacy_rate - overall_mean)^2,
    na.rm = TRUE
  )
  
  total_variance <- within_variance + between_variance
  
  data.frame(
    grouping_variable = group_name,
    number_of_groups = nrow(group_table),
    smallest_group_size = min(group_table$Nh),
    largest_group_size = max(group_table$Nh),
    overall_literacy_rate = overall_mean,
    within_variance = within_variance,
    between_variance = between_variance,
    total_variance = total_variance,
    within_share = within_variance / total_variance,
    between_share = between_variance / total_variance
  )
}

# Compare possible stratification and cluster variable

variance_comparison <- bind_rows(
  variance_decomposition(df, distrito, "district"),
  variance_decomposition(df, subdistrito, "subdistrict"),
  variance_decomposition(df, bairro, "neighborhood")
)

print(variance_comparison)

write.csv(
  variance_comparison,
  file = "output/tables/02_variance_decomposition.csv",
  row.names = FALSE
)

# within vs between variance share

variance_comparison_long <- variance_comparison |>
  select(grouping_variable, within_share, between_share) |>
  tidyr::pivot_longer(
    cols = c(within_share, between_share),
    names_to = "variance_component",
    values_to = "share"
  )

plot_variance_decomposition <- function() {
  
  ggplot(
    variance_comparison_long,
    aes(
      x = grouping_variable,
      y = share,
      fill = variance_component
    )
  ) +
    geom_col(position = "stack") +
    coord_flip() +
    labs(
      title = "Variance decomposition by grouping variable",
      subtitle = "Within-group and between-group shares of literacy variance",
      x = "Grouping variable",
      y = "Share of total variance",
      fill = "Variance component"
    ) +
    theme_minimal()
}

plot_variance_decomposition()

ggsave(
  filename = "output/figures/02_variance_decomposition.png",
  plot = plot_variance_decomposition(),
  width = 8,
  height = 5,
  dpi = 300
)