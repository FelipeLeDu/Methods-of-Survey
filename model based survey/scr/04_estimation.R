# 04_estimation.R 
#
# best AIC model: y ~ cor_raca + escolaridade + idade

modelo <- glmer(
  y_contra_aborto ~ cor_raca + escolaridade + idade + (1 | uf),
  data   = survey,
  family = binomial(link = "logit") # logit model
)

summary(modelo)

