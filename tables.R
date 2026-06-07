library(needs)
needs(tidyverse,parameters)

#group model
## short-term

##gini
###long
gini_tab_param <- model_parameters(
  AIC_min_model_gini,
  effects = "fixed",
  ci = 0.95,
  ci_method = "wald",
  exponentiate = FALSE
)

print(
  gini_tab_param,
  digits = 3,
  ci_digits = 3,
  p_digits = 3
)

## short

gini_tab_param <- model_parameters(
  AIC_min_model_gini_short_term,
  effects = "fixed",
  ci = 0.95,
  ci_method = "wald",
  exponentiate = FALSE
)

print(
  gini_tab_param,
  digits = 3,
  ci_digits = 3,
  p_digits = 3
)

## num of successful
### long
nsf_tab_param <- model_parameters(
  AIC_min_model_nsf,
  effects = "fixed",
  ci = 0.95,
  ci_method = "wald",
  exponentiate = FALSE
)

print(
  nsf_tab_param,
  digits = 3,
  ci_digits = 3,
  p_digits = 3
)

### short

nsf_tab_param <- model_parameters(
  AIC_min_model_nsf_short_term,
  effects = "fixed",
  ci = 0.95,
  ci_method = "wald",
  exponentiate = FALSE
)

print(
  nsf_tab_param,
  digits = 3,
  ci_digits = 3,
  p_digits = 3
)
## foraging duration
### long
fd_tab_param <- model_parameters(
  AIC_min_model_fd,
  effects = "fixed",
  ci = 0.95,
  ci_method = "wald",
  exponentiate = FALSE
)

print(
  fd_tab_param,
  digits = 3,
  ci_digits = 3,
  p_digits = 3
)

### short 

fd_tab_param <- model_parameters(
  AIC_min_model_fd_short_term,
  effects = "fixed",
  ci = 0.95,
  ci_method = "wald",
  exponentiate = FALSE
)

print(
  fd_tab_param,
  digits = 3,
  ci_digits = 3,
  p_digits = 3
)


## exploring duration
ed_tab_param <- model_parameters(
  AIC_min_model_ed,
  effects = "fixed",
  ci = 0.95,
  ci_method = "wald",
  exponentiate = FALSE
)

print(
  ed_tab_param,
  digits = 3,
  ci_digits = 3,
  p_digits = 3
)

###short

ed_tab_param <- model_parameters(
  AIC_min_model_ed_short_term,
  effects = "fixed",
  ci = 0.95,
  ci_method = "wald",
  exponentiate = FALSE
)

print(
  ed_tab_param,
  digits = 3,
  ci_digits = 3,
  p_digits = 3
)
#individual

tab_param <- model_parameters(
  AIC_min_model_sex,
  effects = "fixed",
  ci = 0.95,
  ci_method = "wald",
  exponentiate = FALSE
)


print(
  tab_param,
  digits = 3,
  ci_digits = 3,
  p_digits = 3
)

## male

male_tab_param <- model_parameters(
  male_model_block,
  effects = "fixed",
  ci = 0.95,
  ci_method = "wald",
  exponentiate = FALSE
)


print(
  male_tab_param,
  digits = 3,
  ci_digits = 3,
  p_digits = 3
)

## female

female_tab_param <- model_parameters(
  female_model_block,
  effects = "fixed",
  ci = 0.95,
  ci_method = "wald",
  exponentiate = FALSE
)


print(
  female_tab_param,
  digits = 3,
  ci_digits = 3,
  p_digits = 3
)
