###########
## Packages
###########
library(ggdag)

################################
## Start with exposure → outcome
################################
# 1. Position variables in x-y coordinates (physical load = p_load, low back pain = lbp)
coords1 <- list(
  x = c(p_load = 1, lbp = 3),
  y = c(p_load = 0, lbp = 0)
)

# 2. Define casual structures
g1 <- dagify(
  lbp ~ p_load,
  
  # set position of variables
  coords = coords1,
  
  # label variables
  labels = c(
    "lbp" = "LB pain",
    "p_load" = "Physical\n load"  
  ),
  
  # define exposure and outcome
  exposure = "p_load",
  outcome = "lbp"
)

########
## DAG 1
########
ggdag(g1, text = FALSE, use_labels = "label") +
  theme_dag()


################################
## Add socio-demographic factors
################################
# 1. Position variables in x-y coordinates
coords2 <- list(
  x = c(age = -2.5, gender = -2.5, education = -2, income = -2, 
        p_load = 1, lbp = 3),
  y = c(age = -0.25, gender = -1, education = 0.5, income = 1, 
        p_load = 0, lbp = 0)
)

# 2. Define casual structures
g2 <- dagify(
  lbp ~ p_load + age + gender + education + income,
  p_load ~ age + gender + education,
  education ~ gender,
  income ~ age + gender + education,
  coords = coords2,
  labels = c(
    "lbp" = "LB pain",
    "p_load" = "Physical\n load",
    "age" = "Age",
    "gender" = "Gender",
    "education" = "Education",
    "income" = "Income"
  ),
  exposure = "p_load",
  outcome = "lbp"
)

########
## DAG 2
########
ggdag(g2, text = FALSE, use_labels = "label") +
  theme_dag()


########################
## Add lifestyle factors
########################
# 1. Position variables in x-y coordinates
coords3 <- list(
  x = c(age = -2.5, gender = -2.5, education = -2, income = -2, 
        drink = -1, smoke = -1, sleep = -0.5, pa = -1, 
        p_load = 1, lbp = 3),
  y = c(age = -0.25, gender = -1, education = 0.5, income = 1, 
        drink = 1, smoke = 0.5, sleep = -0.5, pa = -1, 
        p_load = 0, lbp = 0)
)

# 2. Define causal structures
g3 <- dagify(
  lbp ~ p_load + age + gender + education + income + drink + smoke + sleep + pa,
  p_load ~ age + gender + education,
  education ~ gender,
  income ~ age + gender + education,
  drink ~ age + gender + education + income,
  smoke ~ age + gender + education + income,
  sleep ~  age + gender + education + income + drink + smoke + pa,
  pa ~ age + gender + education + income,
  
  coords = coords3,
  labels = c(
    "lbp" = "LB pain",
    "p_load" = "Physical\n load",
    "age" = "Age",
    "gender" = "Gender",
    "education" = "Education",
    "drink" = "Drinking",
    "smoke" = "Smoking",
    "sleep" = "Sleep\n quality",
    "pa" = "Physical\n activity",
    "income" = "Income"
  ),
  exposure = "p_load",
  outcome = "lbp"
)

#########
## DAG 3
#########
ggdag(g3, text = FALSE, use_labels = "label") +
  theme_dag()


##############################
## Add the remaining variables
##############################
# 1. Position variables in x-y coordinates
coords4 <- list(
  x = c(age = -2.5, gender = -2, education = -2, income = -2, 
        drink = -1, smoke = -1, sleep = -0.5, pa = -1, 
        bmi = 0, job_sat = 2, depress = 2, int_stress = 1, 
        p_load = 1, lbp = 3),
  y = c(age = -0.25, gender = -1, education = 0.5, income = 1, 
        drink = 1, smoke = 0.5, sleep = -0.5, pa = -1, 
        bmi = -1.25, job_sat = -1, depress = 1, int_stress = -0.75, 
        p_load = 0, lbp = 0)
)

# 2. Define causal structures
g4 <- dagify(
  lbp ~ p_load + age + gender + education + income + drink + smoke + sleep + pa + bmi + job_sat + depress + int_stress,
  p_load ~ age + gender + education,
  education ~ gender,
  income ~ age + gender + education,
  drink ~ age + gender + education + income,
  smoke ~ age + gender + education + income,
  sleep ~  age + gender + education + income + drink + smoke + pa,
  pa ~ age + gender + education + income,
  bmi ~ age + gender + education + income + drink + smoke + sleep + pa,
  job_sat ~ p_load + int_stress,
  depress ~ p_load + int_stress,
  int_stress ~ age + gender + education,
  
  coords = coords4,
  labels = c(
    "lbp" = "LB pain",
    "p_load" = "Physical\n load",
    "age" = "Age",
    "gender" = "Gender",
    "education" = "Education",
    "drink" = "Drinking",
    "smoke" = "Smoking",
    "sleep" = "Sleep\n quality",
    "pa" = "Physical\n activity",
    "income" = "Income",
    "bmi" = "BMI",
    "job_sat" = "Job\n satisfaction",
    "depress" = "Depression",
    "int_stress" = "Interpersonal\n stress"
  ),
  exposure = "p_load",
  outcome = "lbp"
)

########
## DAG 4
########
ggdag(g4, text = FALSE, use_labels = "label") +
  theme_dag()

################################
## Identifying an adjustment set
################################
ggdag_adjustment_set(g4, text = FALSE, use_labels = "label") +
  theme_dag()


###########################
## Add unmeasured variables
########################### 
# 1. Position variables in x-y coordinates
coords5 <- list(
  x = c(
    prev_behav = -3.2,
    age = -2.5, gender = -2, education = -2, income = -2,
    drink = -1, smoke = -1, sleep = -0.5, pa = -1, 
    bmi = 0, job_sat = 2, depress = 2, int_stress = 1, 
    p_load = 1, lbp = 3
  ),
  y = c(
    prev_behav = -1.8,
    age = -0.25, gender = -1, education = 0.5, income = 1,
    drink = 1, smoke = 0.5, sleep = -0.5, pa = -1, 
    bmi = -1.25, job_sat = -1, depress = 1, int_stress = -0.75, 
    p_load = 0, lbp = 0
  )
)

# 2. Define causal structures
g5 <- dagify(
  lbp ~ p_load + age + gender + education + income + drink + smoke + sleep + pa + bmi + job_sat + depress + int_stress,
  p_load ~ age + gender + education + drink + smoke + bmi,
  education ~ gender,
  income ~ age + gender + education,
  drink ~ age + gender + education + income + prev_behav,
  smoke ~ age + gender + education + income + prev_behav,
  sleep ~ age + gender + education + income + drink + smoke + pa,
  pa ~ age + gender + education + income + prev_behav,
  bmi ~ age + gender + education + income + drink + smoke + sleep + pa + prev_behav,
  job_sat ~ p_load + int_stress,
  depress ~ p_load + int_stress,
  int_stress ~ age + gender + education,
  
  coords = coords5,
  labels = c(
    "prev_behav" = "U: Previous PA\n / health behaviour",
    "lbp" = "LB pain",
    "p_load" = "Physical\nload",
    "age" = "Age",
    "gender" = "Gender",
    "education" = "Education",
    "drink" = "Drinking",
    "smoke" = "Smoking",
    "sleep" = "Sleep\nquality",
    "pa" = "Physical\nactivity",
    "income" = "Income",
    "bmi" = "BMI",
    "job_sat" = "Job\nsatisfaction",
    "depress" = "Depression",
    "int_stress" = "Interpersonal\nstress"
  ),
  exposure = "p_load",
  outcome = "lbp",
  latent = "prev_behav"
)

########
## DAG 5
########
ggdag(g5, text = FALSE, use_labels = "label") +
  theme_dag()


################################
## Identifying an adjustment set
################################
ggdag_adjustment_set(g5, text = FALSE, use_labels = "label") +
  theme_dag()

