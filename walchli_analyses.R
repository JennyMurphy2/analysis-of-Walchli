library(rstatix)
library(afex)
library(car)
library(broom)
library(emmeans)
library(stringr)
library(lmerTest)
library(tidyverse)
library(janitor)
library(MOTE)

set.seed(21)

# Import and prepare data 

rep_data <- read_csv("jump_reward_data.csv") 

# Replication data ----------------

jump_data <- rep_data %>%
  select(subject, jumpheight_ne, jumpheight_af, jumpheight_re, jumpheight_afef, jumpheight_afre, jumpheight_afefre)

# convert to long data set
long_rep_data <- jump_data %>%
  pivot_longer(cols = starts_with("jump"),
               names_to = "condition",
               values_to = "jump_height") 

long_rep_data$condition <- as.factor(long_rep_data$condition)

# Descriptives --------

# mean and sd of data
summary_rep <- long_rep_data %>% 
  group_by(condition) %>%
  summarize(count = n(),
            mean = mean(jump_height, na.rm=TRUE),
            sd = sd(jump_height, na.rm = TRUE))
summary_rep 

## ANOVA -----
# RM ANOVA 

##afex::aov_4(continuous_var ~ group_var + (RM_var|id_var)

anova_results_rep <- afex::aov_4(jump_height ~ (condition|subject), 
                                   data = long_rep_data,
                                   anova_table = list(es = "pes")) # partial eta squared
anova_results_rep

summary(anova_results_rep)

## Assumptions --------

### Normality test -------

long_rep_data %>% 
  dplyr::group_by(condition) %>% 
  rstatix::shapiro_test(jump_height) # shapiro-wilk test on individual groups

### Outliers check --------

long_rep_data %>%
  group_by(condition) %>%
  identify_outliers(jump_height)

## Plots ---------

## violin

long_rep_data %>% 
  ggplot(aes(condition, jump_height)) +  
  geom_violin(fill = "gray") +
  geom_boxplot(width = .07,
               fill = "white") +
  geom_jitter(position = position_jitter(0.21)) +
  stat_summary(fun = mean,
               geom = "point",
               shape = 18,
               color = "red",
               size = 5) +
  theme_bw()


# qq plot

long_rep_data %>% 
  ggplot(aes(sample = jump_height)) +    
  geom_qq() +                               
  stat_qq_line() +                          
  facet_wrap(~ condition,                   # Panel by group
             labeller = label_both) +    
  theme_bw()

## Replication effect size and CI -------

pes_ci_rep <- eta.F(
  dfm = anova_results_rep$anova_table$`num Df`,
  dfe = anova_results_rep$anova_table$`den Df`,
  Fvalue = anova_results_rep$anova_table$F,
  a = 0.05) %>%
  as.data.frame() %>%
  select(eta, etalow, etahigh) %>%
  mutate(study_id = c("Replication study")) # add identifier
pes_ci_rep

# Original data ----------------

# Import and prepare data 

original_data <- read_csv("original_data.csv") %>%
  clean_names() %>%
  rename(jumpheight_ne = "ne_3_5", 
         jumpheight_af = "a_fb_sum", 
         jumpheight_re = "re_sum", 
         jumpheight_afef = "a_fb_ef_sum", 
         jumpheight_afre = "a_fb_re_sum", 
         jumpheight_afefre = "a_fb_ef_re_sum")


# convert to long data set

long_orig_data <- original_data %>%
  pivot_longer(cols = starts_with("jump"),
               names_to = "condition",
               values_to = "jump_height") 

# Descriptives

# mean and sd of data
summary_orig <- long_orig_data %>% 
  group_by(condition) %>%
  summarize(count = n(),
            mean = mean(jump_height, na.rm=TRUE),
            sd = sd(jump_height, na.rm = TRUE))
summary_orig

## ANOVA -----
# RM ANOVA 

##afex::aov_4(continuous_var ~ group_var + (RM_var|id_var)

anova_results_orig <- afex::aov_4(jump_height ~ (condition|id), 
                             data = long_orig_data,
                             anova_table = list(es = "pes")) # partial eta squared
anova_results_orig

summary(anova_results_orig)

## Assumptions --------

### Normality test -------

long_orig_data %>% 
  dplyr::group_by(condition) %>% 
  rstatix::shapiro_test(jump_height) # shapiro-wilk test on individual groups

### Outliers check --------

long_orig_data %>%
  group_by(condition) %>%
  identify_outliers(jump_height)


## Plots ---------

## violin

long_orig_data %>% 
  ggplot(aes(condition, jump_height)) +  
  geom_violin(fill = "gray") +
  geom_boxplot(width = .07,
               fill = "white") +
  geom_jitter(position = position_jitter(0.21)) +
  stat_summary(fun = mean,
               geom = "point",
               shape = 18,
               color = "red",
               size = 5) +
  theme_bw()


# qq plot

long_orig_data %>% 
  ggplot(aes(sample = jump_height)) +    
  geom_qq() +                               
  stat_qq_line() +                          
  facet_wrap(~ condition,                   # Panel by group
             labeller = label_both) +    
  theme_bw()

## Original effect size and CI -------

pes_ci_orig <- eta.F(
  dfm = 5,
  dfe = 85,
  Fvalue = anova_results_orig$anova_table$F,
  a = 0.05) %>%
  as.data.frame() %>%
  select(eta, etalow, etahigh) %>%
  mutate(study_id = c("Original study")) # add identifier
pes_ci_orig

# Replication test -----

pes_rep = anova_results_rep$anova_table$pes
df_rep = anova_results_rep$anova_table$`den Df`
pes_ori = anova_results_orig$anova_table$pes
df_ori = 85

rho_ori = 2*sqrt(pes_ori)-1
rho_rep = 2*sqrt(pes_rep)-1

rep_test = TOSTER::compare_cor(r1 = rho_ori,
                               df1 = df_ori,
                               r2 = rho_rep,
                               df2 = df_rep,
                               alternative = "greater")
rep_test

