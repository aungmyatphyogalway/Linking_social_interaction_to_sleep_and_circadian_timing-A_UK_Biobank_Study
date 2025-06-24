#23 June 2025
#Aung
#Latent class analysis (LCA) of social situation questionnaires
setwd("~/Downloads")
#required packages
#install.packages("poLCA")
library(poLCA)
#install.packages("dplyr")
library(dplyr)

#required dataset (social_variables_before_lca.csv)
#change your directory
load("/Users/aungphyo/Downloads/social_sleep_data_files/social_variables_before_lca.Rda")
social<- social_before_lca

#latent class analysis
social <- social %>%
  mutate(
    loneliness_cat_nu = as.numeric(factor(loneliness_cat,
                                          levels = c("Low", "Mid", "High"))),
    resilience_to_stress_cat_nu = as.numeric(factor(resilience_to_stress_cat,
                                                    levels = c("Very low", "Low", "Mid", "High", "Very high"))),
    social_connectivity_cat_nu = as.numeric(factor(social_connectivity_cat,
                                                 levels = c("Very low",
                                                            "Low",
                                                            "Moderate",
                                                            "Good",
                                                            "High",
                                                            "Very High"))))

f_testing <- as.formula(cbind(loneliness_cat_nu, resilience_to_stress_cat_nu, social_connectivity_cat_nu)~1)
data<- social

# Initialize storage
results <- list()
bic_vals <- c()
aic_vals <- c()
llik_vals <- c()
entropy_vals <- c()
n_classes <- 1:6

# Fit models from 1 to 6 classes
set.seed(123)  # for reproducibility
for (k in n_classes) {
  cat("Fitting model with", k, "classes...\n")
  model <- poLCA(f_testing, data = data, nclass = k, maxiter = 5000, nrep = 15, verbose = FALSE)
  results[[k]] <- model
  bic_vals[k] <- model$bic
  aic_vals[k] <- model$aic
  llik_vals[k] <- model$llik
}

# Function to calculate normalized entropy
compute_entropy <- function(posterior) {
  N <- nrow(posterior)
  K <- ncol(posterior)
  # Avoid log(0) by adding a small constant
  entropy_raw <- -sum(posterior * log(posterior + 1e-10)) / N
  max_entropy <- log(K)
  norm_entropy <- 1 - (entropy_raw / max_entropy)
  return(norm_entropy)
}

# Compute entropy for each model in your results list
entropy_vals <- numeric(length(results))

for (i in seq_along(results)) {
  model <- results[[i]]
  if (!is.null(model$posterior)) {
    entropy_vals[i] <- compute_entropy(model$posterior)
  } else {
    entropy_vals[i] <- NA
  }
}

# Combine into summary data frame
summary_df <- data.frame(
  Classes = n_classes,
  BIC = bic_vals,
  AIC = aic_vals,
  LogLikelihood = llik_vals,
  Entropy = entropy_vals
)
# Print summary
print(summary_df)
#Classes       BIC       AIC LogLikelihood   Entropy
#1       1 1030616.6 1030506.8     -515242.4       Inf
#2       2  999938.8  999709.2     -499831.6 0.7470253
#3       3  997739.4  997390.0     -498660.0 0.7172114
#4       4  997726.3  997257.2     -498581.6 0.5367194
#5       5  997842.3  997253.3     -498567.7 0.5163270
#6       6  997975.4  997266.7     -498562.3 0.4525512

#comparison between model 3 and model 4
# Pull models
model3 <- results[[3]]
model4 <- results[[4]]

# Likelihood ratio test (G²)
G2 <- 2 * (model4$llik - model3$llik)
df_diff <- abs(model4$resid.df - model3$resid.df)
p_value <- pchisq(G2, df = df_diff, lower.tail = FALSE)

# Print test result
cat("G² =", round(G2, 2), "| df =", df_diff, "| p-value =", round(p_value, 4), "\n")
#G² = 156.88 | df = 12 | p-value = 0


#model 3 and 4
model3[["P"]]
#[1] 0.25510877 0.69314422 0.05174701

model4[["P"]]
#[1] 0.26427816 0.50326061 0.03800104 0.19446019

##choose model 3
#model3$probs
#$loneliness_cat_nu
#Pr(1)       Pr(2)        Pr(3)
#class 1:  3.549456e-01 0.645054428 6.032997e-20
#class 2:  9.910939e-01 0.008553991 3.520997e-04
#class 3:  7.041256e-22 0.288719394 7.112806e-01

#$resilience_to_stress_cat_nu
#Pr(1)      Pr(2)     Pr(3)     Pr(4)       Pr(5)
#class 1:  0.0024788249 0.12111893 0.5362302 0.3240312 0.016140854
#class 2:  0.0001537002 0.02003345 0.3087712 0.5695332 0.101508520
#class 3:  0.0206386288 0.31100090 0.5154068 0.1451231 0.007830592

#$social_connectivity_cat_nu
#Pr(1)      Pr(2)     Pr(3)     Pr(4)      Pr(5)      Pr(6)
#class 1:  0.033843876 0.12540600 0.2502710 0.4012113 0.15417820 0.03508964
#class 2:  0.007561593 0.03698461 0.1262891 0.4039075 0.31520098 0.11005621
#class 3:  0.146530207 0.25515761 0.2807160 0.2400517 0.06135188 0.01619256

#data need for LCA radar plot
#model3[["probs.start"]][[1]]
#.       [,1]      [,2]      [,3]
#[1,] 0.38814550 0.1599908 0.4518637
#[2,] 0.47143206 0.2726482 0.2559198
#[3,] 0.04357134 0.2805564 0.6758722

#model3[["probs.start"]][[2]]
#.       [,1]      [,2]      [,3]      [,4]      [,5]
#[1,] 0.06625836 0.3139397 0.3737404 0.0588637 0.1871978
#[2,] 0.06171034 0.2767387 0.2862518 0.1062093 0.2690898
#[3,] 0.12585233 0.2796113 0.2464948 0.1012071 0.2468345

#model3[["probs.start"]][[3]]
#.       [,1]      [,2]        [,3]      [,4]       [,5]       [,6]
#[1,] 0.097332292 0.2461129 0.076502940 0.3442240 0.08252241 0.15330542
#[2,] 0.257954390 0.2555156 0.005907917 0.2939119 0.15406359 0.03264665
#[3,] 0.006491699 0.1935090 0.118752517 0.2275505 0.33602063 0.11767564

social_before_lca$lca_num<- model3[["predclass"]]

social_before_lca <- social_before_lca %>%
  mutate(latent_class_final = case_when(
    lca_num == 1 ~ "Mid loneliness, mid resilience, mid social",
    lca_num == 2 ~ "Low loneliness, high resilience, very high social",
    lca_num == 3 ~ "High loneliness, low resilience, low social",
    TRUE ~ NA_character_  # Handles any unexpected values
  ))

rm(aic_vals,bic_vals,df_diff,entropy_vals,f_testing,G2, i, k, llik_vals, n_classes,p_value,posterior)

social_final<- social_before_lca
#save dataset
save(social_final, file="social_final.Rda")
##create csv file
write.csv(social_final, file="social_final.csv", row.names= FALSE)
