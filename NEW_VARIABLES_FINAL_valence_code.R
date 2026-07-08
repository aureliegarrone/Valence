library(readxl)
valence_polarity_norms <- read_excel("C:/Users/aurel/OneDrive/Documents/Valence/final_valence_polarity_norms.xlsx")
All_variables <- read_excel("C:/Users/aurel/Downloads/OpenLexicon.xlsx")
library(dplyr)
All_variables <- All_variables %>%
  rename(word_name = ortho)

All_variables <- All_variables %>%
  rename(RT = RT...10)
All_variables_val_pol <- merge(All_variables[, c("word_name", "Imageability", "Frequency", "RT", "OLD20", "Nlett")], valence_polarity_norms[, c("word_name", "mean_polarity", "mean_valence")], by = "word_name")
All_variables_val_pol <- distinct(All_variables_val_pol)

mean_val <- mean(valence_polarity_norms$mean_valence, na.rm = TRUE)
sd_val   <- sd(valence_polarity_norms$mean_valence, na.rm = TRUE)

All_variables_val_pol <- All_variables_val_pol %>%
   mutate(
         valence_cat = case_when(
             mean_valence < mean_val - 1.5 * sd_val ~ "negative",
             mean_valence > mean_val + 1.5 * sd_val ~ "positive",
             TRUE ~ "neutral"))

model_val_cont <- summary(lm(RT ~ mean_valence + Frequency + Nlett + Imageability, data = All_variables_val_pol))
model_pol_cont <- summary(lm(RT ~ mean_polarity + Frequency + Nlett + Imageability, data = All_variables_val_pol))

All_variables_val_pol$valence_cat <- as.factor(All_variables_val_pol$valence_cat)
All_variables_val_pol$valence_cat <- relevel(All_variables_val_pol$valence_cat, ref = "neutral")

model_val_cat <- summary(lm(RT ~ valence_cat + Frequency + Nlett + Imageability, data = All_variables_val_pol))

#POSITIVE AND NEGATIVE WORDS ONLY 
All_variables_val_pol_no_neutral <- All_variables_val_pol %>%
  filter(valence_cat != "neutral")

model_val_cat_no_neutral <- summary(lm(RT ~ valence_cat + Frequency + Nlett + Imageability, data = All_variables_val_pol_no_neutral))

All_variables_val_pol_no_neutral$valence_cat <- relevel(All_variables_val_pol_no_neutral$valence_cat, ref = "positive")

