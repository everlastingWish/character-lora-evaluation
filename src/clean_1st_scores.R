# ============================================================
# Clean 1nd evaluation dataset
# ============================================================

library(tidyverse)

input_file  <- "data/raw/raw_1st_evaluation_scores.csv"
output_file <- "data/cleaned/cleaned_1st_evaluation_scores.csv"

df <- read_csv(
  input_file,
  na = c("", "NA"),
  show_col_types = FALSE
)

# clean col names by
# normalise col names for 2nd evaluation dataset
# make col names more precise 
df <- df %>%
  rename(
    learning_rate = "learning rate",
    prompt = "prompt type",
    step = "steps",
    aesthetic_old = "Aesthetic Score (0-2)",
    anatomy_old = "Anatomy Score (2-0)",
    ornament_text = "Text Score (3-0)",
    prompt_adherence_old = "Prompt Adherence (1-0)",
    overfit_old= "overfit (0->3)"
  )

# match prompt names to be consistent with 2nd evaluation dataset
df <- df %>% 
  mutate(
    prompt = case_match(prompt, "Dutch Angle Env" ~ "Dutch Angle", .default = prompt)
  )

write_csv(
  df,
  output_file,
  na = "NA"
)