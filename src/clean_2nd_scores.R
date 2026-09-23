# ============================================================
# Clean / restore 2nd evaluation dataset
# ============================================================

library(tidyverse)

input_file  <- "data/raw/raw_2nd_evaluation_scores.csv"
output_file <- "data/cleaned/cleaned_2nd_evaluation_scores.csv"

actual_missing_file <-
  "data/cleaned/missing_values_2nd_evaluation.csv"

actual_missing_summary_file <-
  "data/cleaned/missing_summary_2nd_evaluation.csv"

dir.create(
  dirname(output_file),
  recursive = TRUE,
  showWarnings = FALSE
)

df <- read_csv(
  input_file,
  na = c("", "NA"),
  show_col_types = FALSE
)


# ------------------------------------------------------------
# 2. Define columns
# ------------------------------------------------------------

training_data_col <- "Training-Data Resemblance"
artifact_col <- "Artifact Score"
requested_condition_col <- "requested condition (base model compare)"
aesthetic_col <- "aesthetic (base model compare)"
ignore_negative_col <- "ignore negative"

explicit_default_cols <- c(
  training_data_col,
  artifact_col,
  requested_condition_col,
  aesthetic_col,
  ignore_negative_col
)


# ------------------------------------------------------------
# 4. Normalisation helpers
#
# Column names have inconsistent capitalisation and spacing,
# so comparisons are done using a normalised version.
# ------------------------------------------------------------

normalise_name <- function(x) {
  x %>%
    str_to_lower() %>%
    str_squish()
}

normalised_col_names <- normalise_name(names(df))
normalised_prompt <- normalise_name(df$Prompt)


# ------------------------------------------------------------
# 5. Map each Prompt to its metric-column prefix
#
# Example:
#   "Winter Coat" -> columns beginning with "winter coat - "
#   "Dutch angle" -> columns beginning with "dutch - "
# ------------------------------------------------------------

prompt_prefixes <- c(
  "baseline"    = "baseline",
  "winter coat" = "winter coat",
  "watercolor"  = "watercolor",
  "mecha"       = "mecha",
  "dutch angle" = "dutch",
  "purple hair" = "purple hair",
  "abstract"    = "abstract"
)


# ------------------------------------------------------------
# 6. Detect ACTUAL missing evaluator scores BEFORE restoring
#    defaults
#
# An actual missing value occurs when:
#
#   - the metric belongs to the current row's Prompt type
#   - AND its value is blank/NA OR -2
#
# -2 in a metric belonging to another prompt type is NOT
# considered an evaluator omission; that is simply the
# normal "metric does not apply" value.
# ------------------------------------------------------------

actual_missing <- map_dfr(seq_len(nrow(df)), function(i) {

  current_prompt <- normalised_prompt[i]
  current_prefix <- prompt_prefixes[[current_prompt]]

  # Find metrics belonging to this row's prompt.
  applicable_cols <- names(df)[
    str_starts(
      normalised_col_names,
      paste0(current_prefix, " - ")
    )
  ]

  if (length(applicable_cols) == 0) {
    return(tibble())
  }

  tibble(
    Row = i,
    Grid = df$Grid[i],
    Step = df$Step[i],
    Prompt = df$Prompt[i],
    Metric = applicable_cols,
    OriginalValue = as.numeric(
      unlist(df[i, applicable_cols], use.names = FALSE)
    )
  ) %>%
    filter(
      is.na(OriginalValue) |
        OriginalValue == -2
    ) %>%
    mutate(
      MissingType = case_when(
        is.na(OriginalValue) ~ "blank/NA",
        OriginalValue == -2 ~ "-2",
        TRUE ~ NA_character_
      )
    )
})


# ------------------------------------------------------------
# 7. Write actual-missing detail + summary
# ------------------------------------------------------------

if (nrow(actual_missing) > 0) {

  write_csv(
    actual_missing,
    actual_missing_file,
    na = "NA"
  )

  actual_missing_summary <- actual_missing %>%
    count(
      Prompt,
      Metric,
      MissingType,
      name = "MissingCount"
    ) %>%
    arrange(
      Prompt,
      Metric,
      MissingType
    )

  write_csv(
    actual_missing_summary,
    actual_missing_summary_file,
    na = "NA"
  )

  cat("\n============================================================\n")
  cat("ACTUAL MISSING VALUES DETECTED\n")
  cat("============================================================\n\n")

  print(actual_missing_summary)

  cat("\nDetailed missing-value file:\n")
  cat(actual_missing_file, "\n")

  cat("\nSummary file:\n")
  cat(actual_missing_summary_file, "\n\n")

} else {

  # Still create an empty summary file with useful columns.
  actual_missing_summary <- tibble(
    Prompt = character(),
    Metric = character(),
    MissingType = character(),
    MissingCount = integer()
  )

  write_csv(
    actual_missing_summary,
    actual_missing_summary_file,
    na = "NA"
  )

  cat("\nNo actual evaluator-missing values detected.\n\n")
}


# ------------------------------------------------------------
# 8. Convert -2 to NA
#
# -2 means the metric does not apply to the row.
#
# IMPORTANT:
# The actual missing values detected above are also left as NA.
# ------------------------------------------------------------

df <- df %>%
  mutate(
    across(
      where(is.numeric),
      ~ ifelse(.x == -2, NA_real_, .x)
    )
  )


# ------------------------------------------------------------
# 9. Restore exporter-suppressed explicit defaults
#
# These columns have real defaults:
#
# Training-Data Resemblance          = 0
# Artifact Score                    = 0
# requested condition (...)         = -1
# aesthetic (...)                   = -1
# ignore negative                   = 0
#
# Blank cells in these columns therefore mean that the exporter
# omitted the default value.
# ------------------------------------------------------------

df[[training_data_col]][
  is.na(df[[training_data_col]])
] <- 0

df[[artifact_col]][
  is.na(df[[artifact_col]])
] <- 0

df[[requested_condition_col]][
  is.na(df[[requested_condition_col]])
] <- -1

df[[aesthetic_col]][
  is.na(df[[aesthetic_col]])
] <- -1

df[[ignore_negative_col]][
  is.na(df[[ignore_negative_col]])
] <- 0


# ------------------------------------------------------------
# 10. Explicitly preserve actual evaluator omissions
#
# This is technically redundant after the -2 -> NA conversion,
# but makes the intended behaviour explicit and protects these
# cells from any later default-restoration logic.
# ------------------------------------------------------------

if (nrow(actual_missing) > 0) {

  for (j in seq_len(nrow(actual_missing))) {

    row_idx <- actual_missing$Row[j]
    metric  <- actual_missing$Metric[j]

    df[[metric]][row_idx] <- NA_real_
  }
}


# ------------------------------------------------------------
# 11. Combine base-character identity metrics
#
# These are currently split across prompt-specific columns:
#
#   blue hair
#   solo 1 girl
#   twintails
#
# We combine them into:
#
#   blue_hair
#   solo_1_girl
#   twintails
#
# Column-name matching is case/spacing insensitive.
#
# Mecha and Dutch angle have no twintails metric. Their
# combined twintails value therefore correctly remains NA.
# ------------------------------------------------------------

# finding strings
identity_source_cols <- list(

  blue_hair = names(df)[
    str_detect(
      normalised_col_names,
      " - blue\\s*hair$"
    )
  ],

  solo_1_girl = names(df)[
    str_detect(
      normalised_col_names,
      " - solo(?: and)?\\s*1\\s*girl$"
    )
  ],

  twintails = names(df)[
    str_detect(
      normalised_col_names,
      " - twintails$"
    )
  ]
)


# A = NA, B =1, will take B's value, both NA, will return NA, if A have a value, take A value 
combine_identity_columns <- function(data, source_cols) {

  if (length(source_cols) == 0) {
    return(rep(NA_real_, nrow(data)))
  }


  do.call(
    dplyr::coalesce,
    unname(data[source_cols])
  )
}


df <- df %>%
  mutate(
    blue_hair = combine_identity_columns(
      .,
      identity_source_cols$blue_hair
    ),

    solo_1_girl = combine_identity_columns(
      .,
      identity_source_cols$solo_1_girl
    ),

    twintails = combine_identity_columns(
      .,
      identity_source_cols$twintails
    )
  )


# ------------------------------------------------------------
# 12. Remove the original prompt-specific identity columns
# ------------------------------------------------------------

identity_cols_to_remove <- unique(
  unlist(identity_source_cols)
)

df <- df %>%
  select(
    -all_of(identity_cols_to_remove)
  )


# ------------------------------------------------------------
# 13. Expand Grid into:
#
#   seed
#   rank
#   learning_rate
#
# Examples:
#
#   "42 8"         -> seed = 42, rank = 8,  lr = 1e-4
#   "61728416 16"  -> seed = 61728416, rank = 16, lr = 1e-4
#   "123456789 32" -> seed = 123456789, rank = 32, lr = 1e-4
#   "42 16e2"      -> seed = 42, rank = 16, lr = 2e-4
#
# "16e2" means rank 16 with learning rate 2e-4.
# ------------------------------------------------------------

# tries to get 2 numbers that may or may not have an extra space or e2
grid_match <- str_match(
  str_squish(df$Grid),          #clean up weird whitespace before matching
  "^(\\d+)\\s+(\\d+)(e2)?$"    # one or more digits + One or more spaces + One or more digits + optional "e2" at the end
)


# seed is first number, rank is second number, learning rate is 2e-4 if "e2" was present, else 1e-4
df <- df %>%
  mutate(
    seed = as.integer(grid_match[, 2]),
    rank = as.integer(grid_match[, 3]),

    learning_rate = if_else(
      !is.na(grid_match[, 4]),
      2.00E-04,
      1.00E-04
    )
  )


df <- df %>%      #remove the original Grid, PromptIndex, and Notes columns
  select(
    -Grid,
    -PromptIndex,
    -Notes
  )


# reorder cols for consistency with 1st evaluation dataset
df <- df %>%
  select(
    learning_rate,
    rank,
    seed,
    Prompt,
    Step,
    blue_hair,
    solo_1_girl,
    twintails,
    everything()
  )

# rename cols for consistency with 1st evaluation dataset
# most col names are shortened to remove the prompt-specific prefix, e.g. "Winter Coat - coat" -> "coat"
df <- df %>%
  rename(
    prompt = "Prompt",
    step = "Step",
    training_data_resemblance = "Training-Data Resemblance",
    artifact_score = "Artifact Score",
    base_condition = "requested condition (base model compare)",
    base_aesthetic = "aesthetic (base model compare)",
    coat = "Winter Coat - coat",
    scarf = "Winter Coat - scarf",
    watercolor_style = "watercolor - watercolor appearance",
    pastel = "watercolor - pastel color existence",
    brush_strokes = "watercolor - brush strokes existence",
    space_background = "mecha - space background",
    mecha = "mecha - mecha present",
    piloting = "mecha - character inside, piloting",
    color_bleed = "mecha - Light Reflection (Color Bleed): Does the blue space glow onto the ship and her hair",
    non_cel = "mecha - not cel coloring",
    dutch_angle = "dutch - camera tilted",
    purple_hair = "Purple hair - purple hair",
    purple_eyes = "Purple hair - purple eyes",
    red_shirt = "Purple hair - red shirt",
    abstract = "Abstract - either character or background abstract",
    negative_violation = "ignore negative"
  )

# normalised prompt names for consistency with other prompt names
df <- df %>% 
  mutate(
    prompt = case_match(prompt, "Dutch angle" ~ "Dutch Angle", .default = prompt)
  )


write_csv(
  df,
  output_file,
  na = "NA"
)