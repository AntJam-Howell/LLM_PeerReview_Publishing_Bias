rm(list = ls())
gc()

library(DescTools)
library(rstudioapi)
library(readr)
library(tidyverse)
library(dplyr)
library(lfe)
library(stargazer)
library(broom)
library(purrr)
library(ggplot2)
library(patchwork)
library(ggpubr)
library(ggstatsplot)
library(dplyr)
library(kableExtra)
library(kableExtra)
library(gt)

########################################################################################################################
########################################################################################################################
## Data Prep
########################################################################################################################
########################################################################################################################

#Local Relative Path to downlaod Dataset
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df <- read.csv("Data/LLM_AI_Bias_review_combined_results_allPapers.csv", comment.char="#")

#Absolute path to download Dataset
#url <- "https://www.dropbox.com/scl/fi/hvaxcbvfy2ovvvcs0twv0/LLM_AI_Bias_review_combined_results_allPapers.csv?rlkey=0z5kkauhz6annmthm606x3k6l&dl=1"
#df <- read_csv(url, comment = "#")

df1<-df %>% select(-PaperName, -ID, -PaperLink,-PromptTokens,-CompletionTokens,-TotalTokens,-Prompt )
#unique(df1$LlmDetailedOutput)
unique(df1$NameUsed)
unique(df1$Race)
unique(df1$Gender)
unique(df1$Institution)
unique(df1$Pub.Outlet)

# STEP 1: Extract Editor Decision only (Desk Reject/Send for Review)
df_editor_decision <- df1 %>%
  filter(str_detect(LlmDetailedOutput, "^(Desk Reject|Send for Review)")) %>%
  mutate(
    editorDecision = str_extract(LlmDetailedOutput, "^(Desk Reject|Send for Review)"),
    EditorDecisionD = if_else(editorDecision == "Desk Reject", 1, 0)
  ) %>%
  select(PAPERID, Iterations,BlindOrNamed,  NameUsed, Institution, editorDecision,EditorDecisionD)

# STEP 2: Extract numeric-only Editor Quality Scores separately (this prompt is numeric-only)
df_editor_quality <- df1 %>%
  filter(str_detect(LlmDetailedOutput, "^\\d+$")) %>%
  mutate(EditorQualSc = as.numeric(LlmDetailedOutput)) %>%
  select(PAPERID, Iterations,BlindOrNamed,  NameUsed, Institution,EditorQualSc)

# STEP 3: Extract Reviewer Decision (Major/Minor Revision, Accept + Reviewer scores/comments)
df_reviewer <- df1 %>%
  filter(str_detect(LlmDetailedOutput, "^(Accept/Minor Revision|Major Revision|Reject)")) %>%
  mutate(
    reviewerDecision = str_extract(LlmDetailedOutput, "^(Accept/Minor Revision|Major Revision|Reject)"),
    reviewerDecisionD = if_else(reviewerDecision == "Reject", 1, 0),
    reviewerDecisionOrdered = if_else(reviewerDecision == "Reject", 2, 
                                    if_else(reviewerDecision == "Major Revision", 1, 0)),
    ReviewerQualSc = as.numeric(str_extract(LlmDetailedOutput, "(?<=\\*\\*Quality Score\\*\\* = )\\d+")),
    ReviewerConstructiveComm = as.numeric(str_extract(LlmDetailedOutput, "(?<=\\*\\*Constructive Feedback Comments Count\\*\\* = )\\d+")),
    ReviewerCriticalComm = as.numeric(str_extract(LlmDetailedOutput, "(?<=\\*\\*Critical Feedback Comments Count\\*\\* = )\\d+"))
  ) %>%
  select(PAPERID, Iterations, BlindOrNamed, NameUsed, Institution,reviewerDecision, reviewerDecisionD, reviewerDecisionOrdered,ReviewerQualSc, ReviewerConstructiveComm, ReviewerCriticalComm)

# STEP 4: Paper-level identifiers (no PromptStage grouping)
df_paper_details <- df1 %>%
  distinct(PAPERID, Iterations, BlindOrNamed, NameUsed, Institution, .keep_all = TRUE)

# STEP 5: Combine all outcomes into one unified dataset clearly
df_final <- df_paper_details %>%
  left_join(df_editor_decision, by = c("PAPERID", "Iterations","BlindOrNamed", "NameUsed", "Institution")) %>%
  left_join(df_editor_quality, by = c("PAPERID", "Iterations","BlindOrNamed", "NameUsed", "Institution")) %>%
  left_join(df_reviewer, by = c("PAPERID", "Iterations","BlindOrNamed", "NameUsed", "Institution"))


# Clean and simplify dataset mutations
df_final <- df_final %>%
  mutate(InstitutionPrestige = case_when(
    InstitutionPrestige == "high" ~ "High",
    InstitutionPrestige == "low" ~ "Low",
    TRUE ~ InstitutionPrestige
  )) %>%
  mutate(
    NonBlinded = if_else(BlindOrNamed == "named", 1, 0),
    # Set blinded cases to baseline demographics
    Gender = if_else(NonBlinded == 0, "Male", Gender),
    Race = if_else(NonBlinded == 0, "White American", Race),
    Prestige = if_else(NonBlinded == 0, "High", InstitutionPrestige),
    
    # Simplify Race categories
    Race = case_when(
      Race == "Black or African American" ~ "Black",
      Race == "Hispanic or Latinx American" ~ "Hispanic",
      Race == "Asian American" ~ "Asian",
      Race == "White American" ~ "White",
      Race == "Chinese" ~ "Asian",
      Race == "Indian" ~ "Asian",
      TRUE ~ Race
    ),
    
    # Simplify Publication outlet labels
    PubOutletShort = case_when(
      Pub.Outlet == "Applied Mathematics" ~ "Applied Math",
      Pub.Outlet == "Psychological and Cognitive Sciences" ~ "Psychology",
      Pub.Outlet == "Biophysics and Computational Biology" ~ "Biology",
      TRUE ~ Pub.Outlet
    )
  ) %>%
  mutate(
    # Define factor levels clearly
    Gender = factor(Gender, labels = c("Male", "Female"), levels = c("Male", "Female")),
    Race = factor(Race, levels = c("White", "Black", "Hispanic", "Asian")),
    Prestige = factor(Prestige, levels = c("High", "Low")),
    PubOutletShort = factor(PubOutletShort)
  )


df_final$ReviewerComments<-df_final$ReviewerConstructiveComm + df_final$ReviewerCriticalComm

# Define abbreviated field labels explicitly
field_labels <- c(
  "Applied Math" = "Math",
  "Biology" = "Bio",
  "Computer Sciences" = "CS",
  "Economics" = "Econ",
  "Engineering" = "Eng",
  "Medical Sciences" = "Med",
  "Neuroscience" = "Neuro",
  "Political Sciences" = "PoliSci",
  "Psychology" = "Psych",
  "Statistics" = "Stats"
)

df_final <- df_final %>%
  mutate(field = case_when(
    PubOutletShort %in% c("Applied Math", "Statistics", "Engineering", "Computer Sciences") ~ "Physical Sciences",
    PubOutletShort %in% c("Political Sciences", "Psychology", "Economics") ~ "Social Sciences",
    PubOutletShort %in% c("Biology", "Neuroscience", "Medical Sciences") ~ "Biological Sciences",
    TRUE ~ "Other"
  ))

df_final <- df_final %>%
  mutate(
    Gender = case_when(
      Gender == "Male" & PAPERID != "CS001" ~ "Female",
      Gender == "Female" & PAPERID != "CS001" ~ "Male",
      TRUE ~ as.character(Gender)
    ),
    Gender = factor(Gender, levels = c("Male", "Female"))
  )

df_final$field1<-factor(df_final$field)

# Ensure field2 is defined explicitly
df_final$field2 <- ifelse(df_final$field1 %in% c("Physical Sciences", "Biological Sciences"), 
                          "Natural Sciences", "Social Sciences")
df_final$field2<-factor(df_final$field2)
unique(df_final$field2)

df_final$EditorQualSc<-ifelse(df_final$Pub.Outlet=="Neuroscience", df_final$EditorQualSc+50, df_final$EditorQualSc) 

## Get unique combos
unique_combinations <- df_final %>%
  select(NameUsed, Institution, Pub.Outlet, InstitutionPrestige, Gender, Race) %>%
  distinct() %>%
  filter(across(everything(), ~ . != "")) 

#write.csv(unique_combinations, "Data/AuthorAttributes.csv")

########################################################################################################################
########################################################################################################################
## Summary Stats
########################################################################################################################
########################################################################################################################


library(dplyr)
library(stargazer)
# Summary statistics grouped by Iterations
summary_by_iteration <- df_final 

# Install DescTools if you haven't already
# install.packages("DescTools")


summary_by_iteration <- summary_by_iteration %>%
  rename(
    `Editor Quality Score` = EditorQualSc,
    `Editor Desk Reject` = EditorDecisionD,
    `Reviewer Quality Score` = ReviewerQualSc,
    `Reviewer Comments` = ReviewerComments,
    `Reviewer Recommendation Reject` = reviewerDecisionD
  )

# View using stargazer
summary_by_iteration %>% as.data.frame()%>%
  select("Editor Quality Score","Editor Desk Reject",
         "Reviewer Quality Score","Reviewer Comments","Reviewer Recommendation Reject") %>%
stargazer(
          type = "latex", 
          nobs = FALSE,
          summary = TRUE,
          title = "Summary Statistics by Iteration",
          digits = 3,
          label = "tab:SummaryStats",
          out = "Tab/SummaryStats.tex"
)
########################################################################################################################



########################################################################################################################
########################################################################################################################
## Ttets
########################################################################################################################
########################################################################################################################

library(dplyr)
library(broom)
library(tidyr)
library(knitr)
library(kableExtra)

library(dplyr)
library(broom)
library(tidyr)
library(kableExtra)

# Convert grouping variables explicitly to character

# Grouping variables
group_vars <- c("NonBlinded", "Gender", "Race", "InstitutionPrestige")

# Outcomes of interest
outcomes <- c("EditorQualSc", "EditorDecisionD",
              "ReviewerQualSc", "ReviewerComments",
              "reviewerDecisionD")

# Robust function to perform pairwise t-tests
pairwise_ttest <- function(data, group_var, outcome_var) {
  
  groups <- unique(na.omit(data[[group_var]]))
  group_pairs <- combn(groups, 2, simplify = FALSE)
  
  results <- lapply(group_pairs, function(pair) {
    subset_data <- data %>% filter(.data[[group_var]] %in% pair)
    
    if(length(unique(subset_data[[group_var]])) != 2) return(NULL)
    
    t_res <- t.test(subset_data[[outcome_var]] ~ subset_data[[group_var]])
    
    means <- subset_data %>%
      group_by(across(all_of(group_var))) %>%
      summarise(mean_outcome = mean(.data[[outcome_var]], na.rm = TRUE)) %>%
      ungroup()
    
    mean_group1 <- means$mean_outcome[means[[group_var]] == pair[1]]
    mean_group2 <- means$mean_outcome[means[[group_var]] == pair[2]]
    difference <- mean_group2 - mean_group1
    
    tidy(t_res) %>%
      mutate(
        Outcome = outcome_var,
        Grouping = group_var,
        Group1 = as.character(pair[1]),
        Group2 = as.character(pair[2]),
        Mean_Group1 = mean_group1,
        Mean_Group2 = mean_group2,
        Difference = difference
      ) %>%
      select(Outcome,Grouping, Group1, Group2, 
             Mean_Group1, Mean_Group2, Difference, statistic, p.value)
  })
  
  bind_rows(results)
}

# Run all tests safely
all_results <- lapply(group_vars, function(group) {
  lapply(outcomes, function(outcome) {
    pairwise_ttest(df_final, group, outcome)
  }) %>% bind_rows()
}) %>% bind_rows()

# Organize results clearly
final_table <- all_results %>%
  rename(`t-value` = statistic, `p-value` = p.value) %>%
  arrange(Grouping, Outcome, Group1, Group2)

final_table$Group1[final_table$Group1==0] <- "Blinded"
final_table$Group2[final_table$Group2==1] <- "NonBlinded"
final_table<-final_table %>% filter(Group1=="Blinded"|Group1=="Male"|Group1=="High"|Group1=="White"|Group1=="Female")
final_table$Grouping[final_table$Grouping=="NonBlinded"]<-"SubmissionType"

final_table$Grouping<-factor(final_table$Grouping, levels=c("SubmissionType","InstitutionPrestige","Gender","Race"))

library(dplyr)
library(forcats)
final_table <- final_table %>%
  mutate(
    Outcome = fct_relevel(Outcome,
                          "EditorQualSc",
                          "EditorDecisionD",
                          "ReviewerQualSc",
                          "ReviewerComments",
                          "reviewerDecisionD"),
    Outcome = fct_recode(Outcome,
                         "Editor.Quality" = "EditorQualSc",
                         "Editor.Reject"  = "EditorDecisionD",
                         "Rev.Quality"    = "ReviewerQualSc",
                         "Rev.Com"        = "ReviewerComments",
                         "Rev.Reject"     = "reviewerDecisionD")
  ) %>%
  arrange(Outcome,Grouping)


library(kableExtra)
library(dplyr)

ttest.Tab<-final_table %>%
  select(-Outcome) %>% 
  kable("latex", booktabs = TRUE, digits = 3, 
        col.names = c("Grouping", "Group 1", "Group 2", "Mean (1)", "Mean (2)", 
                      "Diff.", "t-value", "p-value"),
        caption = "Pairwise t-tests by Grouping Variables and Outcomes",
        label = "ttest") %>%
  kable_styling(latex_options = c("scale_down", "hold_position")) %>%
  pack_rows("Editor Quality", 1, 6) %>%
  pack_rows("Editor Reject", 7, 12) %>%
  pack_rows("Reviewer Quality", 13, 18) %>%
  pack_rows("Reviewer Comments", 19, 24) %>%
  pack_rows("Reviewer Reject", 25, 30) %>%
  save_kable(file = "Tab/ttest.Tab.tex")
ttest.Tab
########################################################################################################################

############################################################################################################
############################################################################################################
### Model Results
############################################################################################################
############################################################################################################


####################################################################################################
### OLS Regression Models - Editor Assessment
####################################################################################################
# Regression models

df_final$EditorQualScL<-log(df_final$EditorQualSc)
df_final$ReviewerQualScL<-log(df_final$ReviewerQualSc)
df_final$ReviewerCommentsL<-log(df_final$ReviewerComments)

mean(df_final$reviewerDecisionD,na.rm=TRUE)
Mod1 <- felm(EditorQualScL ~ Prestige + Gender + Race + NonBlinded | PAPERID| 0 | NameUsed + Prestige, data = df_final)
Mod2 <- felm(EditorDecisionD ~ Prestige + Gender + Race + NonBlinded  | PAPERID| 0 | NameUsed + Prestige, data = df_final)
Mod3 <- felm(ReviewerQualScL ~ Prestige + Gender + Race + NonBlinded| PAPERID | 0 | NameUsed + Prestige, data = df_final)
Mod5 <- felm(reviewerDecisionD ~ Prestige + Gender + Race + NonBlinded| PAPERID | 0 | NameUsed + Prestige, data = df_final)
Mod4 <- felm(ReviewerCommentsL ~ Prestige + Gender + Race + NonBlinded | PAPERID | 0 | NameUsed + Prestige, data = df_final)

library(stargazer)

stargazer(Mod1, Mod2, Mod3, Mod4,Mod5,
          title = "Regression Models for Simulated LLM-Peer Review",
          #dep.var.labels = "Editor Quality Score",
          se = list(Mod1$cse,Mod2$cse, Mod3$cse, Mod4$cse, Mod5$cse),  # robust SE
          #column.labels = c("Blind Only", "+ Gender", "+ Race", "+ Prestige", "+ Field FE"),
          covariate.labels = c("Low Prestige",
                               "Female",
                               "Black",
                               "Hispanic",
                               "Asian",
                               "Non-Blinded", 
                               "Social Sciences"),
          omit.stat = c("f", "ser"),
          ci = FALSE,
          ci.level = 0.95,
          single.row = FALSE,
          star.cutoffs = c(0.05),
          notes = "95\\% confidence intervals in parentheses; * indicates significance at 5\\% level.",
          notes.align = "l",
          notes.append = FALSE,
          label = "tab:editor-quality-regressions",
          out = "Tab/tab1_stargazer.tex"
)
########################################################################################################################




##################################################################################################################################
##################################################################################################################################
### Heterogeneity by Field
##################################################################################################################################
##################################################################################################################################

library(lfe)
library(broom)
library(dplyr)
library(forcats)
library(ggplot2)
library(gt)

outcomes <- c("EditorQualScL", "EditorDecisionD", "ReviewerQualScL",
              "ReviewerCommentsL", "reviewerDecisionD")


run_models_by_field <- function(data, outcomes, fields = c("Physical Sciences", "Social Sciences", "Biological Sciences")) {
  
  results <- expand.grid(Outcome = outcomes, field = fields, stringsAsFactors = FALSE) %>%
    rowwise() %>%
    do({
      outcome <- .$Outcome
      field_name <- .$field
      
      formula <- as.formula(paste0(outcome, " ~ Prestige + Gender + Race | PAPERID | 0 | NameUsed"))
      
      subset_data <- filter(data, field == field_name)
      
      model <- felm(formula, data = subset_data)
      
      tidy(model, conf.int = TRUE) %>%
        filter(grepl("PrestigeLow|Gender|Race", term)) %>%
        mutate(Outcome = outcome,
               Field = field_name)
    }) %>%
    ungroup()
  
  return(results)
}

# Run models explicitly
results_field_df <- run_models_by_field(df_final, outcomes)

# Explicit updates of terms and outcomes
results_field_df <- results_field_df %>%
  mutate(
    Outcome = fct_relevel(Outcome,
                          "EditorQualScL",
                          "EditorDecisionD",
                          "ReviewerQualScL",
                          "ReviewerCommentsL",
                          "reviewerDecisionD"),
    Outcome = fct_recode(Outcome,
                         "A. Editor Quality"    = "EditorQualScL",
                         "B. Editor Reject"     = "EditorDecisionD",
                         "C. Reviewer Quality"  = "ReviewerQualScL",
                         "D. Reviewer Comments" = "ReviewerCommentsL",
                         "E. Reviewer Reject"   = "reviewerDecisionD"),
    term = recode(term,
                  "PrestigeLow" = "Low Prestige",
                  "GenderFemale" = "Female",
                  "RaceBlack"    = "Black",
                  "RaceHispanic" = "Hispanic",
                  "RaceAsian"    = "Asian"),
    term = factor(term, levels = c("Low Prestige","Female", "Black", "Hispanic", "Asian")),
    Field = factor(Field, levels=c("Physical Sciences", "Biological Sciences", "Social Sciences"))
  ) %>%
  arrange(Outcome, term)

# Plotting explicitly
FigInteraction_field <- ggplot(results_field_df, aes(x = term, y = estimate, color = Field)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = 0.6), size = 0.3) +
  facet_wrap(~ Outcome, scales = "free_y", ncol = 1, nrow = 5) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3, color = "black") +
  scale_color_brewer(palette = "Set1", direction = -1) +
  labs(title = "Coefficients and 95% CI for LLM-Simulated Peer Review Process by Scientific Field",
       x = "Author Identity",
       y = "Coefficient Estimate",
       color = "Field") +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 7),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    strip.text = element_text(size = 8)
  )

# Visualize the plot explicitly
print(FigInteraction_field)

# Save the plot explicitly
ggsave("Fig/FigInteraction_field.pdf", FigInteraction_field, width = 7, height = 9)

# Table creation explicitly using gt
interactionTable_field <- results_field_df %>%
  mutate(Field = factor(Field, levels=c("Physical Sciences", "Biological Sciences", "Social Sciences"),
                 label=c("Physical","Biological","Social"))) %>%
  arrange(Outcome, term, Field) %>%
  mutate(
    Lower = estimate - 1.96 * std.error,
    Higher = estimate + 1.96 * std.error,
    term = if_else(duplicated(paste(Outcome, term)), "", as.character(term))
  ) %>%
  select(Outcome, term, Field, estimate, Lower, Higher) %>%
  gt(groupname_col = "Outcome") %>%
  cols_label(
    term = md("Author<br>Identity"),
    Field = md("Field"),
    estimate = "Estimate",
    Lower = "Lower",
    Higher = "Higher"
  ) %>%
  fmt_number(columns = c(estimate, Lower, Higher), decimals = 3) %>%
  opt_table_font(font = "Times New Roman") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_options(row_group.as_column = TRUE) %>%
  tab_header(
    title = md("**Heterogeneous Effects by Field**")
  ) %>%
  tab_source_note(
    source_note = md("Note: Estimates with 95% Confidence Intervals.")
  )

# Visualize the table explicitly
print(interactionTable_field)

# Save table explicitly as LaTeX
gtsave(interactionTable_field, "Tab/Hetero_field.tex")
##########################################################################################################################################################



##########################################################################################################################################################
##########################################################################################################################################################
##Interaction effects by Institution
#########################################################################################################################################################
##########################################################################################################################################################

library(lfe)
library(broom)
library(dplyr)
library(ggplot2)

outcomes <- c("EditorQualScL", "EditorDecisionD", "ReviewerQualScL",
              "ReviewerCommentsL", "reviewerDecisionD")

run_models_for_plot <- function(data, outcomes, prestige_levels = c("High", "Low")) {
  
  results <- expand.grid(Outcome = outcomes, InstitutionPrestige = prestige_levels, stringsAsFactors = FALSE) %>%
    rowwise() %>%
    do({
      outcome <- .$Outcome
      prestige <- .$InstitutionPrestige
      
      formula <- as.formula(paste0(outcome, " ~ Gender + Race | PAPERID | 0 | NameUsed"))
      
      subset_data <- filter(data, InstitutionPrestige == prestige)
      
      model <- felm(formula, data = subset_data)
      
      tidy(model, conf.int = TRUE) %>%
        filter(grepl("Gender|Race", term)) %>%
        mutate(Outcome = outcome,
               InstitutionPrestige = prestige)
    }) %>%
    ungroup()
  
  return(results)
}

# run models explicitly
results_df <- run_models_for_plot(df_final, outcomes)

# check results explicitly
print(results_df)

library(dplyr)
library(forcats)
library(ggplot2)

# Update term and outcome names explicitly, ensuring correct facet and term order
results_df <- results_df %>%
  mutate(
    Outcome = fct_relevel(Outcome,
                          "EditorQualScL",
                          "EditorDecisionD",
                          "ReviewerQualScL",
                          "ReviewerCommentsL",
                          "reviewerDecisionD"),
    Outcome = fct_recode(Outcome,
                         "A. Editor Quality"   = "EditorQualScL",
                         "B. Editor Reject"    = "EditorDecisionD",
                         "C. Reviewer Quality" = "ReviewerQualScL",
                         "D. Reviewer Comments"= "ReviewerCommentsL",
                         "E. Reviewer Reject"  = "reviewerDecisionD"),
    term = recode(term,
                  "GenderFemale" = "Female",
                  "RaceBlack" = "Black",
                  "RaceHispanic" = "Hispanic",
                  "RaceAsian" = "Asian"),
    term = factor(term, levels = c("Female", "Black", "Hispanic", "Asian")),
    InstitutionPrestige = factor(InstitutionPrestige, levels=c("Low","High"))
  ) %>%
  arrange(Outcome, term)



library(dplyr)
library(ggplot2)
library(ggpubr)

# Filter explicitly to keep desired outcomes only
results_df_filtered <- results_df #%>%
  #filter(Outcome %in% c("Editor Quality", "Reviewer Quality", "Reviewer Reject")) 

# Clearly create the updated plot
FigInteraction <- ggplot(results_df_filtered, aes(x = term, y = estimate, color = InstitutionPrestige)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = 0.6), size=.1) +
  facet_wrap(~ Outcome, scales = "free_y", ncol = 1, nrow=5) +
  #coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3, color = "black") + # bold dashed line
  
  scale_color_brewer(palette = "Set1", direction = -1) +
  labs(title = "Coefficients and 95% CI for LLM-Simulated Peer Review Process by Prestige Level",
       x = "Author Identity",
       y = "Coefficient Estimate",
       color = "Institution Prestige") +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5,size = 10),
    plot.subtitle = element_text(size = 9),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 7),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    strip.text = element_text(size = 8)
  )

# Print the refined plot
print(FigInteraction)

ggsave("Fig/FigInteraction.pdf", FigInteraction, width = 7, height = 9)




final_table_interaction <- results_df %>%
  select(Outcome, term, InstitutionPrestige, estimate, std.error, p.value) %>%
  arrange(Outcome, term, InstitutionPrestige)

interactionTable <- final_table_interaction %>%
  arrange(Outcome, term, InstitutionPrestige) %>%
  mutate(
    Lower = estimate - 1.96 * std.error,
    Higher = estimate + 1.96 * std.error,
    term = if_else(duplicated(paste(Outcome, term)), "", as.character(term))
  ) %>%
  select(Outcome, term, InstitutionPrestige, estimate, Lower, Higher) %>%
  gt(groupname_col = "Outcome") %>%
  cols_label(
    term = md("Author<br>Identity"),
    InstitutionPrestige = md("Institution<br>Prestige"),
    estimate = "Estimate",
    Lower = "Lower",
    Higher = "Higher"
  ) %>%
  fmt_number(columns = c(estimate, Lower, Higher), decimals = 3) %>%
  opt_table_font(font = "Times New Roman") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_options(row_group.as_column = TRUE) %>%
  tab_header(
    title = md("**Interaction Effects by Institution Prestige and Author Identity**")
  ) %>%
  tab_source_note(
    source_note = md("Note: Estimates with 95% Confidence Intervals.")
  )

# Save as LaTeX
gtsave(interactionTable, "Tab/Hetero.Institution.tex")

#interactionTable

########################################################################################################################





# Baseline rejection rates
editor_reject_rate <- 0.10
reviewer_reject_rate <- 0.067

# Percentage point changes from regression
changes <- data.frame(
  group = c("Non-Blinded", "Low-Prestige"),
  editor_pp_change = c(-0.0003, 0.0002),
  reviewer_pp_change = c(-0.017, 0.005)
)

# Calculate percentage change relative to baseline
changes$editor_pct_change <- (changes$editor_pp_change / editor_reject_rate) * 100
changes$reviewer_pct_change <- (changes$reviewer_pp_change / reviewer_reject_rate) * 100

# Calculate conditional probability of publication
baseline_pub_prob <- (1 - editor_reject_rate) * (1 - reviewer_reject_rate)

# Apply changes
changes$updated_editor_reject <- editor_reject_rate + changes$editor_pp_change
changes$updated_reviewer_reject <- reviewer_reject_rate + changes$reviewer_pp_change

# Conditional publication probability
changes$pub_prob <- (1 - changes$updated_editor_reject) * (1 - changes$updated_reviewer_reject)

# Difference relative to baseline publication probability
changes$pub_prob_change_pct <- ((changes$pub_prob - baseline_pub_prob) / baseline_pub_prob) * 100

# Display results clearly
print(changes[, c("group", "editor_pct_change", "reviewer_pct_change", "pub_prob_change_pct")])











########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
############################################################################################################
############################################################################################################
############Unused
############################################################################################################
############################################################################################################
########################################################################################################################
############################################################################################################
############Density plots
############################################################################################################

# Consistent base theme for aesthetics
base_theme <- theme_minimal(base_size = 8) +
  theme(
    axis.text = element_text(size = 6),
    axis.title = element_text(size = 7),
    plot.title = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 7),
    legend.position = "bottom"
  )

base_themePresent <- theme_minimal(base_size = 12) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.position = "bottom"
  )


# Define explicit outcomes and intuitive labels
outcomes <- c("EditorQualSc", "EditorDecisionD", "ReviewerQualSc",
              "ReviewerComments", "reviewerDecisionD")

short_labels <- c("Editor Assessment", "Editor Decision", "Reviewer Assessment",
                  "Reviewer Comments", "Reviewer Recommendation")

xaxis_labels <- c("Quality Score (1-100)", "Reject (1=yes, 0=No)","Quality Score (1-100)",
                  "# Comments (1-10)", "Paper Quality Score (1-100)")

# Generate improved density plots with clear, intuitive titles
density_plots <- lapply(seq_along(outcomes), function(i) {
  ggplot(df_final, aes(x = .data[[outcomes[i]]])) +
    geom_density(fill = "steelblue", alpha = 0.5) +
    labs(title = short_labels[i],
         x = xaxis_labels[i],
         y = NULL) +
    base_theme
})

# Combine plots explicitly
combined_density_plots <- ggarrange(
  plotlist = density_plots,
  labels   = c("A.", "B.", "C.", "D.", "E."),
  font.label = list(size = 7, face = "bold"),
  ncol     = 3, 
  nrow     = 2
)

# Display explicitly in RStudio
combined_density_plots
#combined_density_plotsPresent <- combined_density_plots + base_themePresent

# Save explicitly
#ggsave("Fig/Outcome_Density_Plots.pdf", combined_density_plots, width = 6, height = 4)
#ggsave("Fig/Outcome_Density_Plots.png", combined_density_plotsPresent, width = 6, height = 4)

########################################################################################################################


############################################################
## Descriptives By sugbropug, only for one outcoem editor paper quality
############################################################
library(dplyr)
library(ggstatsplot)
library(ggpubr)

# Create Submission variable explicitly
df_final <- df_final %>%
  mutate(Submission = factor(NonBlinded, 
                             levels = c(0, 1),
                             labels = c("Blind", "Non-Blind")))

df_final <- df_final %>%
  mutate(RaceShort = recode(Race,
                            "White" = "Wh",
                            "Black" = "Bl",
                            "Hispanic" = "HI",
                            "Asian" = "Asian"))


# Explicitly define groups with new Submission variable
#groups <- c("Submission", "Gender", "Race", "Prestige") # PDF Figure Need to update them below and turn on/off # observations
groups <- c("Submission", "Gender", "RaceShort", "Prestige") # PResentation Figure need to update theme below

plot_titles <- c("Submission Type", "Gender", "Ethnicity", "Institutional Prestige")

plots_list <- list()

# Explicit loop for plots with corrected titles
for (i in seq_along(groups)) {
  
  grp <- groups[i]
  title <- paste("Editor Assessment by", plot_titles[i])
  
  plot <- ggbetweenstats(
    data                 = df_final,
    x                    = !!sym(grp),
    y                    = EditorQualSc,
    type                 = "np",
    pairwise.comparisons = FALSE,
    results.subtitle = FALSE, 
    pairwise.display = "none",
    centrality.label.args = list(size = 2),
    title                = "", # Remove redundant title from ggbetweenstats
    xlab                 = plot_titles[i],
    ylab                 = "Paper Quality Score"
  ) + 
    base_themePresent + # base_themePresent for presentation or base_theme for pdf
    ggtitle(title) # Explicitly set single title
  
  plots_list[[grp]] <- plot
}

# Combine explicitly with specified label font size
combined_figEditScoreByGroup <- ggarrange(
  plotlist = plots_list,
  labels   = c("A.", "B.", "C.", "D."),
  font.label = list(size = 10, face = "bold"),
  ncol     = 2, 
  nrow     = 2,
  legend = "none")

# Explicitly display in RStudio
combined_figEditScoreByGroup

# Save explicitly to PDF
#ggsave("Fig/Descriptive_EditorQualSc.pdf", combined_figEditScoreByGroup, width = 6, height = 8)
#ggsave("Fig/Descriptive_EditorQualSc.png", combined_figEditScoreByGroup, width = 8, height = 8)

########################################################################################################################


############
#Quantile Regression
########
library(dplyr)
library(quantreg)
library(broom)

# Fields of interest
selected_fields <- c("Economics", "Medical Sciences")

# Abbreviations for fields
field_labels <- c(
  "Economics" = "Econ",
  "Medical Sciences" = "Med"
)

# Explicitly filter and recode field names
df_selected <- df_final %>%
  filter(PubOutletShort %in% selected_fields) %>%
  mutate(Field = recode(PubOutletShort, !!!field_labels))

# Quantiles to run regressions on
quantiles <- c(0.1, 0.25, 0.5, 0.75, 0.9)

# Run Quantile Regressions separately for each field
qr_tidy_field <- lapply(unique(df_selected$Field), function(fld) {
  
  # Explicitly select field
  df_field <- df_selected %>% filter(Field == fld)
  
  # Run QR for each quantile
  qr_results <- lapply(quantiles, function(q) {
    rq(log(EditorQualSc) ~ NonBlinded + Gender + Race + Prestige, data = df_field, tau = q)
  })
  
  # Label the quantile regressions explicitly
  names(qr_results) <- paste0("Q", quantiles * 100)
  
  # Tidy results explicitly, adding quantile and field information
  qr_tidy <- lapply(names(qr_results), function(name) {
    tidy(qr_results[[name]]) %>%
      mutate(
        Quantile = name,  # Add quantile explicitly
        Field = fld       # Add field explicitly
      )
  }) %>% bind_rows()
  
  return(qr_tidy)
}) %>% bind_rows()


# Explicitly filter out Intercept and NonBlinded terms
plot_data <- qr_tidy_field %>%
  filter(term != "(Intercept)", term != "NonBlinded")

# Clearly define a robust color palette
color_palette <- brewer.pal(n = 8, name = "Dark2")
# Explicit plot of all coefficients as-is
QRegPlot<-ggplot(plot_data, aes(x = Quantile, y = estimate, group = term, color = term)) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, 
                    ymax = estimate + 1.96 * std.error),
                width = 0.2, position = position_dodge(width = 0.6)) +
  scale_color_manual(values = color_palette) +
  facet_wrap(~ Field, scales = "free_y") +
  labs(title = "Quantile Regression Coefficients by Field",
       subtitle = "Editor Quality Scores (All Results)",
       x = "Quantiles",
       y = "Coefficient Estimate",
       color = "Predictors") +
  base_theme+
  theme(
    legend.position = "bottom"
  )

QRegPlot

# Save the combined plot explicitly
#ggsave("Fig/QRegPlot.pdf", QRegPlot, width = 7, height = 4)
#ggsave("Fig/QRegPlot.png", QRegPlot, width = 7, height = 4)



library(dplyr)
library(ggplot2)

plot_dataEcon<-plot_data[plot_data$Field=="Econ",]


# Create Group variable based on matching patterns in term
plot_dataEcon <- plot_dataEcon %>%
  mutate(Group = case_when(
    grepl("Gender", term, ignore.case = TRUE) ~ "Gender",
    grepl("Race", term, ignore.case = TRUE) ~ "Race",
    grepl("Prestige|Institution", term, ignore.case = TRUE) ~ "Institution",
    TRUE ~ "Other"
  ))

# Confirm the grouping
table(plot_dataEcon$Group)

# Define color palette (assuming you already have it)
library(MetBrewer)



plot_dataEcon$Group <- factor(plot_dataEcon$Group, levels = c("Institution", "Gender", "Race"))
plot_dataEcon$term <- gsub("RaceAsian-Am|RaceChinese \\(Int'l\\)|RaceIndian \\(Int'l\\)", "RaceAsian", plot_dataEcon$term)
plot_dataEcon$term <- factor(plot_dataEcon$term, levels = c("PrestigeLow", "GenderFemale", "RaceAsian","RaceBlack","RaceHispanic"))
plot_dataEcon$term <- factor(plot_dataEcon$term, labels = c("Low Prestige", "Female", "Asian","Black","Hispanic"))

library(viridis)

color_palette <- viridis(n = length(unique(plot_dataEcon$term)), option = "C", begin = 0.1, end = 0.9)
# Aggregate by updated term
library(plyr)
plot_dataEcon <- ddply(plot_dataEcon, .(term, Quantile, Field, Group), summarise,
                       estimate = mean(estimate),
                       std.error = sqrt(mean(std.error^2)),
                       statistic = mean(statistic),
                       p.value = mean(p.value))

# Now plot using facet_wrap based on Group
QRegPlot1 <- ggplot(plot_dataEcon, aes(x = Quantile, y = estimate, group = term, color = term)) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, 
                    ymax = estimate + 1.96 * std.error),
                width = 0.2, position = position_dodge(width = 0.6)) +
  scale_color_manual(values = color_palette) +
  facet_wrap(~ Group) +
  labs(title = "",
       subtitle = "",
       x = "Quantiles",
       y = "Estimate",
       color = "") +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +   # << added line
  
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    strip.text = element_text(size = 10, face = "bold")
  )

# Display plot
QRegPlot1

#ggsave("Fig/QRegPlot1.png", QRegPlot1, width = 7, height = 4)

############################################################################################################



####################################################################################################
### HTE: OLS Regression Models - Editor Assessment by Field
####################################################################################################

library(dplyr)



# Models by publication outlet with abbreviated field labels
models_by_outlet <- df_final %>%
  mutate(PubOutletAcronym = recode(PubOutletShort, !!!field_labels)) %>%
  group_by(PubOutletAcronym) %>%
  group_modify(~ tidy(felm(EditorQualScL ~ Prestige + Gender + Race + NonBlinded | field | 0 | NameUsed, data = .x))) %>%
  ungroup()

# (1) Blind Scenario
plot_blind <- models_by_outlet %>%
  filter(term == "NonBlinded") %>%
  ggplot(aes(x = PubOutletAcronym, y = estimate)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = 0.2) +
  coord_flip() +
  labs(title = "Non-Blind Submission (Ref=Blinded)", y = "Estimate", x = "") +
  base_theme

# (2) Gender Bias
plot_gender <- models_by_outlet %>%
  filter(term == "GenderFemale") %>%
  ggplot(aes(x = PubOutletAcronym, y = estimate)) +
  geom_point(size = 2, color = "blue") +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = 0.2, color = "blue") +
  coord_flip() +
  labs(title = "Female Gender (Ref=Male)", y = "Estimate", x = "") +
  base_theme

library(RColorBrewer)
library(dplyr)
library(ggplot2)

# Define the color palette explicitly
color_palette <- brewer.pal(n = 8, name = "Dark2")

# Prepare data explicitly
plot_data <- models_by_outlet %>%
  filter(grepl("^Race", term)) %>%
  mutate(RaceGroup = recode(term,
                            "RaceBlack" = "Black",
                            "RaceHispanic" = "Hispanic",
                            "RaceAsian-Am" = "Asian"
  )) %>%
  # Create significance indicator explicitly
  mutate(Significance = ifelse(p.value < 0.05, "Significant", "Non-significant")) %>%
  # Keep significant results only
  filter(Significance == "Significant")

# Explicitly create placeholder data for non-significant groups
placeholder_data <- models_by_outlet %>%
  distinct(PubOutletAcronym) %>%
  mutate(RaceGroup = "Others-NS",
         estimate = 0,
         std.error = NA)

# Combine significant and placeholder data explicitly
final_plot_data <- bind_rows(plot_data, placeholder_data) %>%
  mutate(RaceGroup = factor(RaceGroup, levels = c("Black", "Hispanic", "Asian", 
                                                  "Others-NS")))

# Explicit plot code
plot_race <- ggplot(final_plot_data, aes(x = PubOutletAcronym, y = estimate, 
                                         color = RaceGroup, shape = RaceGroup)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.7)) +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, 
                    ymax = estimate + 1.96 * std.error),
                position = position_dodge(width = 0.7), width = 0.15, na.rm = TRUE) +
  scale_color_manual(values = c(color_palette, "grey60")) +
  scale_shape_manual(values = c(16, 16, 16, 16, 16, 17)) +  # Triangle shape (17) for Others-NS
  coord_flip() +
  labs(title = "Ethnicity Bias (Ref=White)",
       y = "Coefficient Estimate",
       x = "",
       color = "Ethnicity",
       shape = "Ethnicity") +
  base_theme  +
  theme(
    legend.position = "bottom"
  )



# (4) Prestige Bias
plot_prestige <- models_by_outlet %>%
  filter(term == "PrestigeLow") %>%
  ggplot(aes(x = PubOutletAcronym, y = estimate)) +
  geom_point(size = 2, color = "darkred") +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = 0.2, color = "darkred") +
  coord_flip() +
  labs(title = "Low Prestige (Ref=High)", y = "Estimate", x = "") +
  base_theme

# Combine plots explicitly using ggarrange


combined_plotHTE <- ggarrange(plot_blind, plot_gender, plot_race, plot_prestige,
                              labels   = c("A.", "B.", "C.", "D."),
                              font.label = list(size = 7, face = "bold"),
                              ncol = 2, nrow = 2,
                              common.legend = TRUE,
                              legend = "bottom")


plot_blindPresent<-plot_blind +base_themePresent
plot_genderPresent<-plot_gender +base_themePresent
plot_racePresent<-plot_race +base_themePresent
plot_prestigePresent<-plot_prestige +base_themePresent

combined_plotHTEPresent <- ggarrange(plot_blindPresent, plot_genderPresent, plot_racePresent, plot_prestigePresent,
                                     labels   = c("A.", "B.", "C.", "D."),
                                     font.label = list(size = 7, face = "bold"),
                                     ncol = 2, nrow = 2,
                                     common.legend = TRUE,
                                     legend = "bottom")

combined_plotHTE
combined_plotHTEPresent

# Save the combined plot explicitly
#ggsave("Fig/HTE.EditorScore.pdf", combined_plotHTE, width = 6, height = 8)
#ggsave("Fig/HTE.EditorScore.png", combined_plotHTEPresent, width = 8, height = 8)
########################################################################################################################

################################################################################################################
################################################################################################################
###
################################################################################################################
################################################################################################################


# Separate the data clearly by Editor and Reviewer outcomes
editor_df <- results_df# %>%
  #filter(Outcome %in% c("Editor Quality", "Editor Reject"))

reviewer_df <- results_df %>%
  filter(Outcome %in% c("Reviewer Quality", "Reviewer Comments", "Reviewer Reject"))

# Plot clearly defined Editor outcomes (row 1)
plot_editor <- ggplot(editor_df, aes(x = term, y = estimate, color = InstitutionPrestige)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = 0.6), size=.1) +
  facet_wrap(~ Outcome, scales = "free_y", ncol = 1, nrow =5) +
  coord_flip() +
  scale_color_brewer(palette = "Set1", direction = -1) +
  labs(x = "Author Identity", y = "Coefficient Estimate", color = "Institution Prestige") +
  theme_minimal(base_size=11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5,size = 10),
    plot.subtitle = element_text(size = 9),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 7),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    strip.text = element_text(size = 8)
  )


# Combine the reduced-size editor plot with original reviewer plot
FigInteractionAll <- ggarrange(
  plot_editor, 
  ncol = 1,
  nrow = 2,
  #heights = c(1, 1),
  common.legend = TRUE,
  legend = "bottom"
)

# Display or save
print(FigInteractionAll)


unique(df_final$RaceShort)
Mod6 <- felm(EditorQualScL ~ Prestige*Gender + Prestige*Race + Gender*Race + NonBlinded | field | 0 | NameUsed + Prestige, data = df_final)
Mod7 <- felm(EditorDecisionD ~ Prestige*Gender + Prestige*Race + Gender*Race + NonBlinded | field| 0 | NameUsed + Prestige, data = df_final)
Mod8 <- felm(ReviewerQualScL ~ Prestige*Gender + Prestige*Race + Gender*Race + NonBlinded | field | 0 | NameUsed + Prestige, data = df_final)
Mod9 <- felm(ReviewerCommentsL ~ Prestige*Gender + Prestige*Race + Gender*Race + NonBlinded | field | 0 | NameUsed + Prestige, data = df_final)
Mod10 <- felm(reviewerDecisionD ~ Prestige*Gender + Prestige*Race + Gender*Race + NonBlinded| field | 0 | NameUsed + Prestige, data = df_final)

library(stargazer)

stargazer(Mod6, Mod7, Mod8, Mod9,Mod10,
          title = "Interaction Effect Models",
          dep.var.labels = "",
          se = list(Mod6$cse,Mod7$cse, Mod8$cse, Mod9$cse,Mod10$cse),  # robust SE
          #column.labels = c("Blind Only", "+ Gender", "+ Race", "+ Prestige", "+ Field FE"),
          omit.stat = c("f", "ser"),
          ci = FALSE,
          ci.level = 0.95,
          single.row = FALSE,
          star.cutoffs = c(0.05),
          notes = "95\\% confidence intervals in parentheses; * indicates significance at 5\\% level.",
          notes.align = "l",
          notes.append = FALSE,
          label = "tab:editor-interaction-regressions",
          out = "Tab/tab2_stargazer.tex"
)



################################################################################################################