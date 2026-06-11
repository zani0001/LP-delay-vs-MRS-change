# ══════════════════════════════════════════════════════════════
# LP DELAY & MRS CHANGE ANALYSIS
# ══════════════════════════════════════════════════════════════
# Your data is already loaded as "df" — start from Step 1 below
# Do NOT re-import the data, it is already in R
# Run each line one at a time using Ctrl+Enter (Windows)
# or Cmd+Enter (Mac)
# ══════════════════════════════════════════════════════════════


# ── STEP 1: Load packages ─────────────────────────────────────
# Run each of these lines first before anything else
# If you get an error saying package not found, run:
# install.packages("packagename") — replacing packagename with
# whatever package is missing, then run the library line again

library(dplyr)
library(ggplot2)
library(gtsummary)
library(rstatix)
library(ggplot2)
library(readxl)
library(readr)
LP_IMPORTDATAXL <- read_csv("~/Desktop/LP_IMPORTDATAXL.csv")
View(LP_IMPORTDATAXL)
df <- LP_IMPORTDATAXL
# ── STEP 2: Check your data loaded correctly ──────────────────
# This should show you the first 6 rows of your data
# You should see columns: PatientID, AdmissionToLP_hrs,
# DecisionToLP_hrs, MRSChange

head(df)

# This tells you how many rows and columns you have
# Should say 320 rows and 4 columns
dim(df)


# This shows you what type each column is
str(df)

# ── DESCRIPTIVE STATISTICS ────────────────────────────────────
summary(df)
View(summary(df))


# ── STEP 3: Create outcome groups ────────────────────────────
# This creates two new columns:
# MRS_Group = Improved / No Change / Worsened
# Worsened  = Worsened / Not Worsened (simpler version for comparisons)
df <- df %>%
  mutate(
    `MRS Change_group`= case_when(
      `MRS Change` < 0 ~ "Improved",
      `MRS Change` == 0 ~ "No Change",
      `MRS Change` > 0 ~ "Worsened"
    ),
    Worsened = ifelse(`MRS Change` > 0, "Worsened", "Not Worsened")
  )

# Check how many patients are in each group — results appear in console

table(df$`MRS Change_group`)
View(table(df$`MRS Change_group`))

table(df$Worsened)
View(table(df$Worsened))

# ── STEP 4: Check if data is normally distributed ─────────────
# This tells you which statistical tests to use
# If p < 0.05 the data is NOT normal (skewed)
# Time data like yours almost always comes back as NOT normal

shapiro_test(df$`Decision to LP (hours)`)
shapiro_test(df$`Admission to LP (hours)`)
shapiro_test(df$`MRS Change`)

View(shapiro_test(df$`Decision to LP (hours)`))
View(shapiro_test(df$`Admission to LP (hours)`))
View(shapiro_test(df$`MRS Change`))

# ── STEP 5: Summary statistics table (Table 1) ────────────────
# This creates a clean summary table showing
# median and IQR for each variable across all 320 patients

df %>%
  select(`Admission to LP (hours)`,
         `Decision to LP (hours)`,
         `MRS Change`) %>%
  tbl_summary(
    label = list(
      `Admission to LP (hours)` ~ "Admission to LP (hours)",
      `Decision to LP (hours)` ~ "Decision to LP (hours)",
      `MRS Change` ~ "MRS Change"
    ),
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})"
    ),
    digits = all_continuous() ~ 2
  ) %>%
  add_n()

# ── STEP 6: Summary table split by outcome group (Table 2) ────
# This compares the Worsened vs Not Worsened groups side by side
# The p-value column tells you if the difference is significant
# p < 0.05 means there IS a statistically significant difference

View(
  df %>%
       select(`Admission to LP (hours)`, `Decision to LP (hours)`, `MRS Change`, Worsened) %>%
       tbl_summary(
         by = Worsened,
         label = list(
           `Admission to LP (hours)` ~ "Admission to LP (hours)",
           `Decision to LP (hours)`  ~ "Decision to LP (hours)",
           `MRS Change`         ~ "MRS Change"
         ),
         statistic = list(all_continuous() ~ "{median} ({p25}, {p75})"),
         digits = all_continuous() ~ 2
       ) %>%
       add_p(test = list(all_continuous() ~ "wilcox.test")) %>%
       add_n()
  )

# ── STEP 7: Spearman correlation ──────────────────────────────
# Tests whether longer delay is associated with worse MRS score
# Look at: rho (strength of relationship) and p-value (significance)
# rho closer to +1 = longer delay associated with worse outcome
# p < 0.05 = statistically significant

cor.test(df$`Admission to LP (hours)`, df$`MRS Change`, method = "spearman")
View(cor.test(df$`Admission to LP (hours)`, df$`MRS Change`, method = "spearman"))

cor.test(df$`Decision to LP (hours)`, df$`MRS Change`, method = "spearman")
View(cor.test(df$`Decision to LP (hours)`, df$`MRS Change`, method = "spearman"))


# ── STEP 8: Mann-Whitney test ─────────────────────────────────
# Compares delay times between Worsened vs Not Worsened groups
# p < 0.05 means the two groups have significantly different delay times

wilcox.test(df$`Admission to LP (hours)`~ df$Worsened, data = df)
View(wilcox.test(df$`Admission to LP (hours)`~ df$Worsened, data = df))

wilcox.test(df$`Decision to LP (hours)`~ df$Worsened, data = df)
View(wilcox.test(df$`Decision to LP (hours)`~ df$Worsened, data = df))


# ── STEP 9: Histogram — Admission to LP time ──────────────────
# Graph appears in bottom right panel
# Shows the spread of how long patients waited from admission to LP
                 
View(
  ggplot(df, aes(x = df$`Admission to LP (hours)`)) + 
       geom_histogram(bins = 30, fill = "grey70", color = "black") +
                        labs(title = "Time from Admission to LP",
                             x = "Hours", y = "Number of Patients") +
                        theme_minimal()
                      )
ggplot(df, aes(x = df$`Admission to LP (hours)`)) + 
  geom_histogram(bins = 30, fill = "grey70", color = "black") +
  labs(title = "Time from Admission to LP",
       x = "Hours", y = "Number of Patients") +
  theme_minimal()
# ── STEP 10: Histogram — Decision to LP time ──────────────────

ggplot(df, aes(x = df$`Decision to LP (hours)`)) + 
  geom_histogram(bins = 30, fill = "grey70", color = "black") +
  labs(title = "Time from Decision to LP",
       x = "Hours", y = "Number of Patients") +
  theme_minimal()

# ── STEP 11: Bar chart — MRS Change distribution ──────────────
# Shows how many patients improved, stayed the same, or worsened

ggplot(df, aes(x = factor(df$`MRS Change`)) +
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Distribution of MRS Change",
       x = "MRS Change Score", y = "Number of Patients") +
  theme_minimal())

# ── STEP 12: Box plot — Admission to LP by outcome ────────────
# Visually compares delay times between the two outcome groups
# A higher box in the Worsened group suggests longer delays = worse outcomes

ggplot(df, aes(x = df$Worsened, y = df$`Admission to LP (hours)`)) +
  geom_boxplot() +
  labs(title = "Admission to LP Time by Outcome",
       x = "Outcome Group", y = "Hours") +
  theme_minimal()

# ── STEP 13: Box plot — Decision to LP by outcome ─────────────

ggplot(df, aes(x = df$Worsened, y = df$`Decision to LP (hours)`)) +
  geom_boxplot() +
  labs(title = "Decision to LP Time by Outcome",
       x = "Outcome Group", y = "Hours") +
  theme_minimal()

ggplot(df, aes(x = `Decision to LP (hours)`, y = `MRS Change`)) + 
  geom_point(alpha = 0.4) + 
               geom_smooth(method = "lm", se = TRUE, color = "black") +
               labs(title = "Decision to LP vs MRS Change",
                    x= "Admission to LP (hours)", y = "MRS Change") +
               theme_minimal()
