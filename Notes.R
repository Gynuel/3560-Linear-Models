# ==============================================================
# === This is an R Script for all the notes in the Exercise ====
# ==============================================================

library(dplyr)
library(skimr)
library(ggplot2)
Galton <- read.csv("Data/Galton.tab", sep="\t")
# glimpse(Galton) --- Looks like a transpose view of head with data types
# skim(Galton) --- Provides a very detailed summary of the data

# ----------
# Disclaimer: getAnywhere(glimpse) or getAnywhere(skim) - If you want to find the source of a specific function or object.
# ----------

## Data Preparation
Galton.sons <- Galton %>%
  filter(gender=="M") %>%
  group_by(family) %>%
  slice(1) %>% # Function that select specific rows from each group
               # slice(1) selects the first row from each family group
               # If no group it slices the nth row in the table
  ungroup %>%. # Not required, rather crucial to ensure that subsequent operations are performed on the dataset
  mutate(father.cm = father*2.54, son.cm = height*2.54) %>%
  select(father.cm, son.cm)
  glimpse(Galton.sons)

## Data Visualization
ggplot(Galton.sons, aes(x = father.cm, y = son.cm)) + geom_point(color ="blue") +
  xlab("length of father (cm)") + ylab("length of son (cm)") +
  theme(axis.title = element_text(size=15),axis.text = element_text(size=15))

## Correlation Analyses
cor(Galton.sons$father.cm, Galton.sons$son.cm)

# Simple Linear Regression Interpretation
# Y_Son Height = Constant*X_Father Height

## Boxplot - To show a random sample of son height for fathers with =172.72cm height

dev.off()
Galton.sons %>%
  filter(father.cm==172.72) %>%
  ggplot(aes(x=father.cm, y=son.cm)) +
  geom_boxplot() + geom_jitter(position = position_jitter(0.2)) +
  ylab("length of son (cm)") +
  theme(axis.title=element_text(size=15), axis.text=element_text(size=15),
        axis.title.x = element_blank(), axis.text.x=element_blank())