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


# ===========================================
# === 2.1. Interpretation via Simulation ====
# ===========================================

set.seed(254111)
N<-100 # number of repeated experiments
x<-c(165,170,175,180,185) # five father's heights
y<-90+0.5*x+rnorm(5,sd=5) # random sample of 5 outcomes
dev.off()
plot(x,y,cex.axis=1.5,cex.lab=1.5, ylim=c(160,195))
abline(c(90,0.5),col=2)

Data<-data.frame(experiment=1,x=x,y=y)

for(experiment in 2:N) {
  y<-90+0.5*x+rnorm(5,sd=5) # random sample of 5 outcomes
  points(x,y,col=experiment)
  Data<-rbind(Data,cbind(experiment,x,y))
}

par(mfrow=c(2,3))
for(i in 1:5) {
  y<-Data$y[Data$x==x[i]]
  hist(y,main=paste("x=",x[i]),xlab="y")
  abline(v=90+0.5*x[i],col=2,lwd=2)
  abline(v=mean(y),col=4,lty=2,lwd=2)
}

# ======================================
# === 2.2. Least Squares Estimators ====
# ======================================
m<-lm(son.cm~father.cm,data=Galton.sons)

# Note: The intercept, however, has no direct physical interpretation
# because there are no fathers of height 0cm. This issue can be resolved
# by first centering the regressor.

Galton.sons<-Galton.sons %>%
  ungroup %>%
  mutate(father.cm.centered= father.cm - mean(father.cm))

# Mean of Father Height
mean(Galton.sons$father.cm)
# Mean of Centered Father Height
mean(Galton.sons$father.cm.centered)

# Use the centered values
m2<-lm(son.cm~father.cm.centered,data=Galton.sons)
m2

ggplot(Galton.sons,
       aes(x=father.cm, y=son.cm)) +
  geom_point(color="blue") +
  xlab("length of father (cm)") + ylab("length of son (cm)") +
  theme(axis.title=element_text(size=15), axis.text = element_text(size=15)) +
  geom_abline(intercept=m$coefficients[1],slope=m$coefficients[2]) +
  geom_abline(intercept=0,slope=1,color="red")

# ======================================
# === Exer. Blood Pressure Exercise ====
# ======================================

load("Data/BloodPressure.RData")
bp<-lm(bp.reduction~dose,data=BloodPressure)
BloodPressure <- BloodPressure %>%
  mutate(dose_centered = BloodPressure$dose - mean(BloodPressure$dose))

# Mean of Dose
mean(BloodPressure$dose)

# Mean of Centered Dose
mean(BloodPressure$dose_centered)

bp2 <- lm(bp.reduction~dose_centered,data=BloodPressure)


ggplot(BloodPressure,
       aes(x=dose, y=bp.reduction)) +
  geom_point(color="blue") +
  xlab("Dosage (mg per day)") + ylab("BP Reduction (mmHg)") +
  theme(axis.title=element_text(size=15), axis.text = element_text(size=15)) +
  geom_abline(intercept=bp$coefficients[1],slope=bp$coefficients[2]) +
  geom_abline(intercept=0,slope=1,color="red")

# ======================================
# === 2.3. LSE Properties ==============
# ======================================



