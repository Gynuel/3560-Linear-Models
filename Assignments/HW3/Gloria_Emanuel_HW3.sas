/* QUESTION 1b */
/* Step 1: Importing the Data */
PROC IMPORT DATAFILE='/home/u64083638/sasuser.v94/algae.csv'
    OUT=algae
    DBMS=CSV
    REPLACE;
    GETNAMES=YES;
RUN;

/* Step 2: Two-Way ANOVA with Interaction */
PROC GLM DATA=algae;
    CLASS Light WaterSource;
    MODEL Biomass = Light WaterSource Light*WaterSource;
    MEANS Light WaterSource / TUKEY CLDIFF; /* Tukey-adjusted confidence intervals for pairwise comparisons */
    LSMEANS Light WaterSource Light*WaterSource / OMEGA CL; /* Calculating effect size (Omega-squared) and confidence limits */
    OUTPUT OUT=anova_results P=PREDICTED R=RESIDUAL;
RUN;

/* Step 3: Hypothesis Tests */
/* - Light Main Effect: H0: Light intensities does not significantly affect algal growth.
   - WaterSource Main Effect: H0: Water sources does not significantly affect algal growth.
   - Interaction Effect: H0: There is no interaction effect between light intensity and water source on algal growth. */

/* Step 4: Post-hoc comparisons */
PROC GLM DATA=algae;
    CLASS Light WaterSource;
    MODEL Biomass = Light WaterSource Light*WaterSource;
    MEANS Light*WaterSource / TUKEY CLDIFF; /* Post-hoc comparison of all interaction pairs */
RUN;

/* Step 5: Summary of Effect Sizes */
PROC MEANS DATA=algae N MEAN STDDEV;
    CLASS Light WaterSource;
    VAR Biomass;
RUN;


/* QUESTION 1c */
/* Assumption 1: Normality of Residuals */
proc univariate data=algae normal;
    var Biomass;
    histogram Biomass / normal;
    probplot Biomass / normal(mu=est sigma=est);
run;

/* Assumption 2: Homogeneity of Variance */
proc glm data=algae;
    class Light WaterSource;
    model Biomass = Light*WaterSource;
    means Light*WaterSource / hovtest=levene; /* Levene's Test */
run;

/* Assumption 3: Independence of Residuals */
/* Plot residuals to assess independence (residuals vs. predicted values) */
proc sgplot data=anova_results;
    scatter x=PREDICTED y=RESIDUAL;
    refline 0 / axis=y lineattrs=(color=red);
    xaxis label="Predicted Values";
    yaxis label="Residuals";
    title "Residuals vs. Predicted Values";
run;

/* Additional Plot: Q-Q Plot for Residuals */
/* Generate a Q-Q plot for further normality check */
proc sgplot data=anova_results;
    qqplot residuals / line=reference;
    title "Q-Q Plot of Residuals";
run;
