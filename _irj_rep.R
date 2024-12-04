##### The Incomplete Leap: On the Transition from Union Formation to Collective Bargaining #####
### Replication File for Industrial Relations Journal ##
## 04 December 2024 ##
# Authors: Vincent Jerald Ramos, PhD & Edgar Antonio Suguitan
## packages
{
  setwd("~/PLRP new")
  library(tidyverse)
  library(descr)
  library(haven)
  library(stargazer)
  library(survival)
  library(survminer)
  library(ggthemes)
  library(ggridges)
  library(vtable)
  library(eha)
  library(readxl)
  library(forestmodel)
}

#### Reading in the datasets
DATA01 <- read_csv("_irj_eha.csv") ## main dataset
density_data <- read_csv("_irj_uniondensity.csv") ## density data for figure 1

### Descriptives ###
# Counts
DATA01 %>%
  group_by(EBU_YEAR_REG, CBA_yes) %>%
  count()

#### Figure 1. Median union density by year of registration ####
density_data %>%
  mutate(EBU_YEAR_REG = lubridate::ymd(EBU_YEAR_REG, truncated=2L)) %>%
  group_by(EBU_YEAR_REG) %>%
  summarize(median_ud = median(union_density)) %>%
  ggplot(aes(x=EBU_YEAR_REG, y=median_ud)) +
  geom_line() +
  geom_point()+
  labs(title="Median Union Density by Year of Registration (1953-2021)") +
  theme_minimal() +
  theme(plot.title.position = "plot",
        plot.caption = element_text(hjust=-.2, face="italic"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

#### Figure 2: Union and CBA Registrations ####
stacked_chart <- DATA01 %>%
  group_by(CBA_yes, EBU_YEAR_REG)%>%
  count(CBA_yes)%>%
  ggplot(aes(x=EBU_YEAR_REG, fill = CBA_yes, y = n))+
  geom_col(position = "stack")+
  geom_text(aes(label = n, y = n), position = position_stack(vjust = 0.5), family = "serif")+
  theme_minimal(base_family = "serif")+
  labs(title = "New union registrations and contract settlements ",
       subtitle = "2016-2021",
       x = " ", y = " ", fill =" ")
stacked_chart

### Average and Median Union density ###
## No CBAs
mean(DATA01$union_density[DATA01$CBA_yes == 'No CBA'], na.rm = T)
median(DATA01$union_density[DATA01$CBA_yes == 'No CBA'], na.rm = T)

## with CBAs
mean(DATA01$union_density[DATA01$CBA_yes == 'With CBA'], na.rm = T)
median(DATA01$union_density[DATA01$CBA_yes == 'With CBA'], na.rm = T)

#### Table 1. Summary Statistics ####
descriptives <- DATA01 %>%
  select(Density, Type, Sector) %>%
  mutate(Density = as_factor(Density),
         Type = as_factor(Type),
         Sector = as_factor(Sector))
summary(descriptives)

## Relative percentages
## By density
crosstab(DATA01$Density, DATA01$CBA_yes, prop.c = T)
## By union type
crosstab(DATA01$Type, DATA01$CBA_yes, prop.c = T)
## By sector
crosstab(DATA01$Sector, DATA01$CBA_yes, prop.c = T)

#### Figure 3. Union densities ####
### Density ridgeline plot
density_ridge <- DATA01 %>%
  ggplot()+
  geom_density_ridges(mapping = aes(x = union_density, y = CBA_yes, fill = CBA_yes),
                      quantile_lines =T, quantiles = 2)+
  theme_minimal(base_family = "serif")+
  labs(y = " ", title = "Union density by CBA registration", fill =" ",
       x=" ")
density_ridge

#### Figure 4. Kaplan-Meier curves ####
SURV00 <- survfit(Surv(DATA01$TIMECBA, DATA01$EVENT) ~ 1, data = DATA01)
survplot0 <- ggsurvplot(SURV00, data = DATA01, ylim = c(0.5,1),
                        ggtheme = theme_minimal(base_family = "serif")) +
  labs(title = "All")
survplot0

### Main explanatory variables
## Survival trends by union density
SURV01 <- survfit(Surv(DATA01$TIMECBA, DATA01$EVENT) ~ DATA01$Density)
survplot1 <- ggsurvplot(SURV01, data = DATA01, ylim = c(0.5,1),
                        ggtheme = theme_minimal(base_family = "serif")) +
  labs(title = "Union density")
survplot1
survdiff(Surv(DATA01$TIMECBA, DATA01$EVENT) ~ DATA01$Density) ## significant difference

## Survival trends by type
SURV02 <- survfit(Surv(DATA01$TIMECBA, DATA01$EVENT) ~ DATA01$Type)
survplot2 <- ggsurvplot(SURV02, data = DATA01, ylim = c(0.5,1),
                        ggtheme = theme_minimal(base_family = "serif")) +
  labs(title = "Union type")
survplot2
survdiff(Surv(DATA01$TIMECBA, DATA01$EVENT) ~ DATA01$Type) ## significant difference

## Survival trends by broad sector group
SURV03 <- survfit(Surv(DATA01$TIMECBA, DATA01$EVENT) ~ DATA01$Sector)
survplot3 <- ggsurvplot(SURV03, data = DATA01, ylim = c(0.5,1),
                        ggtheme = theme_minimal(base_family = "serif")) +
  labs(title = "Sector")
survplot3
survdiff(Surv(DATA01$TIMECBA, DATA01$EVENT) ~ DATA01$Sector) ## significant difference

#### Table 2. Cox Proportional Hazards models ####
### Variable reference groups 
## reference levels
# Density
DATA01$Density <- as_factor(DATA01$Density)
DATA01$Density <- relevel(DATA01$Density, ref = "Low density")

# Type
DATA01$Type <- as_factor(DATA01$Type)
DATA01$Type <- relevel(DATA01$Type, ref = "Local/Chapter")

# Sector
DATA01$Sector <- as_factor(DATA01$Sector)
DATA01$Sector <- relevel(DATA01$Sector, ref = "All others")

## Control variables
DATA01$island_grp <- as_factor(DATA01$island_grp)
DATA01$SCOPE_new <- as_factor(DATA01$SCOPE_new)

### Models 1-6, one IV per model ###
## Model 1 Density
MODEL01 <- coxph(Surv(DATA01$TIMECBA, DATA01$EVENT) ~ Density, data = DATA01) 
stargazer(MODEL01, type="text", apply.coef=exp, title="Results", t.auto= F, p.auto=F)

cox.zph(MODEL01)

## Model 2 Density (with controls)
MODEL02 <- coxph(Surv(DATA01$TIMECBA, DATA01$EVENT) ~ Density + 
                   island_grp + SCOPE_new, data = DATA01)
stargazer(MODEL02, type="text", apply.coef=exp, title="Results", t.auto= F, p.auto=F,
          omit = c("island_grp", "SCOPE_new"))

cox.zph(MODEL02)

## Model 3 Federation/Independent
MODEL03 <- coxph(Surv(DATA01$TIMECBA, DATA01$EVENT) ~ Type, data = DATA01)
stargazer(MODEL03, type="text", apply.coef=exp, title="Results", t.auto= F, p.auto=F)

cox.zph(MODEL03)

## Model 4 Federation/Independent (with controls)
MODEL04 <- coxph(Surv(DATA01$TIMECBA, DATA01$EVENT) ~ Type + 
                   island_grp + SCOPE_new, data = DATA01)
stargazer(MODEL04, type="text", apply.coef=exp, title="Results", t.auto= F, p.auto=F,
          omit = c("island_grp", "SCOPE_new"))

cox.zph(MODEL04)

## Model 5 Broad Sector Group
MODEL05 <- coxph(Surv(DATA01$TIMECBA, DATA01$EVENT) ~ Sector, data = DATA01)
stargazer(MODEL05, type="text", apply.coef=exp, title="Results", t.auto= F, p.auto=F)

cox.zph(MODEL05)

## Model 6 Broad Sector Group (with controls)
MODEL06 <- coxph(Surv(DATA01$TIMECBA, DATA01$EVENT) ~ Sector + 
                   island_grp + SCOPE_new, data = DATA01)
stargazer(MODEL06, type="text", apply.coef=exp, title="Results", t.auto= F, p.auto=F,
          omit = c("island_grp", "SCOPE_new"))

cox.zph(MODEL06)

### Full-specification model (with controls) ###
MODEL07 <- coxph(Surv(DATA01$TIMECBA, DATA01$EVENT) ~ Density + Type +
                   Sector + island_grp + SCOPE_new, data = DATA01)
stargazer(MODEL07, type="text", apply.coef=exp, title="Results", t.auto= F, p.auto=F,
          omit = c("island_grp", "SCOPE_new"))

cox.zph(MODEL07)

#### Compiling the models into one table
stargazer(MODEL01, MODEL02, MODEL03, MODEL04, MODEL05, MODEL06, MODEL07, type = "text",
          omit = c("island_grp", "SCOPE_new"))

## saving into LaTeX
stargazer(MODEL01, MODEL02, MODEL03, MODEL04, MODEL05, MODEL06, MODEL07, 
          type="latex", out = "models1-7 (updated).tex", apply.coef=exp, 
          title="Results", t.auto= F, p.auto=F,
          covariate.labels = c("High density", "Independent union", 
                               "Manufacturing"),
          dep.var.labels = c("Time to event (CBA)"),
          omit = c("island_grp", "SCOPE_new"),
          add.lines = list(c("Island Group", "No", "Yes", "No", "Yes", "No", "Yes", "Yes"),
                           c("Scope", "No", "Yes", "No", "Yes", "No", "Yes", "Yes")),
          style = "default",
          omit.stat = c("ll", "max.rsq", "wald", "lr", "logrank"))

#### Figure 5. Forest plot for full-specification model ####
forest_model(MODEL07, covariates = c("Density", "Type", "Sector"),
             format_options = forest_model_format_options(
               colour = "black",
               color = NULL,
               shape = 15,
               text_size = 4,
               point_size = 3,
               banded = T
             ),
             factor_separate_line = T)

#### Table A1. Log-rank tests ####
survdiff(Surv(DATA01$TIMECBA, DATA01$EVENT) ~ DATA01$Density) ## significant difference
survdiff(Surv(DATA01$TIMECBA, DATA01$EVENT) ~ DATA01$Type) ## significant difference
survdiff(Surv(DATA01$TIMECBA, DATA01$EVENT) ~ DATA01$Sector) ## significant difference

#### Table A2. Proportional hazards tests ####
cox.zph(MODEL01)
cox.zph(MODEL02)
cox.zph(MODEL03)
cox.zph(MODEL04)
cox.zph(MODEL05)
cox.zph(MODEL06)
cox.zph(MODEL07)

#### Table A.3. Cox Regression Results (Continuous Union Density) ####
## Continuous union density bivariate model
MODEL08 <- coxph(Surv(DATA01$TIMECBA, DATA01$EVENT) ~ union_density, DATA01)
stargazer(MODEL08, type="text", apply.coef=exp, title="Results", t.auto= F, p.auto=F,
          omit = c("island_grp", "SCOPE_new"))
cox.zph(MODEL08)

## Continuous union density (with controls)
MODEL09 <- coxph(Surv(DATA01$TIMECBA, DATA01$EVENT) ~ union_density + 
                   island_grp + SCOPE_new, DATA01)
stargazer(MODEL09, type="text", apply.coef=exp, title="Results", t.auto= F, p.auto=F,
          omit = c("island_grp", "SCOPE_new"))
cox.zph(MODEL09)

## Continuous union density (fully-specified model)
MODEL10 <- coxph(Surv(DATA01$TIMECBA, DATA01$EVENT) ~ union_density + Type + Sector + 
                   island_grp + SCOPE_new, DATA01 )
cox.zph(MODEL10)

### table for continuous union density
stargazer(MODEL08, MODEL09, MODEL03, MODEL04, MODEL05, MODEL06, MODEL10, 
          type="latex", out = "models1-7 (density cont).tex", apply.coef=exp, 
          title="Results", t.auto= F, p.auto=F,
          covariate.labels = c("Union density", "Independent union", 
                               "Manufacturing"),
          dep.var.labels = c("Time to event (CBA)"),
          omit = c("island_grp", "SCOPE_new"),
          add.lines = list(c("Island Group", "No", "Yes", "No", "Yes", "No", "Yes", "Yes"),
                           c("Scope", "No", "Yes", "No", "Yes", "No", "Yes", "Yes")),
          style = "default",
          omit.stat = c("ll", "max.rsq", "wald", "lr", "logrank"))

#### Figure A.1. Proportional Hazards Test (Fully-Specified Model) ####
fit_mod7 <- cox.zph(MODEL07)
fit_mod7_gg <- ggcoxzph(fit_mod7,ggtheme = theme_minimal(base_family = "serif"))
fit_mod7_gg


#### Figure A.2. Interaction Models ####
DATA01$EBU_YEAR_REG <- as_factor(DATA01$EBU_YEAR_REG)

## For Density
MODEL02a <- coxph(Surv(DATA01$TIMECBA, DATA01$EVENT) ~ Density + island_grp + SCOPE_new, data = DATA01)
MODEL02int <- coxph(Surv(DATA01$TIMECBA, DATA01$EVENT) ~ EBU_YEAR_REG*Density + island_grp + SCOPE_new, data = DATA01)

anova(MODEL02a, MODEL02int, test ="Chisq") ### Significantly different

## Interaction plot
int1 <- plot_model(MODEL02int, type = "int", transform="exp", 
                   terms = c("EBU_YEAR_REG", "Density"),
                   axis.title = c("Year", "Hazard Ratios"),
                   title = "Interaction model with union density and year registered")+
  theme_minimal(base_family = "serif")
int1

## For Type
MODEL04a <- coxph(Surv(DATA01$TIMECBA, DATA01$EVENT) ~ Type + island_grp + SCOPE_new, data = DATA01)
MODEL04int <- coxph(Surv(DATA01$TIMECBA, DATA01$EVENT) ~ EBU_YEAR_REG*Type + island_grp + SCOPE_new, data = DATA01)
anova(MODEL04a, MODEL04int, test ="Chisq") ### not significantly different

## Interaction plot
int2 <- plot_model(MODEL04int, type = "int", transform="exp", 
                   terms = c("EBU_YEAR_REG", "Density"),
                   axis.title = c("Year", "Hazard Ratios"),
                   title = "Interaction model with union type and year registered")+
  theme_minimal(base_family = "serif")
int2

## Sector
MODEL06a <- coxph(Surv(DATA01$TIMECBA, DATA01$EVENT) ~ Sector + island_grp + SCOPE_new, data = DATA01)
MODEL06int <- coxph(Surv(DATA01$TIMECBA, DATA01$EVENT) ~ EBU_YEAR_REG*Sector + island_grp + SCOPE_new, data = DATA01)
anova(MODEL06a, MODEL06int, test ="Chisq") ### not significantly different

## Interaction plot
int3 <- plot_model(MODEL06int, type = "int", transform="exp", 
                   terms = c("EBU_YEAR_REG", "Density"),
                   axis.title = c("Year", "Hazard Ratios"),
                   title = "Interaction model with sector group and year registered")+
  theme_minimal(base_family = "serif")
int3

## Compiling interactions into one figure
ggarrange(int1, int2, int3, ncol = 2, nrow=2)


