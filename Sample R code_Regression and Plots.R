#### Sample R code
#### Regression and Plots
#### Suji Kang

## install and load packages
packages <- c("lmtest", "tidyverse", "haven", "stargazer", "janitor",
              "labelled", "estimatr", "lme4",
              "plm", "sandwich", "clubSandwich", "commarobust", "dotwhisker",
              "margins", "ggplot2", "jtools", "broom",
              "ggthemes", "gridExtra", "scales",
              "car", "modelsummary")
package_installed <- sapply(packages, function(pack)
  pack %in% rownames(installed.packages()))
if (any(!package_installed)) {
  sapply(packages[!package_installed], install.packages)
}
sapply(packages, require, character.only = TRUE)
rm(packages,package_installed)


## load data
long_merged <- read.csv("long_merged.csv",
                        stringsAsFactors = F)


#################################
## Run Base Regression
#################################
## to test H1 (noncredible vs. credible)
lm1 <- lm(belief ~ source + topic_question + party +  
            Gender + Education + Religion + 
            race_white + race_black + race_hispanic + race_asian + race_other,
          data=long_merged[long_merged$trt=="trt1",])
lm1_se <- coeftest(lm1, vcov. = vcovCL(lm1, 
                                       cluster = long_merged[long_merged$trt=="trt1",]$PID, 
                                       type = "HC2"))

lm2 <- lm(belief ~ source + topic_question + party +  
            Gender + Education + Religion + 
            race_white + race_black + race_hispanic + race_asian + race_other,
          data=long_merged[long_merged$trt=="trt2",])
lm2_se <- coeftest(lm2, vcov. = vcovCL(lm2, 
                                       cluster = long_merged[long_merged$trt=="trt2",]$PID, 
                                       type = "HC2"))

#################################
## Regression Output for Latex
#################################
stargazer(lm1, lm2,
          type = "latex", 
          dep.var.labels=c('Dep. Variable Name'),
          title = "Title of the Table",
          covariate.labels =  c("Covariates Names"),
          omit = c("Name of variables you want to omit"),
          header = FALSE,
          se = list(lm1_se[,2], lm2_se[,2]),
          p = list(lm1_se[,4], lm2_se[,4]),
          column.labels = c("Column 1", "Column 2"),
          keep.stat=c("n"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          add.lines=list(c("Covariates Included", "Y", "Y"), 
                         c("Topic Fixed Effects", "Y", "Y")),
          notes = "Clustered standard errors in parentheses.")


#################################
## Coefficient Plot
#################################
## plot
coefplot_allmodels <- dwplot(all_models, 
                             dot_args = list(aes(shape = model, colour = model)),
                             size = 3) +
  theme_bw() + 
  labs(title = "Title of the Figure", 
    x = "X axis", 
    y = "Y axis") +
  # xlim( , ) +  
  theme(plot.title = element_text(face="bold", size=12),
        legend.position = "bottom",
        legend.background = element_rect(colour="grey80"),
        legend.title.align = .5) +
  geom_vline(xintercept = 0,
             colour = "grey60",
             linetype = 2) +
  scale_shape_discrete(name  ="Models", breaks = c(0, 1))

## save plot as a pdf
pdf(file="plot.pdf",
    height=4.5, width=6, family="sans")
coefplot_allmodels
dev.off()


