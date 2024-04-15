## Linear mixed models
# Load packages and dataset...
required_packages=c("tidyr","dplyr","lmerTest","lme4","emmeans","easystats","sjmisc","sjPlot","broom","VGAM","confintr","effectsize","ggplot2","ggpubr","plotrix","lavaan","semPlot")
lapply(required_packages,library,character.only=TRUE)
data=read.csv("xx/xx/Data.csv")
# Data preprocessing into a long format ...
# Model
m1=lmer(dif~group*learning_stage*anxiety+(1|ID),data=data_long)
m2=lmer(dif~group*learning_stage*anxiety+(1+leaarning_stage|ID),data=data_long)
# model overview and model comparison with and without random slope
anova(m1)
anova(m2)
anova(m1,m2)
# effect size of winning model  
effectsize::effectsize(anova(m1),type="eta",alternative="two.sided")
# statistical assumptions
# homogeneity of variance 
data_long$residuals_m1=residuals(m1)
data_long$residuals_sq_m1=abs(residuals(m1))^2
hist(data_long$residuals_sq_m1)
lev_m1=lm(residuals_sq_m1~ID,data=data_long) #ANOVA of the squared residuals
anova(lev_m1)
levRes=car::leveneTest(residuals_sq_m1~ID,data=data_long)
levRes
# normality of residuals
ggplot(data=data_long,aes(x=residuals_m1))+geom_histogram()
car::qqPlot(data_long$residuals_m1)
# marginals and post-hocs 
em1=emmeans(m1,specs = pairwise~group*learning_stage)
em1$emmeans
em1$contrasts
# trend
plot_model(m1,type="eff",terms=c("group", "learning_stage"))


## Mediation/path analyses
# Load packages and dataset
# Preprocessing...
# fully-variant model (grouped by type)
med='
# Main effects
M1 ~ c("a1","a2")*IV + C1 + C2 + C3 + C4 + C5 + C6
M2 ~ c("b1","b2")*M2 + c("c1","c2")*IV + C1 + C2 + C3 + C4 + C5 + C6
DV ~ c("d1","d2")*IV + c("e1","e2")*M1 + c("f1","f2")*M2 + C1 + C2 + C3 + C4 + C5 + C6

# Indirect effects definitions
a1e1:=a1*e1
c1f1:=c1*f1
a1b1f1:=a1*b1*f1
m1_total:=a1e1+c1f1+a1b1f1

a2e2:=a2*e2
c2f2:=c2*f2
a2b2f2:=a2*b2*f2
m2_total:=a2e2+c2f2+a2b2f2
'
set.seed(1234)
m=sem(med,data = data, group="type", bootstrap = 10000,se = "bootstrap",group.equal = c("intercepts"),fixed.x = F, auto.var=TRUE)
summary(m, standardized = TRUE,fit.measures=TRUE,rsquare=TRUE)
parameterestimates(m, boot.ci.type = "bca.simple",level = 0.95)


## t-test tests
variables = c("V1", "V2", "V3", "V4", "V5", "V6", "V7")

results_list = list()
for (var in variables) {
  t_test_result = t.test(as.formula(paste(var, "~ type")), data = data)
  cohens_d_result = cohen.d(as.formula(paste(var, "~ type")), data = data)
  summary_stats = data %>% group_by(type) %>% summarise(
    mean = mean(!!sym(var), na.rm = TRUE),
    sd = sd(!!sym(var), na.rm = TRUE)
  )
  results_list[[var]] = list(
    t_test_result = tidy(t_test_result),
    cohens_d_result = cohens_d_result,
    summary_stats = summary_stats
  )
}
# Print results
for (var in c(variables)) {
  cat("Results for", var, ":\\n")
  cat("T-test results:\\n")
  print(results_list[[var]]$t_test_result)
  if (!is.null(results_list[[var]]$cohens_d_result)) {
    cat("Cohen's d:\\n")
    print(results_list[[var]]$cohens_d_result)
  }
  if (!is.null(results_list[[var]]$summary_stats)) {
    cat("Summary statistics (mean and SD by group):\\n")
    print(results_list[[var]]$summary_stats)
  }
  cat("\\n")
}


## Visualization
# E.g., plot mediation
# extracted values from the lavaan output
a_estimate=xx; a_pval=xx  # X -> Mediator
b_estimate=xx; b_pval=xx  # Mediator -> Y
c_estimate=xx; c_pval=xx  # X -> Y (direct effect)
ab_estimate=xx; ab_pval=xx  # X -> Y (indirect effect)
# create labels with formatted p-values
a_label=sprintf("%.2f%s (a)", a_estimate, format_pvalue(a_pval))
b_label=sprintf("%.2f%s (b)", b_estimate, format_pvalue(b_pval))
c_label=sprintf("%.2f%s (c)", c_estimate, format_pvalue(c_pval))
ab_label=sprintf("%.2f%s (c')", ab_estimate, format_pvalue(ab_pval))
# base plot with variables as rectangles
p1_free_M=ggplot(df_positions, aes(x, y))+geom_rect(data=df_positions,aes(xmin=x - 0.5,xmax=x + 0.5,ymin = y - 0.2,ymax = y + 0.2),
                                                    fill="grey80",color = "black")+
  geom_text(aes(label = var),size = 5,fontface = "bold")+
  xlim(0.5,5.5) + ylim(0.2,2.2) +
  theme_void()
# path a
p1_free_M=p1_free_M+geom_segment(aes(x=df_positions[1,]$x+0.5,y=df_positions[1,]$y,
                                     xend=df_positions[2,]$x-0.5,yend=df_positions[2,]$y),
                                 arrow=arrow(type="closed",length=arrow_length),
                                 color="black",linetype="solid",linewidth=1)+
  geom_text(aes(x=1.8,y=1.5,label=a_label),color= "black",size=4.5,angle=angle_a+12)
# path b
p1_free_M=p1_free_M+geom_segment(aes(x=df_positions[2,]$x+0.5,y=df_positions[2,]$y,
                                     xend=df_positions[3,]$x-0.5,yend=df_positions[3,]$y),
                                 arrow=arrow(type="closed",length=arrow_length),
                                 color="black",linetype="solid",linewidth=1)+
  geom_text(aes(x=4.2,y=1.5,label=b_label),color= "black",size=4.5,angle=angle_b-12)
# path c
p1_free_M=p1_free_M+geom_segment(aes(x=df_positions[1,]$x+0.5,y=df_positions[1,]$y-0.1,
                                     xend=df_positions[3,]$x-0.5,yend=df_positions[3,]$y-0.1),
                                 arrow=arrow(type="closed",length=arrow_length),
                                 color="grey", linetype = "solid",linewidth=1) +
  geom_text(aes(x=3, y=0.8,label=c_label),color = "grey", size =4.5,angle=0)
# path ab
p1_free_M=p1_free_M+geom_segment(aes(x=df_positions[1,]$x+0.5,y=df_positions[1,]$y,
                                     xend=df_positions[3,]$x-0.5,yend=df_positions[3,]$y),
                                 arrow=arrow(type = "closed",length=arrow_length),
                                 color = "black",linetype="dashed",linewidth=1)+
  geom_text(aes(x=3,y =1.1,label=ab_label),color = "black", size=4.5)
print(p1_free_M)