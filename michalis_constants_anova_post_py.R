#Home path: setwd("C:/Users/Lukas/Dropbox/Lab/Lab Data/Glucose Consumption/Scripts")
#Work path: setwd("C:/Users/Student2/Dropbox/Lab/Lab Data/Glucose Consumption/Scripts")
library('multcompView')
setwd("C:/Users/Student2/Dropbox/Lab/Lab Data/Glucose Consumption/Glucose_Consumption.Oxygen_Tension.Michalis")
michalis_constants <- read.csv('Michalis_Constants_Python.csv')
pigs <- c("P19", "P20", "P21", "P22", "P23")
michalis_stats <- NULL
for (pig in pigs){
  michalis_constants_pig <- michalis_constants[michalis_constants$Pig == pig, ]
  michalis_constants_pig$Experimental_Groups <- factor(michalis_constants_pig$Experimental_Groups)
  km_anova.values <- aov(Km ~ Experimental_Groups, data = michalis_constants_pig)
  vmax_anova.values <- aov(Vmax ~ Experimental_Groups, data = michalis_constants_pig)
  km.tukey <- extract_p(TukeyHSD(km_anova.values))[[1]]
  vmax.tukey <- extract_p(TukeyHSD(vmax_anova.values))[[1]]
  for (group in levels(michalis_constants_pig$Experimental_Groups)){
    sub_constants <- michalis_constants_pig[michalis_constants_pig$Experimental_Groups
                                            == group, ]
    switch(group,
           '2.5%-NP' = {p_val = c(NA, km.tukey[2], km.tukey[1], NA, vmax.tukey[2], vmax.tukey[1])},
           '5%-NP' = {p_val = c(km.tukey[2], NA, km.tukey[3], vmax.tukey[2], NA, vmax.tukey[3])},
           '21%-NP' = {p_val = c( km.tukey[1], km.tukey[3], NA, vmax.tukey[1], vmax.tukey[3], NA)})
    p_val = unname(p_val)
    michalis_stats <- rbind(michalis_stats, data.frame(Km_Mean = mean(sub_constants$Km), Km_SD = sd(sub_constants$Km),
                                                       Vmax_Mean = mean(sub_constants$Vmax), Vmax_SD = sd(sub_constants$Vmax),
                                                       Pig = pig, Exp_Grp = group, an_km_p = summary(km_anova.values)[[1]][['Pr(>F)']][[1]],
                                                       an_vmax_p = summary(vmax_anova.values)[[1]][['Pr(>F)']][[1]], km_2.5 = p_val[1],
                                                       km_5 = p_val[2], km_21 = p_val[3], vmax_2.5 = p_val[4], vmax_5 = p_val[5],
                                                       vmax_21 = p_val[6]))
    # print(mean(sub_constants$Km))
    # print(mean(sub_constants$Vmax))
    # print(sd(sub_constants$Km))
    # print(sd(sub_constants$Vmax))
  }

  # michalis_stats[michalis_stats$Pig == pig, 'p_val_km_anova'] <- summary(km_anova.values)[[1]][['Pr(>F)']][[1]]
  # michalis_stats[michalis_stats$Pig == pig, 'p_val_vmax_anova'] <- summary(vmax_anova.values)[[1]][['Pr(>F)']][[1]]
  # print(pig)
  # print(summary(km_anova.values))
  # print(summary(vmax_anova.values))

}
print(michalis_stats)
