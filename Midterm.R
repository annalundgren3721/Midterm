rm(list=ls())

tephritis <- read.csv("C:/Users/Anna/Documents/Statistics course/Midterm/tephritis.txt")

#Making some datasets
dat <- tephritis[c(1:5)]            #this dataset only contains the sex, patry, hostplant, BL and OL 
dat_fem <- dat[complete.cases(dat),]  #this is the dat dataset but only with females and no NA

#First, making some statistics
hist(dat_fem$OL)  #Histogram over the Ovipositor Lenght (OL)
hist(dat_fem$BL)  #Histogram over the Body Length (BL)

#OL looks normally distributed but not BL. Should log-transform them both
#Log transform
log_OL = log(dat_fem$OL)
log_BL = log(dat_fem$BL)

#Question 1: Does BL and OL have a relationship?
m_BL = lm(log_OL ~ log_BL)
summary(m_BL)

#Plotting the relationship between BL and OL
library(ggplot2)

ggplot(dat_fem, aes(x = log(BL), y = log(OL), color = "darkgrey")) + 
  geom_point(col = "darkgrey", pch = 1) + theme_classic() + 
  labs(title = "Relationship between Body Length and Ovipositor Length", 
       x = "Log-transformed Ovipositor Length (mm)", y = "Log transformed Body Length (mm)") + 
  geom_smooth(method = "lm", se = FALSE, col = "black", lwd = 0.6)


#For question 2, I will make an ANOVA for OL as variable for Patry-level and Hostplant

#To begin with, some statistics on patry, hostplant and OL
dat_fem$Patry = paste0(dat_fem$Patry, "P")
dat_fem$Hostplant = paste0(dat_fem$Hostplant, "H")
means = tapply(log(dat_fem$OL), list(dat_fem$Hostplant, dat_fem$Patry), mean)
means             #Here we get the mean values of the OL for patry and hostplant

#Making a model for the ANOVA
m = lm(log(OL) ~ Hostplant*Patry, data = dat_fem)   
anova(m)
summary(m)
confint(m)  #confidence interval

#Making a model for each of the possible interactions between OL, hostplant and patry
m                                                          #model with hostplant and patry
m_h <- lm(log(OL) ~ Hostplant, data = dat_fem)             #model with only hostplant
m_p <- lm(log(OL) ~ Patry, data = dat_fem)                 #model with only patry
m_null <- lm(log(OL) ~ 1, data = dat_fem)                  #null-model
m_add <- lm(log(OL) ~ Hostplant + Patry, data = dat_fem)   #additive model, not interacting

#Making a table for selection, containg df, AIC, delta, logLIK and weight
mlist = list(m, m_h, m_p, m_null, m_add)
AICTab = AIC(m, m_h, m_p, m_null, m_add)
AICTab$logLik = unlist(lapply(mlist, logLik))
AICTab = AICTab[order(AICTab$AIC, decreasing=F),]
AICTab$delta = round(AICTab$AIC - min(AICTab$AIC), 2)
lh = exp(-0.5*AICTab$delta)
AICTab$w = round(lh/sum(lh), 2)
AICTab

#Calculating the standrad errors for the OL depending on the hostplant and patry-level
ses = tapply(log(dat_fem$OL),                              
             list(dat_fem$Hostplant, dat_fem$Patry), 
             function(x) sd(x)/sqrt(sum(!is.na(x))))
ses

#Plotting the interaction
interaction.plot(dat_fem$Patry, dat_fem$Hostplant, log(dat_fem$OL), legend = F,
                 main="Differences of Ovipository Length depending on 
                 Hostplant and Patry-level",
                 cex.main=1.0,
                 xlab="Patry-level",
                 ylab="Logtransformed Ovipository Length (mm)",
                 col= c("maroon2","darkblue"),
                 xaxt="n", las=1, pch=c(21,16), color="white",
                 ylim = c(0.45, 0.6) )

axis(1, 1:2, labels=c("Allopatry", "Sympatry"))  #Fixing the x-axis

#Adding arrows for the ses
arrows(c(1.0), means[,1]-ses[,1], c(1.0), 
       means[,1]+ses[,1], length=0.05, angle=90, code=3, col="darkblue")
arrows(c(2.0), means[,2]-ses[,2], c(2.0), 
       means[,2]+ses[,2], length=0.05, angle=90, code=3, col="maroon2")
arrows(c(1.0), means[1,1]-ses[1,1], c(1.0), 
       means[1,1]+ses[1,1], length=0.05, angle=90, code=3, col="maroon2")
arrows(c(2.0), means[2,2]-ses[2,2], c(2.0), 
       means[2,2]+ses[2,2], length=0.05, angle=90, code=3, col="darkblue")

points(c(1.0), means[1,1], pch=c(21,16), bg="maroon2")
points(c(1.0), means[2,1], pch=c(21,16), bg="darkblue")
points(c(2.0), means[1,2], pch=c(21,16), bg="maroon2")
points(c(2.0), means[2,2], pch=c(21,16), bg="darkblue")

legend(
  "topright", 
  legend = c("Heterophyllum", "Oleraceum"),
  fill= c("maroon2", "darkblue"),
  title = "Hostplant")

#All the statistics needed 
anova(m)
summary(m)
colMeans(means)
rowMeans(means)
means

#Question 3, how would the variance of the BL interact with the ANOVA?
#ANCOVA model
m_anc = lm(log(OL) ~ Hostplant * Patry + log(BL), data = dat_fem)
summary(m_anc)
anova(m_anc)

#Making a new column in dat_fem with both Patry-level and Hostplant
dat_fem$Groups = paste(dat_fem$Patry, dat_fem$Hostplant, sep = " ") 

#Plotting them together
#colors I want
group.colors = c("AllopatryP HeterophyllumH" = "lightpink", "AllopatryP OleraceumH" = "lightblue", 
                 "SympatryP HeterophyllumH" = "purple", "SympatryP OleraceumH" = "darkgrey")

#the plot
ggplot(dat_fem, aes(x= log(BL), y = log(OL), color = Groups)) + 
  geom_point() + theme_classic() + 
  labs(title = "Logtransformed Body Length and Ovipositor Length 
       Interacting with Hostplant and Patry-level", x = "Logtransformed Body Length (mm)",
       y = "Logtransformed Ovipositor Length (mm)") + 
  geom_smooth(method = "lm", se = FALSE) + 
  scale_color_manual(values = group.colors)


#Models for this
nm = lm(log(OL) ~ log(BL) * Groups, data = dat_fem)          #interaction between BL and the ANOVA-interaction
nm_add = lm(log(OL) ~ log(BL) + Groups, data = dat_fem)      #additive, the one I called m_anc before
nm_null = lm(log(OL) ~ 1, data = dat_fem)                    #null model
nm_BL = lm(log(OL) ~ log(BL), data = dat_fem)                #With only body length

#and the new table
mlist = list(nm, nm_add, nm_null, nm_BL )   
AICTab = AIC(nm, nm_add, nm_null, nm_BL)
AICTab$logLik = unlist(lapply(mlist, logLik))
AICTab = AICTab[order(AICTab$AIC, decreasing=F),]
AICTab$delta = round(AICTab$AIC - min(AICTab$AIC), 2)
lh = exp(-0.5*AICTab$delta)
AICTab$w = round(lh/sum(lh), 2)
AICTab

