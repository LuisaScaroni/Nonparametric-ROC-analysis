#setwd("C:/Users/scaro/Desktop/università/advanced non par statistics")

## Purpose: Test whether the ROC and the AROC curves of the same diagnostic marker are the same or not.
#### (from line 244 ILPD) ####

#KU LEUVEN: Master of Mathematics: Advanced-Non Parametric Statistics and Smoothing

# Packages
install.packages("viridis")
# library(refreg)
library(viridis) # color selection
library(hdrcde) # estimating the conditional densities
library(ggplot2) #graphical representations
library(ggExtra) #graphical representations
library(kableExtra) #presenting the tables

# Loading functions from the functions.R script that we were able to find in the github at the end of the paper
#NOTE: the majority of the code that we decided to employ was already implemented on the GitHub at the end of the paper, we only tried to adapt them to our own datasets
source("functions.R")


#------------------------------------
#WE START WITH BUPA DATASET#
bupa <- read.csv("bupa.data", header = FALSE)
#we try to visualize the dataset
colnames(bupa) <- c("mcv","alkphos","sgpt","sgot","ggt","drinks","selector")
head(bupa)
table(bupa$selector)
summary(bupa)
bupa$selector <- ifelse(bupa$selector == 1, 1, 0)
table(bupa$selector)
sum(is.na(bupa$ggt))
sum(is.na(bupa$drinks))
# Outcome: at first we assumed this kind of distinction 1 = diseased, 0 = healthy
# Diagnostic marker: ggt
# Covariate: drinks
#DATASET VARIABLE EXPLAINED:
#mcv= Mean Corpuscular Volume(dimension of red globuli)->high in alcoholist
#alkphos=alkaline phosphatase->high when problems with livers
#sgpt=serum glutamic pyruvis Transaminase->high with epatite, alcoholism, inflammation
#sgot=serum glutamic oxaloacetic transaminase, high when livers problems
#ggt=gamma glutamyl transferase best marker for alcohol consume effects-> MARKER USED IN OUR CASE
#drinks=number of drinks in one day->COVARIATE IN AROC MODEL
#selector=the variable which separate the dataset in two groups:diseased=1, healthy=0
# ----- Diagnostic marker -----
#WHY WE CHOOSE GGT AS A DIAGNOSTIC MARKER?
#ggt increases in drinkers, the main enzyme to give a diagnosis in hepatiits, the most discriminant one between healthy and ill
Y  = bupa$ggt #consider all the value of ggt the continuous marker
#We divide them between healthy and unhealthy
YF = bupa$ggt[bupa$selector == 1]   # diseased select ggt of ill people
YG = bupa$ggt[bupa$selector == 0]   # healthy select ggt of healthy people
sampleY = list(YF, YG) #a list necessary for allowing the Test.ROCAROC function to actually work

# ----- Covariate -----
#we choose drinks since is the most correlated to ggt, it is a covariate because it does not define a disease
X  = bupa$drinks #consider all the value of the covariate drinks
XF = bupa$drinks[bupa$selector == 1] #1-consider the disaesed one
XG = bupa$drinks[bupa$selector == 0] #0-consider the healthy one
sampleX = list(XF, XG) #create the list in order to make the function work

# Names for various plots
nameF="Liver disease"
nameG="Healthy"
nameY="ggt"
nameX="drinks"
length(YF); length(YG) #if length 0 the ROC-AROC can't work
length(XF); length(XG)

nameY = "ggt"
nameG = "Healthy"
nameF = "Diseased"
nameYG = paste(nameY, "(", nameG, ")")
nameYF = paste(nameY, "(", nameF, ")")

Y.sum = rbind(summary(Y), summary(YF), summary(YG))
rownames(Y.sum) = c(nameY, nameYF, nameYG)
kable(Y.sum) #we can notice that the values for the healthy population are higher compared to the diseased one->this is weird because usually healthy patients should be having smaller values in ggt

# Covariate
nameX = "drinks"
nameXG = paste(nameX, "(", nameG, ")")
nameXF = paste(nameX, "(", nameF, ")")


X.sum = rbind(summary(X), summary(XF), summary(XG))
rownames(X.sum) = c(nameX, nameXF, nameXG)
kable(X.sum) # We notice that some patients drink a lot (notice the maximum equal to 20) and others less
#ALL: from min, max ,median et cetera I notice that the majority does not drink a lot, but a small part has an excessive consumption of alcohol->asymmetrical distribution with  a long tail at right
#DISEASED: mean is slightly higher (not in a so significant way)in this case than in healthy from this we might suspect that alcohol might not be influencing the ggt: not a lot of them drinks excessively->the unhealthy one includes heavy outliers, bigger variability
#HEALTHY: do not have important outliers, have a better distribution, a uniform level of distribution->they drink less then the diagnosed->we don't have extreme case just as before
# Scatterplot with marginal densities
bupa$group <- factor(
  bupa$selector,
  levels = c(0, 1),
  labels = c("Healthy", "Diseased")
)

# We create a dataframe for all
bupa_all <- data.frame(
  drinks = bupa$drinks,
  group  = factor("All", levels = c("All", "Healthy", "Diseased"))
)

# We create a dataframe for diseased
bupa_hd <- data.frame(
  drinks = bupa$drinks,
  group  = bupa$group   # Healthy / Diseased
)

# We visualize through violin-boxplot
bupa_plot <- rbind(bupa_all, bupa_hd)
ggplot(bupa_plot, aes(x = group, y = drinks, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.15, outlier.colour = "red", outlier.size = 2) +
  theme_minimal() +
  labs(title = "Distribution of Drinks by Group",
       x = "Group", 
       y = "Number of Drinks per Day") +
  theme(legend.position = "none")
library(ggplot2)
library(ggExtra)

q = ggplot(bupa) +
  geom_point(aes(x = drinks, y = ggt, color = factor(selector)), 
             alpha = 0.6, shape = 16) +
  scale_color_manual(labels = c(nameG, nameF),
                     values = c("0" = viridis(5)[2], "1" = viridis(4)[3])) +
  theme_minimal() +
  theme(legend.position = "bottom") + 
  labs(x = "drinks", y = "ggt", color = "Group")
ggMarginal(q, type = "densigram", groupColour = TRUE, groupFill = TRUE, alpha = 0.3)
#the violin is larger in most common values, thinner in less common one, 
#boxplot with maximum and minimum and median in the line
#red points->outlier: we notice that in the healthy one we don't have many outliers.

#DISTRIBUTION OF GGT FOR HEALTHY AND UNHEALTHY
#if the following curve are overlapping a lot it is difficult to distinguish between healthy and unhealthy
ggplot(bupa, aes(x = ggt, fill = group)) +
  geom_density(alpha = 0.4) +
  theme_minimal() +
  labs(title = "Distribution of GGT for Healthy and Diseased",
       x = "GGT", y = "Density")
#From the plot it is easy to notice the problem that we were describing previously, in fact for very small value of ggt the diseased distribution is higher, in the meanwhile for the healthy one it has an higher density at higher ggt values compared to the diseased one
# Conditional densities F/G
plot.densities.FG(XF, YF, XG, YG,
                  x.name = nameX,
                  y.name = nameY,
                  col1 = viridis(5, alpha = 0.4)[2],
                  col2 = viridis(4, alpha = 0.4)[3])
#GREEN=healthy, BLUE=diseased->how ggt values are distributed with the change of drinks
#with higher number of drinks also higher ggt->we notice that drinks does not modify the discrimination so ROC is similar to AROC
# Testing ROC vs AROC
set.seed(1234)
results.1 = Test.ROCAROC(sampleY, sampleX, nboots = 200, nr = 2)
#the bigger result$statistics are the more are different, the smaller the more similar
#0.048075->very small, 0.003177103->near to 0, 0.1359564>small
results.1$statistics
results.1$pvalues
#0.83->high, 0.915->high, 0.955->high in the three space we can accept the null hypothesis
#all p-value above 0.05 we don't reject H0->ROC pooled and AROC are the same
#AROC parameters is not more useful than standard ROC
#therefore, adjusting for the covariate ‘drinks’ does not change the diagnostic accuracy of GGT.
#Adjusting for the covariate means evaluating the accuracy of the marker GGT after removing the effect that the covariate ‘drinks’ has on its values. In practice, we correct GGT for different drinking levels before comparing healthy and diseased individuals. In our dataset, this adjustment does not change the ROC curve, meaning that the discriminatory ability of GGT is stable across different drinking levels
plot(results.1$p, results.1$ROC, type = "l", col = "orange", lwd = 2,
     xlab = "p (False Positive Rate)", ylab = "ROC(p)", ylim = c(0,1))
lines(results.1$p, results.1$AROC, col = "darkred", lwd = 2)
legend("bottomright", c("ROC", "AROC"), col = c("orange","darkred"), lwd = 2)
#from ROC and AROC graph we understand that the covariate effect is very small
#From this simulation we learned that adjusting for the covariate ‘drinks’ does not change the diagnostic accuracy of GGT. Although drinking level affects the absolute values of GGT, it does not affect how well GGT separates healthy and diseased individuals. This is why the ROC and AROC curves are almost identical
#an high ggt does not imply a disease but more the fact that maybe they drink a lot ( in reality two random groups 1 and 2)
kable(cbind(results.1$statistics, results.1$pvalues),
      caption="Test ROC vs AROC")

#We calculate AUC and AAUC, if the curves looks similar and AUC is similar to AAUC my conclusion is stronger
#When AUC ≈ AAUC and AUCₓ shows little variation, the covariate does not modify the discriminatory capability of the marker, and adjustment is unnecessary.
install.packages("pROC")
install.packages("pracma")
library(pracma)
library(pROC)
#AUC is a global measure of the diagnostic accuracy of a marker.
auc_pooled <- auc(bupa$selector, bupa$ggt)

# Compute AAUC
AAUC <- trapz(results.1$p, results.1$AROC)

auc_pooled #0.6284->the marker has moderate but not strong accuracy->So a diseased subject has a higher GGT value than a healthy subject in about 63% of the pairs.
AAUC  #0.3582732 ->a much weaker discriminatory capability
#the pooled ROC overestimates accuracy when the covariate distributions differ strongly between groups
#AAUC (Area under the AROC curve) averages AUCₓ across the covariate distribution and represents the covariate-adjusted diagnostic accuracy
#AAUC removes the influence of the covariate drinks, which artificially increases discrimination in the pooled ROC
#conditional AUC_X 
library(hdrcde)
#The conditional AUCₓ measures the discriminatory capability of the marker at a specific value of a covariate, such as drinks = 5 or drinks = 15.
AUC_x_fun <- function(x0, XF, YF, XG, YG, h = NULL) {
  
  if (is.null(h)) {
    h <- 1.06 * sd(c(XF, XG)) * length(c(XF, XG))^(-1/5)
  }
  
  # kernel weights
  wF <- dnorm((XF - x0)/h)
  wG <- dnorm((XG - x0)/h)
  
  # normalizza
  wF <- wF / sum(wF)
  wG <- wG / sum(wG)
  
  # calcola AUC_x stimato
  auc_val <- 0
  for (i in seq_along(YF)) {
    for (j in seq_along(YG)) {
      auc_val <- auc_val + wF[i] * wG[j] * (YF[i] > YG[j])
    }
  }
  return(auc_val)
}


grid_x <- seq(min(X), max(X), length.out = 10)

AUCx <- sapply(grid_x, function(xx) AUC_x_fun(xx, XF, YF, XG, YG))

# plots
plot(grid_x, AUCx, type = "b",
     main = "Conditional AUC_x across covariate values (corrected)",
     xlab = "drinks", ylab = "AUC_x", ylim = c(0,1))

abline(h = auc_pooled, col = "orange", lwd = 2)
abline(h = AAUC, col = "red", lwd = 2, lty = 2)

legend("bottomright", legend = c("AUC_x", "AUC pooled", "AAUC"),
       col = c("black", "orange", "red"), lty = c(1,1,2))
#it fluctuate around AUUC=0.36, it is always lower than the pooled AUC->the discriminatory capability does not depend on drinks 
#the true discriminatory capability is weaker than the pooled AROC suggests
#match scenario C in paper
##CONCLUSION: we conclude by saying that the selector was not employed in the right way, indeed since on kaggle it was not specified we assumed that it was created in order to split between healthy and diseased
#which was also the idea that had more sense in this case, but at the end we were able to notice that it was just a random splitting between two random groups(like group 1 and group 2)



#---------------------------------------------------------------------
#We now choose another dataset in order to do a more accurate case study->
#we have chosen ILPD  a dataset on Kaggle about liver disorders

setwd("C:/Users/scaro/Desktop/università/advanced non par statistics")

# Packages as we did before
install.packages("viridis")
library(viridis)
library(hdrcde)
library(ggplot2)
library(ggExtra)
library(kableExtra)

# Load Test.ROCAROC: present in the github of the paper
source("functions.R")

# Load ILPD dataset 
ilpd <- read.csv("indian_liver_patient.csv")

# Rename columns
# Our dataset has these variables:
# Age, Gender, Total_Bilirubin, Direct_Bilirubin,
# Alkaline_Phosphotase, Alamine_Aminotransferase,
# Aspartate_Aminotransferase, Total_Protiens,
# Albumin, Albumin_and_Globulin_Ratio, Dataset (1=Diseased, 2=Healthy)

#Then we chose the various variable:
#covariate: age continuous->age is not a strong predictor of ALT
#marker: ALT continuous->it increases when the liver is damaged->higher in ill people
#dataset: healthy or diagnosed
colnames(ilpd)

# Fix selector: diseased = 1, healthy = 0
ilpd$selector <- ifelse(ilpd$Dataset == 1, 1, 0)
table(ilpd$selector)

summary(ilpd)

# We check if some values are missing
sum(is.na(ilpd$Alamine_Aminotransferase))
sum(is.na(ilpd$Age))

#We choose marker and covariate

# Diagnostic marker: ALT (Alamine Aminotransferase)
# Reason: ALT is the standard enzyme for liver damage like GGT in BUPA
Y  = ilpd$Alamine_Aminotransferase
YF = Y[ilpd$selector == 1]
YG = Y[ilpd$selector == 0]
sampleY = list(YF, YG)

# Covariate: Age
# Reason: continuous, clinically relevant, affects enzyme levels
X  = ilpd$Age
XF = X[ilpd$selector == 1]
XG = X[ilpd$selector == 0]
sampleX = list(XF, XG)

# Naming for plots
nameF = "Liver Disease"
nameG = "Healthy"
nameY = "ALT"
nameX = "Age"

length(YF); length(YG)
length(XF); length(XG)


# Exploratory Summary we did the same with BUPA
nameYG = paste(nameY, "(", nameG, ")")
nameYF = paste(nameY, "(", nameF, ")")

#In general healthy people should be having a small values of alt(if the selector is chosen in the right way)
Y.sum = rbind(summary(Y), summary(YF), summary(YG))
rownames(Y.sum) = c(nameY, nameYF, nameYG)
kable(Y.sum)#In this case we are satisfied, indeed diseased patients have higher values for mean, median, max and min->it makes sense

nameXG = paste(nameX, "(", nameG, ")")
nameXF = paste(nameX, "(", nameF, ")")

X.sum = rbind(summary(X), summary(XF), summary(XG))
rownames(X.sum) = c(nameX, nameXF, nameXG)
kable(X.sum)#ill people tend to have an higher age (but not so significantly usually only around 6 years we can also think that this is connected to the small size sample)


# Violin + Boxplot like we did for BUPA
#similar distribution ->we don't have huge differences->age for now does not look so useful
ilpd$group <- factor(ilpd$selector,
                     levels = c(0,1),
                     labels = c("Healthy", "Liver Disease"))

ilpd_all <- data.frame(
  Age = ilpd$Age,
  group = factor("All", levels = c("All","Healthy","Liver Disease"))
)

ilpd_hd <- data.frame(
  Age = ilpd$Age,
  group = ilpd$group
)

ilpd_plot <- rbind(ilpd_all, ilpd_hd)

ggplot(ilpd_plot, aes(x = group, y = Age, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.15, outlier.colour = "red", outlier.size = 2) +
  theme_minimal() +
  labs(title = "Distribution of Age by Group",
       x = "Group", y = "Age") +
  theme(legend.position = "none")
#the age does not affect it too much

# Scatterplot + Marginal densities
#The graph is asymmetrical-> We have some huge outliers->higher in ill people ->but it is not affected a lot by age
#Indeed the majority of the outliers is in a range of age between 25 and 50 so not very old age
q = ggplot(ilpd) +
  geom_point(aes(x = Age, y = Alamine_Aminotransferase, color = factor(selector)),
             alpha = 0.6, shape = 16) +
  scale_color_manual(labels = c(nameG, nameF),
                     values = c("0" = viridis(5)[2], "1" = viridis(4)[3])) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "Age", y = "ALT", color = "Group")

ggMarginal(q, type = "densigram", groupColour = TRUE, groupFill = TRUE, alpha = 0.3)


# Density of ALT by group 
#both strongly asymmetric, diseased has in general higher ALT then healthy, the two distribution are overlapping a lot in downs
#we can notice that for smaller alt the density function for healthy is higher->it makes sense!
ggplot(ilpd, aes(x =Alamine_Aminotransferase , fill = group)) +
  geom_density(alpha = 0.4) +
  theme_minimal() +
  labs(title = "Distribution of ALT for Healthy and Diseased",
       x = "ALT", y = "Density")


# Conditional densities:
#healthy and diseased have similar distributions, both are near alt 0-100, the difference between healthy and unhealthy does not increase with age, long tail in both cases->age has no effect on discrimination
plot.densities.FG(XF, YF, XG, YG,
                  x.name = nameX,
                  y.name = nameY,
                  col1 = viridis(5, alpha=0.4)[2],
                  col2 = viridis(4, alpha=0.4)[3])


# ROC vs AROC Test
set.seed(1234)
results.1 = Test.ROCAROC(sampleY, sampleX, nboots = 200, nr = 2)

results.1$statistics
results.1$pvalues
#T1->0.79, T2->0.585, T3->0.58-> we can accept the null hypothesis since p-value>0.05
plot(results.1$p, results.1$ROC, type="l", col="orange", lwd=2,
     xlab="False Positive Rate", ylab="True Positive Rate", ylim=c(0,1))
lines(results.1$p, results.1$AROC, col="darkred", lwd=2)
legend("bottomright", c("ROC","AROC"), col=c("orange","darkred"), lwd=2)

kable(cbind(results.1$statistics, results.1$pvalues),
      caption="Test ROC vs AROC – ILPD")
#We don't reject H0 since all the p-value>0.05,  for all of them AROC=ROC->the covariate does not modify the results
#In the graph we can notice that the curve are almost overlapping->a similar scenario to B/C

# AUC, AAUC
install.packages("pROC")
install.packages("pracma")
library(pROC)
library(pracma)

auc_pooled <- auc(ilpd$selector, Y)
AAUC <- trapz(results.1$p, results.1$AROC)

auc_pooled  #0.6856->in 68% the marker has an higher value for the diseased patients then healthy
AAUC #0.6954202->->discriminatory capability of the marker is 69.5%
#The pooled AUC (0.686) and the covariate-adjusted AAUC (0.695) are extremely close.
#This indicates that the covariate has only a minimal impact on the discriminatory capability of the marker.
#The diagnostic accuracy remains essentially the same after adjustment, suggesting that the ROC and AROC curves are statistically indistinguishable.

# Conditional AUC_x
AUC_x_fun <- function(x0, XF, YF, XG, YG, h=NULL){
  if (is.null(h)) h <- 1.06 * sd(c(XF,XG)) * length(c(XF,XG))^(-1/5)
  wF <- dnorm((XF - x0)/h); wG <- dnorm((XG - x0)/h)
  wF <- wF/sum(wF); wG <- wG/sum(wG)
  
  val <- 0
  for(i in seq_along(YF)){
    for(j in seq_along(YG)){
      val <- val + wF[i] * wG[j] * (YF[i] > YG[j])
    }
  }
  return(val)
}

grid_x <- seq(min(X), max(X), length.out = 10)
AUCx <- sapply(grid_x, function(xx) AUC_x_fun(xx, XF, YF, XG, YG))

plot(grid_x, AUCx, type="b",
     main="Conditional AUC_x across Age values",
     xlab="Age", ylab="AUC_x", ylim=c(0,1))

abline(h = auc_pooled, col="orange", lwd=2)
abline(h = AAUC, col="red", lwd=2, lty=2)

legend("bottomright", c("AUC_x","AUC pooled","AAUC"),
       col=c("black","orange","red"), lty=c(1,1,2))

#AUCx points oscillates around 0.70
#orange line:AUC pooled
#AAUC=red line
#->the age does not affect the capability of the marker to discriminate between healthy and diagnosed
#equally informative for every age
#only for extreme old age(above 80) two pints near to one
#that's because in the extreme I don't have many data->because of small sample size

lpd <- read.csv("indian_liver_patient.csv")
ilpd$selector <- ifelse(ilpd$Dataset == 1, 1, 0)
ilpd <- na.omit(ilpd) 

Y_ilpd = ilpd$Alamine_Aminotransferase
X_ilpd = ilpd$Age
YF_i = Y_ilpd[ilpd$selector == 1]; YG_i = Y_ilpd[ilpd$selector == 0]
XF_i = X_ilpd[ilpd$selector == 1]; XG_i = X_ilpd[ilpd$selector == 0]
sampleY_i = list(YF_i, YG_i); sampleX_i = list(XF_i, XG_i)

png("ILPD_Exploratory.png", width = 1200, height = 600, res = 120)
par(mfrow = c(1, 2))
boxplot(Age ~ selector, data = ilpd, names = c("Healthy", "Liver Disease"),
        col = c("#00BA38", "#619CFF"), main = "Age Distribution by Group")
plot.densities.FG(XG_i, YG_i, XF_i, YF_i, x.name = "Age", y.name = "ALT",
                  col1 = viridis(5, alpha=0.4)[2], col2 = viridis(4, alpha=0.4)[3])
dev.off()


res_ilpd = Test.ROCAROC(sampleY_i, sampleX_i, nboots = 100, nr = 2)


png("ILPD_Results.png", width = 1200, height = 600, res = 120)
par(mfrow = c(1, 2))
plot(res_ilpd$p, res_ilpd$ROC, type="l", col="orange", lwd=2, main="ILPD: ROC vs AROC")
lines(res_ilpd$p, res_ilpd$AROC, col="darkred", lwd=2)
legend("bottomright", c("ROC", "AROC"), col=c("orange","darkred"), lwd=2)


grid_xi <- seq(min(X_ilpd), max(X_ilpd), length.out = 10)
AUCxi <- sapply(grid_xi, function(xx) AUC_x_fun(xx, XF_i, YF_i, XG_i, YG_i))
plot(grid_xi, AUCxi, type="b", main="ILPD: Conditional AUC_x", xlab="Age", ylab="AUCx")
dev.off()


