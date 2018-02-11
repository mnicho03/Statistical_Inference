#load libraries
library(ggplot2)

#set all variables based on the prompt

# lamdba = 0.2, with 40 exponentials, over 1000 samples
lambda <- .2
n <- 40
simulations <- 1000

#set seed for reproducible simulations
set.seed(16)

#show distributions of mean over 1,000 simulations
means <- NULL
for (i in 1:simulations) means <- c(means, mean(rexp(40, lambda)))
#create data frame for ggplot & display red line at the mean
SimMeans <- as.data.frame(means)
ggplot(SimMeans, aes(means)) + 
        scale_fill_brewer(palette = "Spectral") +
        geom_histogram(binwidth = .1, col="black", size=.1) +
        labs(title="Distribution of Means: Exponential Distributions", subtitle="Sample Mean ~ Red vs Theoretical Mean ~ Blue", x = "Mean", y = "Frequency") + 
        geom_vline(aes(xintercept = mean(means)),col='red',size=1) + 
        geom_vline(aes(xintercept = 5), col='blue', size = 1)
        
#create table of comparisons of the means
MeanTable <- matrix(c(theoreticalMean, simMean),ncol=1,byrow=TRUE)
colnames(MeanTable) <- "Mean"
rownames(MeanTable) <- c("Theoretical","Simulation")
MeanTable<- as.table(MeanTable)

#print table of the means comparison
print(MeanTable)

#Theoretical vs Sample Variance

#standard deviation from the simulations
simStandardDev <- sd(means)

#theoretical standard deviation
theoreticalStandardDev <- (1/lambda)/sqrt(n)

#variancestandard deviation from the simulations
simVariance <- simStandardDev^2

#theoretical variance
theoreticalVariance <- theoreticalStandardDev^2

#create table of comparisons
VarianceTable <- matrix(c(theoreticalStandardDev, theoreticalVariance, simStandardDev, simVariance),ncol=2,byrow=TRUE)
colnames(VarianceTable) <- c("Variance", "Standard Deviation")
rownames(VarianceTable) <- c("Theoretical","Simulation")
VarianceTable<- as.table(VarianceTable)

#print table of variance & standard deviation comparison
print(VarianceTable)

#distribution
ggplot(data = SimMeans, aes(x = means)) + 
        geom_histogram(binwidth=0.1, aes(y=..density..), fill = "grey")  +
        stat_function(fun=dnorm,args=list(mean=theoreticalMean, sd= theoreticalStandardDev), color = "blue", size = 1.0) +
        stat_function(fun=dnorm,args=list(mean= simMean, sd= simStandardDev), color = "red", size = 1.0) +
        labs(title="Distribution of Means: Exponential Distributions", subtitle="Sample Distribution ~ Red vs Theoretical Distribution ~ Blue", x="Means", y="Density")


#part 2 -  toothgrowth
# ToothGrowth - data frame with 60 observations on 3 variables
# - len	numeric	Tooth length
# - supp	factor	Supplement type (VC or OJ).
# ---VC: ascorbic acid tablet
# ---OJ: orange juice
# - dose	numeric	Dose in milligrams/day
library(datasets)
data("ToothGrowth")

str(ToothGrowth)
head(ToothGrowth)
summary(ToothGrowth)

#basic plot of the summary statistics
qplot(factor(dose), len, data = ToothGrowth, color = factor(dose), facets = supp ~ ., geom = c("boxplot", "jitter"))

# Explore effects of supplement type on tooth growth by 
# performing a two-sample t-test for the difference in tooth 
# length by supplement (without including the dosage). We assume a 
# confidence level of 95% and unequal variances.

ToothTTest <- t.test(len ~ supp, data = ToothGrowth, var.equal = FALSE, paired = FALSE)

#show table of two sample t-test results for tooth growth by supplement
tTestResults <- matrix(data.frame(ToothTTest$p.value, ToothTTest$conf.int[1], ToothTTest$conf.int[2], ToothTTest$estimate[1], ToothTTest$estimate[2]))
rownames(tTestResults) <- c("p-value", "Lower Conf Int", "Upper Conf Int", "OJ Mean", "VC Mean")
colnames(tTestResults) <- "Results"

print(tTestResults)



