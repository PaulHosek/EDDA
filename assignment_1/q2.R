#a.
# plot data
library(ggplot2)
library(dplyr)
library(ggpubr)

data <- read.table("data/cholesterol.txt", header = TRUE, sep = " ");


data_long <- data.frame(
  Time = rep(c("Before", "After 8 Weeks"), each = nrow(data)),
  Value = c(data$Before, data$After8weeks)
);

jpeg(file= "plots/hist1.jpeg");
ggplot(data_long, aes(x = Value, fill = Time)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 10) +
  labs(title = "Histogram of Before and After 8 Weeks", x = "Values", y = "Frequency") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("Before", "After 8 Weeks"));
dev.off();


# normality
jpeg(file= "plots/qq1.jpeg");
qqplot_data_before <- qqplot(qnorm(ppoints(nrow(data))), data$Before, main = "QQ Plot for Before", pch = 16, col = "red", ylab = "");
par(new=T);
qqplot_data_after8weeks <- qqplot(qnorm(ppoints(nrow(data))), data$After8weeks, pch = 17, col = "green", ylab="Data");
legend("bottomright", legend = c("Before", "After 8 Weeks"), pch = c(16, 17), col = c("red", "green"));
dev.off();

# correlation
jpeg(file= "plots/correlation.jpeg");
ggscatter(data, x = colnames(data)[1], y = colnames(data)[2],
          add = "reg.line",cor.coef = TRUE,, conf.int = TRUE, cor.method = "pearson",
          xlab = "Before", ylab = "After 8 Weeks");
dev.off();

####################################
#b

# apply repeated measures t-test
res_rp1 <- t.test(data$Before, data$After8weeks, paired = TRUE, alternative = "two.sided");
# print(res_rp1);

# apply non-parametric repeated measure t-test
res_rp2 <- wilcox.test(data$Before, data$After8weeks, paired = TRUE, alternative = "two.sided");
# print(res_rp2);

# yes there is a statistically significant effect of the diet
# however, this must not imply that the effect has practical significance

# yes the permutation test is applicable
# can express any test statistic including repeated measures
# is non-parameteric

####################################
#c)

# random resample from this and estimate max each time
nsamples <- 1000;



theta <- replicate(nsamples, max(sample(data$After8weeks,size=18, replace = TRUE)));
# theta <- replicate(nsamples, 3+2*mean(sample(data$After8weeks,size=10, replace = TRUE)));
cat("RES",quantile(theta, c(0.025, 0.975)),"\n");


# improve
# i can improve it with more samples

####################################
#d)
# bootstrap test on max
nsamples <- 1000;
thetas <- seq(3, 12, by = .01);

# Loop over theta values and perform bootstrap test
p_vals <- numeric(length(thetas));
for (i in 1:length(thetas)) {
  res <- replicate(nsamples,max(sample(data$After8weeks,size=10, replace = TRUE)));
  # res <- replicate(nsamples,3+ 2*mean(sample(data$After8weeks,size=10, replace = TRUE)));
  p_vals[i] <- sum(res >= thetas[i]) / nsamples;
}

# Plot p-values vs theta values
jpeg(file = "plots/test.jpeg");
plot(thetas, p_vals, type = "l", xlab = "theta", ylab = "p-value");
abline(h = 0.05, col = "red");
legend("topright", legend = c("p-value", "0.05"),
       col = c("black", "red"), lty = c(1, 1));
dev.off();

# yes we can use the KS tes instead.
# KS-test

thetas <- seq(3, 12, by = .01);

# Loop over theta values and perform bootstrap test
ks_res <- numeric();
p_vals <- numeric(length(thetas));
for (i in 1:length(thetas)) {
  theta <- thetas[i];
  cur_unif <- runif(10000,3,thetas[i]);
  p_vals[i] <- ks.test(data$After8weeks, cur_unif)['p.value'];
  # res <- replicate(nsamples,3+ 2*mean(sample(data$After8weeks,size=10, replace = TRUE)));
}

# Plot p-values vs theta values
jpeg(file = "plots/ks_resd.jpeg");
plot(thetas, p_vals, type = "l", xlab = "theta", ylab = "p-value");
abline(h = 0.05, col = "red");
legend("topright", legend = c("p-value", "0.05"),
       col = c("black", "red"), lty = c(1, 1));
dev.off();

####################################
#e)


# paired samples t-test
#TODO: test assumptions
wx_e <-wilcox.test(data$Before, data$After8weeks, paired = TRUE, alternative = "less");


# second part

# Calculate the fraction of cholesterol levels less than 4.5
prop <- mean(data$After8weeks < 4.5)

# Perform the one-sided binomial test
# H0 = true porportion of cholesterol <4.5 is equal to 25%
binom_e <- binom.test(sum(data$After8weeks < 4.5), nrow(data), p = 0.25, alternative = "less")$p.value
print(binom_e)
#











