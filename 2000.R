se <- "SE.csv"
snp <- "SNP.csv"

# Read data frames
g <- read.table(se, header = TRUE, sep= ",")[,c("Date","Adj.Close")]
s <- read.table(snp, header = TRUE, sep= ",")[,c("Date","Adj.Close")]

# Change the coumn names
names(g)[2] <- "g.price"
names(s)[2] <- "s.price"# Join on date

gs <- merge(g, s, by = "Date")
head(gs)

# Convert dates to real 'date objects
gs[,c("Date")] <- as.Date(gs[,c("Date")], "%Y-%m-%d")

# Sort by date (recent on top)
gs <- gs[order(gs$Date, decreasing = TRUE),]

# Convert to stationary data
gs[-nrow(gs),-1] <- (gs[-nrow(gs),-1] / gs[-1,-1]  )- 1

# deleting last row
gs <- gs[-nrow(gs),]

# Rename return cols
names(gs)[2:3] <- c("g.ret","s.ret")

# p(y_i) = 1 / (1+e^(-(A+Bx_i)))
# p(y) = Prob. of o.
# x = R s of S0o.

comb <- data.frame(gs[-nrow(gs),],gs[-1,-1])

# Excluded first and last rows
#         Date         g.ret         s.ret       g.ret.1       s.ret.1
#21 2017-09-08 -0.0100967059 -0.0014888507  0.0087733631 -0.0001784360
#20 2017-09-07  0.0087733631 -0.0001784360 -0.0006893360  0.0031287266
#19 2017-09-06 -0.0006893360  0.0031287266 -0.0094843010 -0.0075508068
#18 2017-09-05 -0.0094843010 -0.0075508068 -0.0021185206  0.0019825409
#17 2017-09-01 -0.0021185206  0.0019825409  0.0104994889  0.0057209760
#16 2017-08-31  0.0104994889  0.0057209760  0.0089874298  0.0046151489
#15 2017-08-30  0.0089874298  0.0046151489  0.0081854872  0.0008428219
#14 2017-08-29  0.0081854872  0.0008428219 -0.0022710336  0.0004870719
#13 2017-08-28 -0.0022710336  0.0004870719 -0.0058505708  0.0016728693
#12 2017-08-25 -0.0058505708  0.0016728693 -0.0061704110 -0.0020744619
#11 2017-08-24 -0.0061704110 -0.0020744619  0.0024981323 -0.0034535928
#10 2017-08-23  0.0024981323 -0.0034535928  0.0198862082  0.0099407800
#9  2017-08-22  0.0198862082  0.0099407800 -0.0044033624  0.0011626509
#8  2017-08-21 -0.0044033624  0.0011626509 -0.0003402896 -0.0018353673
#7  2017-08-18 -0.0003402896 -0.0018353673 -0.0172391922 -0.0154369519
#6  2017-08-17 -0.0172391922 -0.0154369519  0.0051398269  0.0014201029
#5  2017-08-16  0.0051398269  0.0014201029 -0.0004877280 -0.0004988081
#4  2017-08-15 -0.0004877280 -0.0004988081  0.0090551820  0.0100437547
#3  2017-08-14  0.0090551820  0.0100437547  0.0078810735  0.0012755698

names(comb)[4:5] <- c("g.lag","s.lag")

comb$g.UP = comb$g.ret >= 0

# glm(y ~ x1 + x2, family=binomial())
fit <- glm(comb$g.UP ~ comb$g.lag + comb$s.lag, family=binomial())
summary(fit)

# Call:
# glm(formula = comb$g.UP ~ comb$g.lag + comb$s.lag, family = binomial())
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -1.5438  -0.9944  -0.6111   1.2253   1.4235  
# 
# Coefficients:
#     A,B,C
#                |
#                v
#             Estimate Std. Error z value Pr(>|z|)
# (Intercept)  -0.4951     0.5235  -0.946    0.344 (and here)
# comb$g.lag   34.4058    96.1100   0.358    0.720 (and here)
# comb$s.lag  100.8827   162.0528   0.623    0.534 (not stat. sig)
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 25.864  on 18  degrees of freedom
# Residual deviance: 23.659  on 16  degrees of freedom
# AIC: 29.659
# 
# Number of Fisher Scoring iterations: 4

# Check
results <- data.frame(comb$g.UP, fitted(fit) >= 0.5)
names(results) <- c("Actual","Predicted")
results$Correct = results$Actual == results$Predicted
length(results$Correct[results$Correct == TRUE]) / length(results$Correct)

