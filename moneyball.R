setwd("C:/Users/DELL/Desktop/ISSMTECHASSIGNMENTS/Kaggle/Baseball/")
baseball=read.csv("baseball.csv")
str(baseball)
moneyball=subset(baseball,Year<2002)
moneyball$RD=moneyball$RS-moneyball$RA

# Find number of wins required to qualify playoffs
with(moneyball,plot(W,col=factor(Playoffs)))
abline(h=95,col="blue",lty=2,lwd=2)
# Thus, atleast 95 wins are required to safely reach playoff


with(moneyball,plot(RD,W,col=factor(Playoffs)))
WinsReg=lm(W~RD,data=moneyball)
summary(WinsReg)
# Find RD required to win atleast 95 games
# Eqn is W=80.881375+0.105766(RD)
# solve 80.881375+0.105766(RD)>=95
# RD >= 133.4893

# To predict the RD, one needs to predict RS and RA

# We Predict the important variables to predict the Run scored (RS)
RunsReg=lm(RS~OBP+SLG+BA,moneyball)
summary(RunsReg)

# We see that coefficient of BA is negative which is counterintuitive
# as normally one would assume that as BA increases RS would also increase
# This anomaly is due to multi-collinearity between OBP and BA
# Diff is that BA excludes walks, whereas OBP includes both hits and walks
cor(moneyball$OBP,moneyball$BA)

# Thus we remove BA and only consider OBP
RunsScoredReg=lm(RS~OBP+SLG,moneyball)
summary(RunsScoredReg)

# We see that the coefficient of OBP is more than SLG, as units of both OBP and SLG are same
# we can conclude that OBP is more important than SLG

## we create similar model to create a model for RA
# This is based on OOBP (Opponenet on base percentatge) and OSLG of our opponenet

RunsAllowedReg=lm(RA~OOBP+OSLG,moneyball)
summary(RunsAllowedReg)





