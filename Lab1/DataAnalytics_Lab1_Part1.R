
data_EPI <- read.csv(skip=0,"C:/Users/Naran/Downloads/2010EPI_data.csv", header=TRUE)

#### Exercise 1 ####

View(data_EPI)
attach(data_EPI)
EPI

tf <- is.na(EPI)
tf

E <- EPI[!tf]

summary(EPI)

fivenum(EPI, na.rm=TRUE)
stem(EPI)
#USED TO GENERATE A HISTOGRAM PLOT
hist(EPI)
hist(EPI,seq(30.,95.,1.0),prob=TRUE)
lines(density(EPI,na.rm=TRUE,bw=1))
rug(EPI)

plot(ecdf(EPI), do.points=FALSE, verticals=TRUE)

par(pty="s")
qqnorm(EPI); qqline(EPI)

## Repeating the Same For Air_H & Water_E

#### AIR_H Normal QQ_Plot ####
tf <- is.na(AIR_H)
tf

Air_Non_na <- EPI[!tf]

summary(Air_Non_na)

par(pty="s")
qqnorm(Air_Non_na); qqline(Air_Non_na)

#### Water_E Normal QQ_Plot #####
tf <- is.na(WATER_E)
tf

Water_non_na <- EPI[!tf]

summary(Water_non_na)

par(pty="s")
qqnorm(Water_non_na); qqline(Water_non_na)

# Intern Comparing EPI Against ENVHEALTH, ECOSYSTEM,
# DALY, AIR_H, WATER_H, AIR_E,WATER_E,
# BIODIVERSITY

par(mfrow=c(2,4))

#### EPI vs ENVHEALTH ####
qqplot(EPI,ENVHEALTH, main="EPI vs ENVHEALTH", xlab="EPI", ylab="ENVHEALTH")

#### EPI vs ECOSYSTEM ####
qqplot(EPI,ECOSYSTEM, main="EPI vs ECOSYSTEM", xlab="EPI", ylab="ECOSYSTEM")

#### EPI vs DALY ####
qqplot(EPI,DALY, main="EPI vs DALY", xlab="EPI", ylab="DALY")

#### EPI vs AIR_H ####
qqplot(EPI,AIR_H, main="EPI vs AIR_H", xlab="EPI", ylab="AIR_H")

#### EPI vs WATER_H ####
qqplot(EPI,WATER_H, main="EPI vs WATER_H", xlab="EPI", ylab="WATER_H")

#### EPI vs AIR_E ####
qqplot(EPI,AIR_E, main="EPI vs AIR_E", xlab="EPI", ylab="AIR_E")

#### EPI vs WATER_E ####
qqplot(EPI,WATER_E, main="EPI vs WATER_E", xlab="EPI", ylab="WATER_E")

#### EPI vs BIODIVERSITY ####
qqplot(EPI,BIODIVERSITY, main="EPI vs BIODIVERSITY", xlab="EPI", ylab="BIODIVERSITY")

#### Exercie 2 - Filtering ####
EPILand <- EPI[!Landlock]
Eland <- EPILand[!is.na(EPILand)]
par(mar=c(1,1,1,1))
hist(Eland)
hist(Eland,seq(30.,95.,1.0), prob=TRUE)
lines(density(EPILand,na.rm=TRUE,bw=1))
rug(EPI)

par(pty="s")
qqnorm(EPILand); qqline(EPILand)

#### How to filter values when the data type is a string/char ####
EPI_South_Asia <- EPI[EPI_regions=="South Asia"]
EPI_South_Asia
