data_EPI <- read.csv(skip=0,"C:/Users/Naran/Downloads/2010EPI_data.csv", header=TRUE)



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
