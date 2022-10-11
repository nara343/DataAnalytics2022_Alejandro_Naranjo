require(kknn)
data(ionosphere)

#Training Set
ionosphere.learn <- ionosphere[1:200,]
#Testing Set
ionosphere.valid <- ionosphere[-c(1:200),]

#Fitting the model 
fit.kknn <- kknn(class ~ ., ionosphere.learn, ionosphere.valid)
#Testing the model fit and seeing the performance
table(ionosphere.valid$class, fit.kknn$fit)

# Adusting the model and finding a better fit by adding a kernel increasing kmax
# And Increasing the distance 

(fit.train1 <- train.kknn(class ~ ., ionosphere.learn, kmax = 15, 
	kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 1))
table(predict(fit.train1, ionosphere.valid), ionosphere.valid$class)
(fit.train2 <- train.kknn(class ~ ., ionosphere.learn, kmax = 15, 
	kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 2))
table(predict(fit.train2, ionosphere.valid), ionosphere.valid$class)
