require(rpart)
library(rpart)
#Loading in the swiss data
data("swiss")

Swiss_rpart <- rpart(Fertility ~ Agriculture + Education + Catholic, data = swiss)

plot(Swiss_rpart) # try some different plot options
text(Swiss_rpart) # try some different text options

help(rpart)
