# Load raw data

data<-read.csv(file.choose())
# # Load up caTools package to use for data split
library(caTools)

# Split the data
split<-sample.split(data,SplitRatio = 0.8)

# Training set is a dataset used to train a model.
training<-subset(data,split=="TRUE")

# Testing dataset is used only for testing the final solution in order -
# to confirm the actual predictive power.
testing<-subset(data,split=="FALSE")

# Create a appropriate model using training dataset
model<-glm(Outcome~.,training,family = "binomial")
summary(model)

# Predict the values for the testing dataset
ans<-predict(model,testing,type = "response")

# We categorize the values according to threshold which is 0.5
# Create Confusion Matrix for the testing dataset 
tab<-table(Actual=testing$Outcome, Predict=ans>0.5)

# To calculate classification error (try to reduce clasification error by changing threshold value i.e 0.5)
class.error<-1-(sum(diag(tab))/sum(tab))
class.error
