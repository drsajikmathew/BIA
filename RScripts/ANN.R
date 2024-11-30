#ANN Model for predicting Stock Price of Infosys from NASDAQ

#Invoke all necessary packages - TTR, neuralnet and ggplot2
library(TTR)
library(neuralnet)
library(ggplot2)

#Read input data from csv file
infy=read.table("Infy.csv",header=T,sep=",")
infy1 = infy[order(nrow(infy):1),] #Flipping the data in ascending order
infy1$SerialNo=1:nrow(infy) #Adding serial number to flipped data
########################### Creating Input Variables ########################################

#Calculate lagged prices for 1 and 2 day lags
length=nrow(infy1)
infy1$Price = c(infy1$Close)
infy1$Price_1 = c(0,head(infy1$Close,-1))
infy1$Price_2 = c(0,0,head(infy1$Close,-2))

#C(): concatenate to create  a vector of numbers

#Calculate simple moving average for 5, 10 and 50 days using TTR
infy1$SMA_5=SMA(infy1$Close,n=5)     
infy1$SMA_10=SMA(infy1$Close,n=10)
infy1$SMA_50=SMA(infy1$Close,n=50)

#Calculate Relative Strength Indicator for 10 days (RSI) using TTR
infy1$RSI=RSI(infy1$Close,n=10)

#Calculate  momentum function for most recent 5 days using TTR
infy1$Momentum=(infy1$Close - c(0,0,0,0,0,head(infy1$Close,-5)))
infy1$Momentum[1:5]=0

#Replace NA's with 0's (when MA s created it leads to NAs
infy1$SMA_5[is.na(infy1$SMA_5)]<-0
infy1$SMA_10[is.na(infy1$SMA_10)]<-0
infy1$SMA_50[is.na(infy1$SMA_50)]<-0
infy1$RSI[is.na(infy1$RSI)]<-0

#Creating target data

close=data.frame(infy1$Close)
infy1$tr=c(close[2:length,],0)

####################################### Write variables to file and read again ##############################

write.csv(infy1,"Infyfinal_model.csv")
infynew=read.table("Infyfinal_model.csv",header=TRUE,sep=",")[-c(1:50,length),];
#reads columns 10:18
infy2=infynew[,10:18]

#Normalization function to normalize all values between 0-1 scale
#Feature scaling 	X' = \frac{X - X_{min}}{X_{max}-X_{min}}, 0-1

norm.fun = function(x){
  (x - min(x))/(max(x) - min(x)) 
}

#Applying norm function from coulmn 4 to coumn 21 in both train and validation data
infy3=apply(infy2,2,norm.fun)

train.length=round(length*0.70) 
#70% of observations is training data. Computing its length


train.data=infy3[1:train.length,] #Storing Traindata
val.data=infy3[-c(1:train.length),-9] #Remaining 30% is Validation data 



################################## MODEL - NEURAL NETWORKS ############################





##################### Building the ANN Model #################################################

#70% is training data and 30% is validation data
#8-3-1 ANN model with 8 input variables, 1 hidden layer with 3 neurons and one output
#output variable - Closing Price
#8 input variables - Price, Price Lag 1, Price Lag 2, SMA_5 lag 1, SMA_10 lag 1, SMA_50 lag 1, RSI lag 1, Momentum lag 1
#Algorithm - rprop+ - resilient backpropagation with weight backtracking

infy831<- neuralnet(tr ~ Price + Price_1 + Price_2 + SMA_5 + SMA_10 + SMA_50
                    + RSI + Momentum,
                    
                    train.data, 
                    hidden = c(3), threshold = 0.1,
                    stepmax = 10000, rep = 10,algorithm = "rprop+",
                    err.fct = "sse", 
                    linear.output = TRUE, exclude = NULL,
                    constant.weights = NULL, likelihood = FALSE)
#Plot the model
plot(infy831, rep = "best");

pred= compute(infy831, val.data,rep=1)
result = data.frame(actual = val.data[,1], prediction = pred$net.result)
#Plotting the actual and predicted values of the validation data
ggplot(result, aes(1:nrow(val.data))) +  # basic graphical object
  geom_line(aes(y=result$actual), colour="green") +  # first layer
  geom_line(aes(y=result$prediction), colour="red")  # second layer

#Computing root mean square error for the model
RMSE = (mean((result$actual-result$prediction)^2))^0.5
RMSE
#Arguments
#learning rate is set by the algorithm
#formula a symbolic description of the model to be fitted.
#data a data frame containing the variables specified in formula.
#hidden a vector of integers specifying the number of hidden neurons (vertices) in each layer.
#threshold a numeric value specifying the threshold for the partial derivatives of the #error
#function as stopping criteria.
#stepmax the maximum steps for the training of the neural network. Reaching this maximum leads to a stop of the neural network’s training process.
