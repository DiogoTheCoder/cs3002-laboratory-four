library(neuralnet)

# OR gate input data
trainin = rbind(c(1,1), c(1,-1), c(-1,1), c(-1,-1));

# OR gate output data
trainout = rbind(1, 1, 1, 0);

# Combined OR gate data
ORdat = cbind(trainout, trainin)

# fit neural network with no hidden layers
set.seed(2)
NN = neuralnet(ORdat[,1]~., ORdat[,-1], hidden = 0 , threshold = 0.001,
               stepmax = 1e+05, linear.output = FALSE)

# visualise the NN
plot(NN)

print(NN$weights)

testin = rbind(c(1,1))
predict_testNN = compute(NN, testin)

print(predict_testNN$net.result)

predict_out = as.numeric(predict_testNN$net.result>0.5)
print(predict_out)

# set up the input sequence
testin = rbind(c(1,1),c(1,-1),c(-1,1), c(-1,-1))
predict_testNN = compute(NN, testin)

print(predict_testNN$neurons)
print(predict_testNN$net.result)

predict_out = as.numeric(predict_testNN$net.result>0.5)
print(predict_out)

# ASSESSED EXERCISE
# XOR gate input data
train_in = rbind(c(1,1), c(1,-1), c(-1,1), c(-1,-1));

# XOR gate output data
train_out = rbind(0, 1, 1, 0);

# Combined OR gate data
XOR_data = cbind(train_out, train_in)

# fit neural network with no hidden layers
set.seed(2)
NN = neuralnet(XOR_data[,1]~., XOR_data[,-1], hidden = 0, threshold = 0.001,
               stepmax = 1e+05, linear.output = FALSE)

# visualise the NN
plot(NN)

predict_testNN = compute(NN, testin)

print(predict_testNN$neurons)
print(predict_testNN$net.result)

predict_out = as.numeric(predict_testNN$net.result > 0.50)
print(predict_out)

# Add Hidden Layer, to try to solve XOR problem
NN = neuralnet(XOR_data[,1]~., XOR_data[,-1], hidden = c(3,3) , threshold =
                 0.001, stepmax = 1e+05, linear.output = FALSE)

# visualise the NN
plot(NN)

predict_testNN = compute(NN, testin)

print(predict_testNN$neurons)
print(predict_testNN$net.result)

predict_out = as.numeric(predict_testNN$net.result > 0.50)
print(predict_out)

setwd("/Users/diogocosta/Work/computer-science/cs3002/cs3002-laboratory-four")
wine_data = read.csv('winedata2.csv', sep=",")
wine_class = wine_data[,1]
wine_abv = wine_data[,2:3]

# set up a training set
wine_class_train = wine_class[1:65]
wine_values_train = wine_abv[1:65,]

# test set
wine_class_test = wine_class[66:130]
wine_values_test = wine_abv[66:130,]

wine_class_train = replace(wine_class_train, wine_class_train == 1, 0)
wine_class_train = replace(wine_class_train, wine_class_train >= 2, 1)

wine_class_test = replace(wine_class_test, wine_class_test == 1, 0)
wine_class_test = replace(wine_class_test, wine_class_test >= 2, 1)

# fit neural network with no hidden layers
set.seed(2)
NN = neuralnet(wine_class_train~., wine_values_train, hidden = 0, threshold = 0.001,
               stepmax = 1e+05, linear.output = FALSE)

# visualise the NN
plot(NN)

print(NN$weights)
predict_testNN = compute(NN, wine_values_test)
print(predict_testNN);

print(predict_testNN$neurons)
print(predict_testNN$net.result)

predict_out = as.numeric(predict_testNN$net.result > 0.50)
print(predict_out)
accuracy = sum(predict_out == wine_class_test) / length(wine_class_test)
print(accuracy)
