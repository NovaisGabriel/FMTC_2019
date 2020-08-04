import numpy as np

class NeuralNetwork():
    
    def __init__(self):
        # seeding for random number generation
        np.random.seed(1)
        
        #converting weights to a 13 by 1 matrix with values from -1 to 1 and mean of 0
        self.synaptic_weights = 2 * np.random.random((11, 1)) - 1
        
        # BRENDA
        self.validation_mse = None
        self.iterations = None

    def sigmoid(self, x):
        #applying the sigmoid function
        return 1 / (1 + np.exp(-x))

    def sigmoid_derivative(self, x):
        #computing derivative to the Sigmoid function
        return x * (1 - x)
      
    # BRENDA
    def train(self, training_inputs, training_outputs, validation_inputs, validation_outputs, l, w):
        
        #training the model to make accurate predictions while adjusting weights continually
        mses = list()
        while len(mses) <= w or mses[len(mses)-w-1]>min(mses[len(mses)-w-1:len(mses)]):  
            for iteration in range(l):
                #siphon the training data via  the neuron
                output = self.think(training_inputs)

                #computing error rate for back-propagation
                error = training_outputs - output

                #performing weight adjustments
                adjustments = np.dot(training_inputs.T, error * self.sigmoid_derivative(output))

                self.synaptic_weights += adjustments
            
            self.validation_mse = self.validate(validation_inputs, validation_outputs)
            mses.append(self.validation_mse)
            self.iterations = l*len(mses)
        #self.mse = mses[len(mses)]
        

    def think(self, inputs):
        #passing the inputs via the neuron to get output   
        #converting values to floats
        
        inputs = inputs.astype(float)
        output = self.sigmoid(np.dot(inputs, self.synaptic_weights))
        return output
    
    # BRENDA
    def validate(self, validation_inputs, validation_outputs):
        # Prediction
        output = self.think(validation_inputs)
        # computing error rate for back-propagation
        error = validation_outputs - output
        # computing MSE
        mse = np.mean(np.square(error))
        return mse






