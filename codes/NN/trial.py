import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import keras 
import tensorflow
import theano
import sys
import pydot
import pickle
import itertools

from sklearn.model_selection import train_test_split,cross_val_score,GridSearchCV
from sklearn.preprocessing import StandardScaler,LabelEncoder,OneHotEncoder
from sklearn.metrics import confusion_matrix
from keras.models import Sequential, Model
from keras.layers import Dense,Flatten, dot, multiply
from keras.wrappers.scikit_learn import KerasRegressor
from keras.layers import Dropout
from keras.layers import Input, Concatenate, Conv2D, Flatten, Dense, Lambda, concatenate
from keras.utils import plot_model

X = np.array([[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]]*340)
Z = np.array([range(0,340)]*340)
Y = np.array([[5]]*340)
X.shape
Y.shape
Z.shape
x_train, x_test, y_train, y_test = train_test_split(X,Y,test_size=0.20,random_state=0)
Y_train = np.array(y_train)
# First Input Layer:
inp = Input(shape=(340,))
# First Layer:
first = Dense(340, activation='exponential')(inp)
# Second Layer:
second = Dense(340, activation='softmax')(first)
# Second Input Layer:
inp2 = Input(shape=(340,))
# Second input and second layer together:
dotted = dot([second, inp2], axes=1)
# Output Layer:
third = Dense(1, activation='linear')(dotted)
# Creating Model:
model = Model(inputs = [inp,inp2], outputs = third)
# Visualising Neural Network:
print(model.summary())
# Compiling model:
model.compile(optimizer = 'sgd',loss='mean_squared_error', metrics=['mse'])
# Fit
fit = model.fit(x = [X,Z], y = Y_train, epochs=100, batch_size=50)