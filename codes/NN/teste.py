{
 "cells": [
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": [
    "# FMTC2019 - Neural Networks - Variable Annuities\n",
    "\n",
    "#### paper : https://arxiv.org/pdf/1606.07831"
   ]
  },
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": [
    "### Importing Packages "
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 50,
   "metadata": {},
   "outputs": [],
   "source": [
    "import pandas as pd\n",
    "import numpy as np\n",
    "import matplotlib.pyplot as plt\n",
    "import keras \n",
    "import tensorflow\n",
    "import theano\n",
    "import sys\n",
    "import pydot\n",
    "\n",
    "from sklearn.model_selection import train_test_split,cross_val_score,GridSearchCV\n",
    "from sklearn.preprocessing import StandardScaler,LabelEncoder,OneHotEncoder\n",
    "from sklearn.metrics import confusion_matrix\n",
    "from keras.models import Sequential, Model\n",
    "from keras.layers import Dense,Flatten, dot, multiply\n",
    "from keras.wrappers.scikit_learn import KerasRegressor\n",
    "from keras.layers import Dropout\n",
    "from keras.layers import Input, Concatenate, Conv2D, Flatten, Dense, Lambda, concatenate\n",
    "# from tensorflow.keras.layer import Input, Lambda, Dense, concatenate\n",
    "# from tensorflow.keras.models import Model\n",
    "from keras.utils import plot_model\n",
    "import itertools"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 10,
   "metadata": {},
   "outputs": [],
   "source": [
    "import pickle as pk"
   ]
  },
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": [
    "### Importing Data"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 3,
   "metadata": {},
   "outputs": [],
   "source": [
    "# We will use two databases selected by K-means.\n",
    "# Dimension of sample1.csv = 340 observations (rep_contracts).\n",
    "# Dimension of sample2.csv = 680 observations (train_contracts).\n",
    "# sample2 will be our data that will be splitted up into train, test and validation data.\n",
    "# sample1 will be our representative contracts.\n",
    "\n",
    "# Importing:\n",
    "rep_contracts = pd.read_csv(\"sample1.csv\")\n",
    "train_contracts = pd.read_csv(\"sample2.csv\")\n",
    "\n",
    "# Cleaning\n",
    "rep_contracts = rep_contracts.iloc[:,2:]\n",
    "train_contracts = train_contracts.iloc[:,2:]"
   ]
  },
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": [
    "### Data "
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 4,
   "metadata": {},
   "outputs": [],
   "source": [
    "# Function that cleans Data:\n",
    "\n",
    "def cleaningData(x):\n",
    "    \n",
    "    # Building Account Value (AV) variable \n",
    "    x['AV'] = x[['FundValue' + str(i) for i in np.arange(1, 11, 1)]].sum(axis=1)\n",
    "\n",
    "    \n",
    "    # Filtering only importants variables\n",
    "    x = x[['gender','productType','ttm','age', 'AV','gbAmt','withdrawal','wbWithdrawalRate','fmv']]\n",
    "    \n",
    "    # Selecting only two categories\n",
    "\n",
    "    # TRASH:\n",
    "    x.loc[:, 'productType'] = [\"trash\" if value in ['ABRP','ABRU','ABSU','IBRP','IBRU','IBSU','MBRP','MBRU','MBSU','DBAB','DBIB','DBMB','DBWB'] else value for value in list(x.productType)]\n",
    "\n",
    "    # GMDB:\n",
    "    x.loc[:, 'productType'] = [\"GMDB\" if value in ['DBRP','DBRU','DBSU'] else value for value in list(x.productType)]\n",
    "\n",
    "    # GMWB:\n",
    "    x.loc[:, 'productType'] = [\"GMWB\" if value in ['WBRP','WBRU','WBSU'] else value for value in list(x.productType)]\n",
    "    \n",
    "    # Building variables:\n",
    "    x.withdrawal = x.withdrawal/x.AV\n",
    "    x.gbAmt = x.gbAmt/x.AV\n",
    "    \n",
    "    # Categories into numbers:\n",
    "    auxiliar_data = pd.DataFrame(x[::])\n",
    "    auxiliar_data['male'] = [1 if value == 'M' else 0 for value in auxiliar_data.gender]\n",
    "    auxiliar_data['GMDB'] = [1 if value == 'GMDB' else 0 for value in auxiliar_data.productType]\n",
    "    auxiliar_data['GMWB'] = [1 if value == 'GMWB' else 0 for value in auxiliar_data.productType]\n",
    "\n",
    "    # Dropping old category variables:\n",
    "    auxiliar_data = auxiliar_data.drop(['gender', 'productType'], axis=1)\n",
    "\n",
    "    # Dropping Acount Value == 0:\n",
    "    auxiliar_data = auxiliar_data[auxiliar_data.AV != 0]\n",
    "    \n",
    "    y = pd.DataFrame(auxiliar_data['fmv'])\n",
    "    x = auxiliar_data.drop(['fmv'], axis = 1)\n",
    "    \n",
    "    return [x,y]"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 5,
   "metadata": {},
   "outputs": [
    {
     "name": "stderr",
     "output_type": "stream",
     "text": [
      "/home/novais/anaconda3/lib/python3.7/site-packages/pandas/core/indexing.py:543: SettingWithCopyWarning: \n",
      "A value is trying to be set on a copy of a slice from a DataFrame.\n",
      "Try using .loc[row_indexer,col_indexer] = value instead\n",
      "\n",
      "See the caveats in the documentation: http://pandas.pydata.org/pandas-docs/stable/indexing.html#indexing-view-versus-copy\n",
      "  self.obj[item] = s\n",
      "/home/novais/anaconda3/lib/python3.7/site-packages/pandas/core/generic.py:4405: SettingWithCopyWarning: \n",
      "A value is trying to be set on a copy of a slice from a DataFrame.\n",
      "Try using .loc[row_indexer,col_indexer] = value instead\n",
      "\n",
      "See the caveats in the documentation: http://pandas.pydata.org/pandas-docs/stable/indexing.html#indexing-view-versus-copy\n",
      "  self[name] = value\n"
     ]
    }
   ],
   "source": [
    "# Cleaning Data:\n",
    "rep = cleaningData(rep_contracts)\n",
    "train = cleaningData(train_contracts)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 6,
   "metadata": {},
   "outputs": [],
   "source": [
    "# Building continuous transformation funtion:\n",
    "# def contTransform(representative, trainning):\n",
    "#     Rt = np.array([max(max(rep[0].loc[:, value]),max(train[0].loc[:, value])) for value in rep[0].columns[0:6]])\n",
    "    \n",
    "#     fmenos = trainning - representative\n",
    "#     fmenos = np.array([max(value, 0) for value in fmenos])\n",
    "#     fmenos = fmenos/Rt\n",
    "\n",
    "#     fmais = representative - trainning\n",
    "#     fmais = np.array([max(value, 0) for value in fmais])\n",
    "#     fmais = fmais/Rt\n",
    "\n",
    "#     return list(fmenos) + list(fmais)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 6,
   "metadata": {},
   "outputs": [],
   "source": [
    "# Building function in page 8.\n",
    "# There are two types of vaiables: Categorical and Continuous.\n",
    "# This function will be the inputs of our Neural Networks.\n",
    "\n",
    "def distance(t, r):\n",
    "    \n",
    "    # Building continuous transformation funtion:\n",
    "    def contTransform(representative, trainning):\n",
    "        Rt = np.array([max(max(rep[0].loc[:, value]),max(train[0].loc[:, value])) for value in rep[0].columns[0:6]])\n",
    "\n",
    "        fmenos = trainning - representative\n",
    "        fmenos = np.array([max(value, 0) for value in fmenos])\n",
    "        fmenos = fmenos/Rt\n",
    "\n",
    "        fmais = representative - trainning\n",
    "        fmais = np.array([max(value, 0) for value in fmais])\n",
    "        fmais = fmais/Rt\n",
    "\n",
    "        return list(fmenos) + list(fmais)\n",
    "    \n",
    "    # Continuous Variables Transformation\n",
    "    F_MenosMais = contTransform(representative = r[['ttm','age', 'AV', \"gbAmt\", 'withdrawal', 'wbWithdrawalRate']],\n",
    "                 trainning = t[['ttm','age', 'AV', \"gbAmt\", 'withdrawal', 'wbWithdrawalRate']])\n",
    "    \n",
    "    # Categorical Variables Transformation\n",
    "    Fc = t[['male', 'GMDB', 'GMWB']] == r[['male', 'GMDB', 'GMWB']]\n",
    "    Fc = list(Fc.apply(lambda x:1 if x==False else 0))\n",
    "    \n",
    "    # Appending Results\n",
    "    f = Fc + F_MenosMais\n",
    "    return(f)\n",
    "    "
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 12,
   "metadata": {},
   "outputs": [],
   "source": [
    "#Loading trainning\n",
    "file = open('measures2.pk', 'rb')\n",
    "train = pk.load(file)\n",
    "\n",
    "# file = open('measuresValidation.pk', 'rb')\n",
    "# test = pk.load(file)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 35,
   "metadata": {},
   "outputs": [],
   "source": [
    "x_train, x_test, y_train, y_test = train_test_split(train[0],train[1],test_size=0.20,random_state=0)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 36,
   "metadata": {},
   "outputs": [],
   "source": [
    "Y_train = np.array(y_train)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 37,
   "metadata": {},
   "outputs": [
    {
     "data": {
      "text/plain": [
       "(540, 5070)"
      ]
     },
     "execution_count": 37,
     "metadata": {},
     "output_type": "execute_result"
    }
   ],
   "source": [
    "train.shape"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 23,
   "metadata": {},
   "outputs": [
    {
     "name": "stderr",
     "output_type": "stream",
     "text": [
      "WARNING: Logging before flag parsing goes to stderr.\n",
      "W0729 20:26:39.141212 140483554953024 deprecation_wrapper.py:119] From /home/novais/anaconda3/lib/python3.7/site-packages/keras/backend/tensorflow_backend.py:74: The name tf.get_default_graph is deprecated. Please use tf.compat.v1.get_default_graph instead.\n",
      "\n",
      "W0729 20:26:39.387470 140483554953024 deprecation_wrapper.py:119] From /home/novais/anaconda3/lib/python3.7/site-packages/keras/backend/tensorflow_backend.py:517: The name tf.placeholder is deprecated. Please use tf.compat.v1.placeholder instead.\n",
      "\n"
     ]
    }
   ],
   "source": [
    "inp = Input(shape=(5070,))\n",
    "aux1 = Lambda(lambda x: x[:,0:15])(inp)\n",
    "for i in np.arange(15, 5070, 15):\n",
    "    aux2 = Lambda(lambda x: x[:,(0+i):(15+i)])(inp)\n",
    "    aux1 = concatenate([aux1, aux2])"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 24,
   "metadata": {},
   "outputs": [
    {
     "data": {
      "text/plain": [
       "<tf.Tensor 'concatenate_337/concat:0' shape=(?, 5070) dtype=float32>"
      ]
     },
     "execution_count": 24,
     "metadata": {},
     "output_type": "execute_result"
    }
   ],
   "source": [
    "aux1"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 155,
   "metadata": {},
   "outputs": [],
   "source": [
    "inp = Input(shape=(4,))\n",
    "aux1 = Lambda(lambda x: x[:,0:2])(inp)\n",
    "aux2 = Lambda(lambda x: x[:,2:4])(inp)\n",
    "teste = Concatenate([aux1, aux2])"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 156,
   "metadata": {},
   "outputs": [
    {
     "data": {
      "text/plain": [
       "<keras.layers.merge.Concatenate at 0x7f43383d2f28>"
      ]
     },
     "execution_count": 156,
     "metadata": {},
     "output_type": "execute_result"
    }
   ],
   "source": [
    "teste"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 65,
   "metadata": {},
   "outputs": [],
   "source": [
    "# inp = Input(shape=(2,))\n",
    "# inp2 = Lambda(lambda x: x[:,1:2])(inp)   # get the second neuron \n",
    "\n",
    "# h1_out = Dense(1, activation='sigmoid')(inp2)  # only connected to the second neuron\n",
    "# h2_out = Dense(1, activation='relu')(inp)  # connected to both neurons\n",
    "# h_out = Concatenate([h1_out, h2_out])\n"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 209,
   "metadata": {},
   "outputs": [],
   "source": [
    "inp = Input(shape=(5070,))\n",
    "#aux1 = Lambda(lambda x: x[:,0:15])(inp)\n",
    "tt = []\n",
    "# for i in np.arange(15, 5070, 15):\n",
    "for i in np.arange(0, 5070, 15):\n",
    "    tt.append(Lambda(lambda x: x[:,(0+i):(15+i)])(inp))\n",
    "#     aux2 = Lambda(lambda x: x[:,(0+i):(15+i)])(inp)\n",
    "#     aux1 = concatenate([aux1, aux2])\n",
    "aux1 = concatenate(tt)    "
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 78,
   "metadata": {},
   "outputs": [
    {
     "data": {
      "text/plain": [
       "(338,)"
      ]
     },
     "execution_count": 78,
     "metadata": {},
     "output_type": "execute_result"
    }
   ],
   "source": [
    "train2 = np.array(list(range(0,338)))\n",
    "train2.shape"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 83,
   "metadata": {},
   "outputs": [],
   "source": [
    "inp = Input(shape=(5070,))\n",
    "aux1 = Lambda(lambda x: x[:,0:15])(inp)\n",
    "tt = []\n",
    "# for i in np.arange(15, 5070, 15):\n",
    "for i in np.arange(0, 5070, 15):\n",
    "    tt.append(Lambda(lambda x: x[:,(0+i):(15+i)])(inp))\n",
    "    aux2 = Lambda(lambda x: x[:,(0+i):(15+i)])(inp)\n",
    "    aux1 = concatenate([aux1, aux2])    \n",
    "\n",
    "first = Dense(338, activation='exponential')(aux1)\n",
    "second = Dense(338, activation='softmax')(first)\n",
    "inp2 = Input(shape=(338,))\n",
    "dotted = dot([second, inp2],axes=1)\n",
    "third = Dense(1, activation='linear',trainable=False)(dotted)\n",
    "\n",
    "model = Model(inputs = [inp,inp2], outputs = third)\n",
    "\n",
    "# print(model.summary())\n",
    "\n",
    "model.compile(optimizer = 'sgd',loss='mean_squared_error', metrics=['mse'])"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 84,
   "metadata": {},
   "outputs": [
    {
     "ename": "ValueError",
     "evalue": "Error when checking input: expected input_46 to have shape (338,) but got array with shape (1,)",
     "output_type": "error",
     "traceback": [
      "\u001b[0;31m---------------------------------------------------------------------------\u001b[0m",
      "\u001b[0;31mValueError\u001b[0m                                Traceback (most recent call last)",
      "\u001b[0;32m<ipython-input-84-1185c7e69463>\u001b[0m in \u001b[0;36m<module>\u001b[0;34m\u001b[0m\n\u001b[0;32m----> 1\u001b[0;31m \u001b[0mteste\u001b[0m \u001b[0;34m=\u001b[0m \u001b[0mmodel\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mfit\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mx\u001b[0m \u001b[0;34m=\u001b[0m \u001b[0;34m[\u001b[0m\u001b[0mtrain\u001b[0m\u001b[0;34m,\u001b[0m\u001b[0mtrain2\u001b[0m\u001b[0;34m]\u001b[0m\u001b[0;34m,\u001b[0m \u001b[0my\u001b[0m \u001b[0;34m=\u001b[0m \u001b[0mY_train\u001b[0m\u001b[0;34m,\u001b[0m \u001b[0mepochs\u001b[0m\u001b[0;34m=\u001b[0m\u001b[0;36m100\u001b[0m\u001b[0;34m,\u001b[0m \u001b[0mbatch_size\u001b[0m\u001b[0;34m=\u001b[0m\u001b[0;36m50\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[0m",
      "\u001b[0;32m~/anaconda3/lib/python3.7/site-packages/keras/engine/training.py\u001b[0m in \u001b[0;36mfit\u001b[0;34m(self, x, y, batch_size, epochs, verbose, callbacks, validation_split, validation_data, shuffle, class_weight, sample_weight, initial_epoch, steps_per_epoch, validation_steps, **kwargs)\u001b[0m\n\u001b[1;32m    950\u001b[0m             \u001b[0msample_weight\u001b[0m\u001b[0;34m=\u001b[0m\u001b[0msample_weight\u001b[0m\u001b[0;34m,\u001b[0m\u001b[0;34m\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m    951\u001b[0m             \u001b[0mclass_weight\u001b[0m\u001b[0;34m=\u001b[0m\u001b[0mclass_weight\u001b[0m\u001b[0;34m,\u001b[0m\u001b[0;34m\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[0;32m--> 952\u001b[0;31m             batch_size=batch_size)\n\u001b[0m\u001b[1;32m    953\u001b[0m         \u001b[0;31m# Prepare validation data.\u001b[0m\u001b[0;34m\u001b[0m\u001b[0;34m\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m    954\u001b[0m         \u001b[0mdo_validation\u001b[0m \u001b[0;34m=\u001b[0m \u001b[0;32mFalse\u001b[0m\u001b[0;34m\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n",
      "\u001b[0;32m~/anaconda3/lib/python3.7/site-packages/keras/engine/training.py\u001b[0m in \u001b[0;36m_standardize_user_data\u001b[0;34m(self, x, y, sample_weight, class_weight, check_array_lengths, batch_size)\u001b[0m\n\u001b[1;32m    749\u001b[0m             \u001b[0mfeed_input_shapes\u001b[0m\u001b[0;34m,\u001b[0m\u001b[0;34m\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m    750\u001b[0m             \u001b[0mcheck_batch_axis\u001b[0m\u001b[0;34m=\u001b[0m\u001b[0;32mFalse\u001b[0m\u001b[0;34m,\u001b[0m  \u001b[0;31m# Don't enforce the batch size.\u001b[0m\u001b[0;34m\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[0;32m--> 751\u001b[0;31m             exception_prefix='input')\n\u001b[0m\u001b[1;32m    752\u001b[0m \u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m    753\u001b[0m         \u001b[0;32mif\u001b[0m \u001b[0my\u001b[0m \u001b[0;32mis\u001b[0m \u001b[0;32mnot\u001b[0m \u001b[0;32mNone\u001b[0m\u001b[0;34m:\u001b[0m\u001b[0;34m\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n",
      "\u001b[0;32m~/anaconda3/lib/python3.7/site-packages/keras/engine/training_utils.py\u001b[0m in \u001b[0;36mstandardize_input_data\u001b[0;34m(data, names, shapes, check_batch_axis, exception_prefix)\u001b[0m\n\u001b[1;32m    136\u001b[0m                             \u001b[0;34m': expected '\u001b[0m \u001b[0;34m+\u001b[0m \u001b[0mnames\u001b[0m\u001b[0;34m[\u001b[0m\u001b[0mi\u001b[0m\u001b[0;34m]\u001b[0m \u001b[0;34m+\u001b[0m \u001b[0;34m' to have shape '\u001b[0m \u001b[0;34m+\u001b[0m\u001b[0;34m\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m    137\u001b[0m                             \u001b[0mstr\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mshape\u001b[0m\u001b[0;34m)\u001b[0m \u001b[0;34m+\u001b[0m \u001b[0;34m' but got array with shape '\u001b[0m \u001b[0;34m+\u001b[0m\u001b[0;34m\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[0;32m--> 138\u001b[0;31m                             str(data_shape))\n\u001b[0m\u001b[1;32m    139\u001b[0m     \u001b[0;32mreturn\u001b[0m \u001b[0mdata\u001b[0m\u001b[0;34m\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m    140\u001b[0m \u001b[0;34m\u001b[0m\u001b[0m\n",
      "\u001b[0;31mValueError\u001b[0m: Error when checking input: expected input_46 to have shape (338,) but got array with shape (1,)"
     ]
    }
   ],
   "source": [
    "teste = model.fit(x = [train,train2], y = Y_train, epochs=100, batch_size=50)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 2,
   "metadata": {},
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "[NbConvertApp] WARNING | pattern 'teste.py' matched no files\r\n",
      "This application is used to convert notebook files (*.ipynb) to various other\r\n",
      "formats.\r\n",
      "\r\n",
      "WARNING: THE COMMANDLINE INTERFACE MAY CHANGE IN FUTURE RELEASES.\r\n",
      "\r\n",
      "Options\r\n",
      "-------\r\n",
      "\r\n",
      "Arguments that take values are actually convenience aliases to full\r\n",
      "Configurables, whose aliases are listed on the help line. For more information\r\n",
      "on full configurables, see '--help-all'.\r\n",
      "\r\n",
      "--debug\r\n",
      "    set log level to logging.DEBUG (maximize logging output)\r\n",
      "--generate-config\r\n",
      "    generate default config file\r\n",
      "-y\r\n",
      "    Answer yes to any questions instead of prompting.\r\n",
      "--execute\r\n",
      "    Execute the notebook prior to export.\r\n",
      "--allow-errors\r\n",
      "    Continue notebook execution even if one of the cells throws an error and include the error message in the cell output (the default behaviour is to abort conversion). This flag is only relevant if '--execute' was specified, too.\r\n",
      "--stdin\r\n",
      "    read a single notebook file from stdin. Write the resulting notebook with default basename 'notebook.*'\r\n",
      "--stdout\r\n",
      "    Write notebook output to stdout instead of files.\r\n",
      "--inplace\r\n",
      "    Run nbconvert in place, overwriting the existing notebook (only \r\n",
      "    relevant when converting to notebook format)\r\n",
      "--clear-output\r\n",
      "    Clear output of current file and save in place, \r\n",
      "    overwriting the existing notebook.\r\n",
      "--no-prompt\r\n",
      "    Exclude input and output prompts from converted document.\r\n",
      "--no-input\r\n",
      "    Exclude input cells and output prompts from converted document. \r\n",
      "    This mode is ideal for generating code-free reports.\r\n",
      "--log-level=<Enum> (Application.log_level)\r\n",
      "    Default: 30\r\n",
      "    Choices: (0, 10, 20, 30, 40, 50, 'DEBUG', 'INFO', 'WARN', 'ERROR', 'CRITICAL')\r\n",
      "    Set the log level by value or name.\r\n",
      "--config=<Unicode> (JupyterApp.config_file)\r\n",
      "    Default: ''\r\n",
      "    Full path of a config file.\r\n",
      "--to=<Unicode> (NbConvertApp.export_format)\r\n",
      "    Default: 'html'\r\n",
      "    The export format to be used, either one of the built-in formats, or a\r\n",
      "    dotted object name that represents the import path for an `Exporter` class\r\n",
      "--template=<Unicode> (TemplateExporter.template_file)\r\n",
      "    Default: ''\r\n",
      "    Name of the template file to use\r\n",
      "--writer=<DottedObjectName> (NbConvertApp.writer_class)\r\n",
      "    Default: 'FilesWriter'\r\n",
      "    Writer class used to write the  results of the conversion\r\n",
      "--post=<DottedOrNone> (NbConvertApp.postprocessor_class)\r\n",
      "    Default: ''\r\n",
      "    PostProcessor class used to write the results of the conversion\r\n",
      "--output=<Unicode> (NbConvertApp.output_base)\r\n",
      "    Default: ''\r\n",
      "    overwrite base name use for output files. can only be used when converting\r\n",
      "    one notebook at a time.\r\n",
      "--output-dir=<Unicode> (FilesWriter.build_directory)\r\n",
      "    Default: ''\r\n",
      "    Directory to write output(s) to. Defaults to output to the directory of each\r\n",
      "    notebook. To recover previous default behaviour (outputting to the current\r\n",
      "    working directory) use . as the flag value.\r\n",
      "--reveal-prefix=<Unicode> (SlidesExporter.reveal_url_prefix)\r\n",
      "    Default: ''\r\n",
      "    The URL prefix for reveal.js (version 3.x). This defaults to the reveal CDN,\r\n",
      "    but can be any url pointing to a copy  of reveal.js.\r\n",
      "    For speaker notes to work, this must be a relative path to a local  copy of\r\n",
      "    reveal.js: e.g., \"reveal.js\".\r\n",
      "    If a relative path is given, it must be a subdirectory of the current\r\n",
      "    directory (from which the server is run).\r\n",
      "    See the usage documentation\r\n",
      "    (https://nbconvert.readthedocs.io/en/latest/usage.html#reveal-js-html-\r\n",
      "    slideshow) for more details.\r\n",
      "--nbformat=<Enum> (NotebookExporter.nbformat_version)\r\n",
      "    Default: 4\r\n",
      "    Choices: [1, 2, 3, 4]\r\n",
      "    The nbformat version to write. Use this to downgrade notebooks.\r\n",
      "\r\n",
      "To see all available configurables, use `--help-all`\r\n",
      "\r\n",
      "Examples\r\n",
      "--------\r\n",
      "\r\n",
      "    The simplest way to use nbconvert is\r\n",
      "    \r\n",
      "    > jupyter nbconvert mynotebook.ipynb\r\n",
      "    \r\n",
      "    which will convert mynotebook.ipynb to the default format (probably HTML).\r\n",
      "    \r\n",
      "    You can specify the export format with `--to`.\r\n",
      "    Options include ['asciidoc', 'custom', 'html', 'latex', 'markdown', 'notebook', 'pdf', 'python', 'rst', 'script', 'slides']\r\n",
      "    \r\n",
      "    > jupyter nbconvert --to latex mynotebook.ipynb\r\n",
      "    \r\n",
      "    Both HTML and LaTeX support multiple output templates. LaTeX includes\r\n",
      "    'base', 'article' and 'report'.  HTML includes 'basic' and 'full'. You\r\n",
      "    can specify the flavor of the format used.\r\n",
      "    \r\n",
      "    > jupyter nbconvert --to html --template basic mynotebook.ipynb\r\n",
      "    \r\n",
      "    You can also pipe the output to stdout, rather than a file\r\n",
      "    \r\n",
      "    > jupyter nbconvert mynotebook.ipynb --stdout\r\n",
      "    \r\n",
      "    PDF is generated via latex\r\n",
      "    \r\n",
      "    > jupyter nbconvert mynotebook.ipynb --to pdf\r\n",
      "    \r\n",
      "    You can get (and serve) a Reveal.js-powered slideshow\r\n",
      "    \r\n",
      "    > jupyter nbconvert myslides.ipynb --to slides --post serve\r\n",
      "    \r\n",
      "    Multiple notebooks can be given at the command line in a couple of \r\n",
      "    different ways:\r\n",
      "    \r\n",
      "    > jupyter nbconvert notebook*.ipynb\r\n",
      "    > jupyter nbconvert notebook1.ipynb notebook2.ipynb\r\n",
      "    \r\n",
      "    or you can specify the notebooks list in a config file, containing::\r\n",
      "    \r\n",
      "        c.NbConvertApp.notebooks = [\"my_notebook.ipynb\"]\r\n",
      "    \r\n",
      "    > jupyter nbconvert --config mycfg.py\r\n",
      "\r\n"
     ]
    }
   ],
   "source": [
    "!jupyter nbconvert --to script 'teste.py'"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {},
   "outputs": [],
   "source": []
  }
 ],
 "metadata": {
  "kernelspec": {
   "display_name": "Python 3",
   "language": "python",
   "name": "python3"
  },
  "language_info": {
   "codemirror_mode": {
    "name": "ipython",
    "version": 3
   },
   "file_extension": ".py",
   "mimetype": "text/x-python",
   "name": "python",
   "nbconvert_exporter": "python",
   "pygments_lexer": "ipython3",
   "version": "3.7.3"
  }
 },
 "nbformat": 4,
 "nbformat_minor": 2
}
