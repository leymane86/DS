"""Perceptron model."""

import numpy as np

from operator import add, sub
class Perceptron:
    def __init__(self, n_class: int, lr: float, epochs: int, decay:bool):
        """Initialize a new classifier.

        Parameters:
            n_class: the number of classes
            lr: the learning rate
            epochs: the number of epochs to train for
        """
        self.w = None  # TODO: change this
        self.lr = lr
        self.epochs = epochs
        self.n_class = n_class
        self.arr = []
        self.decay = decay

    def train(self, X_train: np.ndarray, y_train: np.ndarray):
        """Train the classifier.

        Use the perceptron update rule as introduced in the Lecture.

        Parameters:
            X_train: a number array of shape (N, D) containing training data;
                N examples with D dimensions
            y_train: a numpy array of shape (N,) containing training labels
        """

        # TODO: implement me
        total_loss=[]
        weights = np.random.default_rng()
        arr = np.expand_dims(weights.random(X_train.shape[1]+1), axis=1).T
        for i in range(1,self.n_class):
            arr = np.concatenate((arr, np.expand_dims(weights.random(X_train.shape[1]+1), axis=1).T), axis=0)         
        for epoch in range(0,self.epochs):
            if(self.decay==True):
                self.lr = self.lr*.98
            epoch_loss = 0
            for j in range(0,X_train.shape[0]):                
                class_index = 0 
                sample = j
                bias = np.expand_dims([1],axis=1)
                weights = (np.expand_dims(arr[class_index,:],axis=1)) #arr (dimensions,class)
                X = np.concatenate((bias,np.expand_dims(X_train[sample,:],axis=1)),axis=0)
                Wc = weights.T@X
                for i in range(1,self.n_class):
                    class_index = i
                    weights = (np.expand_dims(arr[class_index,:],axis=1))
                    Wc = np.concatenate((Wc,weights.T@X),axis=0)
                index = y_train[sample]
                updated = False
                for i in range (0,self.n_class):
                    if(Wc[(index),0]<Wc[i,0]):
                        epoch_loss = epoch_loss + Wc[i,0].sum() - Wc[(index),0].sum() #accounts for bias at the beginning
                        class_index = (index)
                        if(updated == False):
                            arr[class_index,:]=arr[class_index,:]+self.lr*np.concatenate((np.squeeze(bias,axis=1),(X_train[sample,:])),axis=0)
                        arr[i,:]=arr[i,:]-self.lr*np.concatenate((np.squeeze(bias,axis=1),(X_train[sample,:])),axis=0)
            total_loss.append(epoch_loss)
        self.arr=arr
        pass

    def predict(self, X_test: np.ndarray) -> np.ndarray:
        """Use the trained weights to predict labels for test data points.

        Parameters:
            X_test: a numpy array of shape (N, D) containing testing data;
                N examples with D dimensions

        Returns:
            predicted labels for the data in X_test; a 1-dimensional array of
                length N, where each element is an integer giving the predicted
                class.
        """
        predictions = []
        for j in range(0,X_test.shape[0]):               
            sample = j
            bias = np.expand_dims([1],axis=1)
            class_index = 0
          
            weights = (np.expand_dims(self.arr[class_index,:],axis=1))
            X = np.concatenate((bias,np.expand_dims(X_test[sample,:],axis=1)),axis=0)
            Wc = weights.T@X
            for i in range(1,self.n_class):
                class_index = i
                weights = (np.expand_dims(self.arr[class_index,:],axis=1))
                Wc = np.concatenate((Wc,weights.T@X),axis=0)
            predictions.append(np.argmax(Wc))
        return(np.array(predictions))
