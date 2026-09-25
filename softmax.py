import numpy as np
import math

class Softmax:
    def __init__(self, n_class: int, lr: float, epochs: int, reg_const: float):
        """Initialize a new classifier.
        Parameters:
            n_class: the number of classes
            lr: the learning rate
            epochs: the number of epochs to train for
            reg_const: the regularization constant
        """
        self.w = None  # TODO: change this
        self.lr = lr
        self.epochs = epochs
        self.reg_const = reg_const
        self.n_class = n_class
        self.arr = []
    def calc_gradient(self, X_train: np.ndarray, y_train: np.ndarray) -> np.ndarray:
        """Calculate gradient of the softmax loss.
        Inputs have dimension D, there are C classes, and we operate on
        mini-batches of N examples.
        Parameters:
            X_train: a numpy array of shape (N, D) containing a mini-batch
                of data
            y_train: a numpy array of shape (N,) containing training labels;
                y[i] = c means that X[i] has label c, where 0 <= c < C
        Returns:
            gradient with respect to weights w; an array of same shape as w
        """
        gradient = np.zeros((self.n_class, X_train.shape[1] + 1))
        for j in range(0,X_train.shape[0]):  
            sample = j              
            bias = np.expand_dims([1],axis=1)
            class_index = 0
            weights = (np.expand_dims(self.w[class_index,:],axis=1)) #arr (dimensions,class)
            X = np.concatenate((bias,np.expand_dims(X_train[sample,:],axis=1)),axis=0)
            Wc = weights.T@X
            for i in range(1,self.n_class):
                class_index = i
                weights = (np.expand_dims(self.w[class_index,:],axis=1))
                Wc = np.concatenate((Wc,weights.T@X),axis=0)     
            index = y_train[sample]
            local_gradient = np.zeros((self.n_class, X_train.shape[1] + 1))
            max = np.max(Wc)
            for i in range (0,self.n_class):
                Wc[i,0] = np.exp(Wc[i,0].sum()-max)
            total = 0
            for i in range (0,self.n_class):
                total = total + Wc[i,0].sum()
            for i in range (0,self.n_class):
                reg_gradient=0
                if(i==index):
                    local_gradient[i,:] = np.squeeze((((Wc[i,0]*X)/total)-X)+reg_gradient,axis=1)
                else:
                    local_gradient[i,:]= np.squeeze(((Wc[i,0]*X)/total)+reg_gradient,axis=1)
            gradient = gradient+local_gradient  
        gradient = gradient + 2 * self.reg_const * self.w
        gradient = gradient/X_train.shape[0]
        return(gradient)
    def train(self, X_train: np.ndarray, y_train: np.ndarray):     

        """Train the classifier.
        Hint: operate on mini-batches of data for SGD.
        Parameters:
            X_train: a numpy array of shape (N, D) containing training data;
                N examples with D dimensions
            y_train: a numpy array of shape (N,) containing training labels
        """
        total_loss=[]
        weights = np.random.default_rng()
        num_batches=10
        batch_start=0
        batch_end = 0
        self.w = np.expand_dims(weights.random(X_train.shape[1]+1), axis=1).T
        for i in range(1,self.n_class):
            self.w = np.concatenate((self.w, np.expand_dims(weights.random(X_train.shape[1]+1), axis=1).T), axis=0)        
        for epoch in range(0,self.epochs):
            epoch_loss = 0
            batch_size = math.ceil(X_train.shape[0] / num_batches)
            for j in range(0,num_batches):    
                batch_start= j*batch_size            
                if((batch_start+batch_size)>X_train.shape[0]):
                    batch_end=X_train.shape[0]
                else:
                    batch_end=batch_start+batch_size
                self.w=self.w-self.lr*self.calc_gradient(X_train[batch_start:batch_end,:],y_train[batch_start:batch_end])
            total_loss.append(epoch_loss)
        # TODO: implement me
        pass
        return
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
            weights = (np.expand_dims(self.w[class_index,:],axis=1))        
            X = np.concatenate((bias,np.expand_dims(X_test[sample,:],axis=1)),axis=0)
            Wc = weights.T@X
            for i in range(1,self.n_class):
                class_index = i
                weights = (np.expand_dims(self.w[class_index,:],axis=1))
                Wc = np.concatenate((Wc,weights.T@X),axis=0)
            predictions.append(np.argmax(Wc))
        return(np.array(predictions))