import numpy as np
import math

class SVM:
    def __init__(self, n_class: int, lr: float, epochs: int, reg_const: float):
        """Initialize a new classifier.

        Parameters:
            n_class: the number of classes
            lr: the learning rate
            epochs: the number of epochs to train for
            reg_const: the regularization constant
        """
        self.n_class = n_class
        self.lr = lr
        self.epochs = epochs
        self.reg_const = reg_const
        self.w = None

    def calc_gradient(self, X_batch: np.ndarray, y_batch: np.ndarray) -> np.ndarray:
        """Calculate gradient of the svm hinge loss using vectorization.
        
        Parameters:
            X_batch: a numpy array of shape (N, D+1) containing a mini-batch
                of data (bias trick already applied)
            y_batch: a numpy array of shape (N,) containing training labels
            
        Returns:
            The gradient with respect to weights w; an array of shape (C, D+1)
        """
        N = X_batch.shape[0]

        # 1. Calculate the class scores (N, C)
        # X_batch is (N, D+1), self.w is (C, D+1). Transpose w to match dimensions.
        scores = X_batch.dot(self.w.T)

        # 2. Extract the scores of the correct classes for each sample (N, 1)
        correct_class_scores = scores[np.arange(N), y_batch].reshape(-1, 1)

        # 3. Calculate the hinge loss margins: max(0, score_j - score_correct + 1)
        margins = np.maximum(0, scores - correct_class_scores + 1.0)
        
        # 4. We don't count the true class in the margin sum, so zero it out
        margins[np.arange(N), y_batch] = 0

        # 5. Create a binary mask of where the margin is greater than 0
        binary_mask = (margins > 0).astype(float)

        # For the correct class, the gradient subtracts the sum of the positive margins
        row_sum = np.sum(binary_mask, axis=1)
        binary_mask[np.arange(N), y_batch] = -row_sum

        # 6. Compute the final gradient matrix (C, D+1)
        dW = binary_mask.T.dot(X_batch) / N

        # 7. Add the regularization gradient
        dW += (self.reg_const / N) * self.w

        return dW

    def train(self, X_train: np.ndarray, y_train: np.ndarray):
        """Train the classifier using Mini-Batch Gradient Descent."""
        N, D = X_train.shape

        # The Bias Trick: Append a column of 1s to X_train so we don't have to 
        # compute the bias separately. X_train becomes shape (N, D+1)
        X_train_bias = np.hstack([X_train, np.ones((N, 1))])

        # Initialize weights efficiently with small random numbers
        if self.w is None:
            self.w = np.random.randn(self.n_class, D + 1) * 0.001

        num_batches = 10
        batch_size = math.ceil(N / num_batches)

        for epoch in range(self.epochs):
            for j in range(num_batches):
                batch_start = j * batch_size
                batch_end = min(batch_start + batch_size, N)

                X_batch = X_train_bias[batch_start:batch_end]
                y_batch = y_train[batch_start:batch_end]

                if X_batch.shape[0] == 0:
                    continue

                # Calculate gradient and update weights
                grad = self.calc_gradient(X_batch, y_batch)
                self.w -= self.lr * grad

    def predict(self, X_test: np.ndarray) -> np.ndarray:
        """Use the trained weights to predict labels for test data points."""
        
        # Apply the bias trick to the test data
        N = X_test.shape[0]
        X_test_bias = np.hstack([X_test, np.ones((N, 1))])

        # Calculate all scores at once (N, C)
        scores = X_test_bias.dot(self.w.T)

        # The prediction is simply the class with the highest score
        return np.argmax(scores, axis=1)