#############################################################
# Data Analysis
# Computer Lab Session n° 8:
# Linear Regression (2/2)
#############################################################



#############################################
# Gradient Descent and Closed-Form Solution #
#############################################
# Create some values
xs <- seq(0,4,len=20) 

# Define the function we want to optimize
f <-  function(x) 
  {
  1.2 * (x-2)^2 + 3.2
  }

# Plot the function 
plot(xs, f(xs), type="l",xlab="x",ylab=expression(1.2(x-2)^2 +3.2))

# calculate the gradient df/dx
grad <- function(x)
  {
  1.2*2*(x-2)
  }

lines (c (2,2), c (3,8), col="red",lty=2)
text (2.1,7, "Closedform solution",col="red",pos=4)


# Gradient descent implementation
# Initialize the first guess for x-value
x <- 0.1 

# Store x-values for graphing purposes (initial)
xtrace <- x 

# Store y-values (function evaluated at x) for graphing purposes (initial)
ftrace <- f(x) 
alpha <- 0.6   # learning rate 'alpha'
for (step in 1:100) 
  {
  x <- x - alpha*grad(x)   # Gradient descent update
  xtrace <- c(xtrace,x)    # Update for graph
  ftrace <- c(ftrace,f(x)) # Update for graph
  }

lines ( xtrace , ftrace , type="b",col="blue")
text (0.5,6, "Gradient Descent",col="blue",pos= 4)

# Print final value of x
print(x) 

# Result: x converges to 2.0



####################
# Gradient Descent #
####################

# Load data and initialize values
data <- read.csv("http://www.statalgo.com/wp-content/uploads/2011/10/housing.csv")

pairs(data)
cor(data)
reghousing <- lm(price ~ area+bedrooms,  data = data)
reghousing <- lm(price ~ area,  data = data)
reghousing

