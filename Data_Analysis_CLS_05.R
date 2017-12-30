#############################################################
# Data Analysis
# Computer Lab Session n° 5:
# Linear Algebra (2/2)
#############################################################

###################################
# 1. Eigenvalues and Eigenvectors #
###################################

# Initial 3x3 Hilbert matrix
H3 <- matrix(c(1, 1/2, 1/3, 1/2, 1/3, 1/4, 1/3, 1/4, 1/5), nrow=3)
H3

eigen(H3)

# Exercise
X <- matrix(c(1, 2, 3, 1, 4, 9), ncol=2)

# Calculate the matrix H = X (X^T X) ^{-1} X^T
H <- X %*% solve(t(X) %*% X) %*% t(X)

# Eigenvalues and eigenvectors of H
E <- eigen(H)
E

# Trace of H
trace <- function(data)sum(diag(data))
trace(H)

# Sum of the eigenvalues
sum(E$values)

# Determinant of the matrix H
det(H)

# Product of the eigenvalues
prod(E$values)

# Product of H and X
H %*% X

# I <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow=3)
# more simple : 
I <- diag(3)
I

H %*% (I - H)


######################
# 2. Advanced Topics #
######################

# The Singular Value Decomposition of a Matrix 

H3.svd <- svd(H3)
H3.svd

H3.svd$u %*% diag(H3.svd$d) %*% t(H3.svd$v)

H3.svd$v %*% diag(1/H3.svd$d) %*% t(H3.svd$u)


# R Function apply()

apply(H3, 1, sum)


#############
# Exercises #
#############

# Exercise 1
P <- matrix(c(0.1, 0.4, 0.3, 0.2,
              0.2, 0.1, 0.4, 0.3,
              0.3, 0.2, 0.1, 0.4,
              0.4, 0.3, 0.2, 0.1), nrow=4)

apply(P, 1, sum)

P2 <- P %*% P
P2

P3 <- P %*% P %*% P
P3

P5 <- P2 %*% P3
P5

P10 <- P5 %*% P5
P10

P10 %*% P10


# Exercise 2
# (a) Total number of each type of policy
totals <- matrix(c(245921, 7304620, 34390.48, 6864693), ncol=1)

# annual income from each policy (in euros)
an_inc_pol <- c(10, 30, 50, 100)

# expected number of claims (per year)
exp_nb_claim <- c(0.1, 0.15, 0.03, 0.5)

# expected size of the claims (in euros)
exp_size_claim <- c(50, 180, 1500, 250)

# expected total claim amount
exp_claim_amount <- exp_nb_claim * exp_size_claim 
exp_claim_amount

# system of linear equations
lin_eq <- rbind(rep(1,4), an_inc_pol, exp_nb_claim, exp_claim_amount)
lin_eq

# solving the system of linear equations with the totals
nb_pol <- solve(lin_eq, totals)
nb_pol


# (b) Income and claimsize
income <- an_inc_pol * nb_pol
income

claimsize <-nb_pol * exp_claim_amount
claimsize
