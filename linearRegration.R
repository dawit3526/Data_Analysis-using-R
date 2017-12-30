load(whirlwind.csv)
planets <- readWorksheet(loadWorkbook("c:/Users/Dawit Tewelde/Desktop/whirlwind.cvs"),sheet=1)
load("C:/Users/Dawit Tewelde/Desktop/whirlwind.cvsv")
val = load(whirlwind.CSV,verbose = FALSE)
hp2 <??? read . c s v ( "whir " )

attach(whirlwind)
plot(whirlwin)
install.packages()
library(XLConnect)
plot(whirlwind$x,whirlwind$y)
xs = seq(0.4,len=20)
f = function(x)
{
  1.2*(x-2)^2 + 3.2
}
plot(xs,f(xs),type = "l",xlab = "x",ylab = expression(1.2(x-2)^2+3.2))
X = matrix(c(1,5,1,3),nrow=2)
X
Y = matrix(c(9,6))
Y
 E=eigen(X)
solve(H3)
H3
t(H3)
result =solve((t(X)%*%X))%*%t(X)%*%X
result
t(X)%*%
trace <- function(data)sum(diag(data))
trace(X)
det(X)
prod(E$values)
E
prod(E$vectors)
result%*%X
I = matrix(c(1,0,0,1),nrow=2)

result%*%(I-result)
v_svd=svd(X)
v_svd$u%*%diag(v_svd$d)%*%t(v_svd$v)
p= matrix(c(0.1,0.4,0.3,0.2,0.2,0.1,0.4,0.3,0.3,0.2,0.1,0.4,0.4,0.3,0.2,0.1),nrow = 4)
p
apply(X,1,sum)
P2 <- p %*% p
P2

P3 <- p %*% p %*% p
P3

P5 <- p2 %*% p3
P5

P10 <- p5 %*% p5
P10

P10 %*% P10

totals <- matrix(c(245921, 7304620, 34390.48, 6864693), ncol=1)
totals
# annual income from each policy (in euros)
an_inc_pol <- c(10, 30, 50, 100)
an_inc_pol
# expected number of claims (per year)
exp_nb_claim <- c(0.1, 0.15, 0.03, 0.5)

# expected size of the claims (in euros)
exp_size_claim <- c(50, 180, 1500, 250)
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
