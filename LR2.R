x=2
xs = seq(0.4,len=20)
f = function(x)
{
  1.2*(x-2)^2+3.2
}
plot(xs, f(xs),type = "l",xlab ="x",ylab = expression(1.2(x-2)^2+3.2))
grad = function(x)
{
  1.2*2*(x-2)
}
lines (c(2,2),c(3,8), col="red",lty=2)
text(2.1,7,"closedform soution", col = "red",pos=4)
x = 2
xtrace = x
ftrace = f(x)
alpha = 0.6
for(step in 1:100)
{
  x= x-alpha*grad(x)
  xtrace = c(xtrace,x)
  ftrace = c(ftrace,f(x))
}
lines(xtrace, ftrace , type = "b", col="blue")
text(0.5,6,"Gradient Decent",col="blue",pos=4)
print(x)

H = matrix(c(-3,20,2,1,3,-2,-3,10,4),nrow=3)
solve(H)
det(H)
determinant(H)
H
