program monte_carlo
implicit none
real,external:: f
real::a,b,x,p,integral_r
integer::n,i
a=0
b=3
n=100000
integral_r = 0.
do i =1,n
p = rand()
x = a-p*a +b*p
integral_r = integral_r+f(x)
end do
print*,(b-a)*integral_r/n

end



real function f(x)
real::x
f = x**2
end