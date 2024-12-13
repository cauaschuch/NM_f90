program rk2
implicit none
real::x,y,h,xg,k1,k2
real,external::f
integer::i,n
x = 0
y = 3
xg = 1
h=.0001
n = int((xg-x)/h)

do i=1,n
k1 = f(x,y)
k2 = f(x+h,y+k1*h)
x = x+h
y = y +h*0.5*(k1+k2)
end do

print*,x,y,n
end
real function f(x,y)
real::x,y
f = 7*exp(-0.3*x)-1.2*y
end

