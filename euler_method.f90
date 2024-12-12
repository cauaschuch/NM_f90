program euler
implicit none
real,external::f
real::x,h,xg
real::y
integer::n,i
print*, 'initial condition x0'
read*, x
print*, 'initial condition y0'
read*, y
print*, 'h value'
read*,h
print*, 'the xg'
read*, xg
n=int((xg-x)/(h))
do i=1,n
y = y+h*f(x,y)
x=x+h
end do

print*, 'the value of (x,y) is', x,y


end

real function f(x,y)
f = x+2*y
end