program integral
implicit none

real::a,b,x,h,fODD,fEVEN,simpson
real,external::f
integer::n,i
fODD = 0.
fEVEN = 0.
b=9
a=0
n = 20
h = (b-a)/n
x = 0.
do i=1,n-1
x = x+h
!result = mod(i,2)
if (mod(i,2)==0) then
    fEVEN = fEVEN + f(x)
else
    fODD = fODD +f(x)
end if
end do
simpson = (h/3.)*(f(a)+f(b)+4*fODD+2*fEVEN)
print*,simpson
end



real function f(x)
real::x
f = x**2



end



