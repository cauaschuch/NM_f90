program newton
implicit none
real,external::f,df
integer::i
real::a,thr
thr = 1d-10
i = 0
a = -1
do while (abs(f(a)) > thr .and. i < 100)
a = a - f(a)/df(a)
i = i+1
end do
print*, (a),i


end
real function f(x)
real::x
f = x**2 +4 - x**3-x**4
end function f 
real function df(x)
real::x
df = 2*x -3*x**2 -4*x**3
end function df