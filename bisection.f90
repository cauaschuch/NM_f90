program bissection
implicit none
real, external::f 
real::a,b,c,thr,i
print*,'a and b'
read*,a,b
thr = 0.000000001
i = 0
do while (abs(a-b) > thr .and. i < 100)
c=(a+b)/2
if (f(c)==0) then
    print*, 'the root is' , c
end if
if (f(a)*f(c) .gt. 0 ) then
    a=c
else
    b=c
end if
i = i+1
end do
print*, c
end
real function f(x)
real::x
f = 3*x-1
end