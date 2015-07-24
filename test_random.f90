program test_random

implicit none

integer :: i, j, negative, positive
integer :: binx(5001), biny(5001), binz(5001)
double precision :: width,x, y, z 

width = 0.01
negative = 0
positive = 0
open(123,file='random.txt')

DO i = 1, 50000

read(123,*) x, y, z

IF(x<0)then
negative = negative + 1

else
positive = positive + 1
cycle
end if

!write(*,*) x, y, z
print *, i
!stop
x = dabs(x*1d2)
y = dabs(y*1d2)
z = dabs(z*1d2)

binx(int(x/width)+1) = binx(int(x/width)+1) + 1
biny(int(y/width)+1) = biny(int(y/width)+1) + 1
binz(int(z/width)+1) = binz(int(z/width)+1) + 1

END DO

open(124,file='plot_random.txt')

DO i = 1, 5000

write(124,'(4i5)') i, binx(i)!, biny(i), binz(i)

END DO

print *, 'negative', negative
print *, 'positive', positive
END PROGRAM test_random
