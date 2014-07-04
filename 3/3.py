num = 600851475143
mxf = 1
x = 2

while (x * x <= num) :
	if (num % x == 0) :
		mxf = x
		while (num % x == 0) :
			num /= x
	x += 1

if (num != 1) :
	mxf = num

print mxf