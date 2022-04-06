#define TEST1(%1)    (%1 + 10)
#define TEST2        10 + 10

start

#assert TEST1(10) == 20

end