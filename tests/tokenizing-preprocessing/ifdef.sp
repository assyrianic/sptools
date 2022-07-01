#define TEST1(%1)    (%1 + 10)
#define TEST2        10 + 10


start

///*
#if TEST1(9) == 20
	foo1
#elseif TEST1(10) == 20
	bar1
#else
	baz1
#endif
//*/

///*
#if TEST1(9+1) == 20
	foo2
	#if 1==1
		foo2A
	#endif
#elif TEST2 == 20
	bar2
#else
	baz2
#endif
//*/

#define _DEBUG
///*
#if defined DEBUG || defined _DEBUG
	return true;
#else
	return false;
#endif
//*/

end