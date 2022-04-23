#define TEST1(%1)    (%1 + 10)
#define TEST2        10 + 10

start

#if TEST1(9) == 20
	foo
	#if 1==1
		foo1
	#endif
#elseif TEST2 == 20
	bar
#else
	bazz
#endif

end

#if defined DEBUG || defined _DEBUG
	return true;
#else
	return false;
#endif