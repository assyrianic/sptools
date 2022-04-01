#define TEST1(%1)    (%1 + 10)
#define TEST2    10 + 10

#if __SPTOOLS__
	// code1
#endif

#if defined __SPTOOLS__
	// code1
#endif

#if TEST1(10)
	// code1
#endif

#if TEST1(10) < 9 && TEST1(1) > 0
	// code1
#endif