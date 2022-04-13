{
	return;
	return foo.bar(true);
	while (1) {}
	do{} while(1);
	do return; while(1);
	if( foo ) {}
	if( bar ) {} else {}
	if( foo ){} else if( bar ){} else if( baz ){} else {}
	
	if (n)
		return;
	else if(n)
		return;
	else
		return;
	
	if( foo ) {
		if( bar ) {
			if( baz ) {
			} else {
			}
		} else {
		}
	} else {
	}
	
	switch( 1 ) {
		case 0: {
			if (n) return;
			else if(n) return;
			else return;
			return foo;
		}
		case 1,2,3: return bart;
		default: return baz;
	}
}