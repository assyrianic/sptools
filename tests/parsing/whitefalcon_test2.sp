#define Foo(%0)         \
	PrintToServer(#%0)

void Bad() {
	PrintToServer(":)");
}

public void OnPluginStart()
{
	Foo(Bad());
	Foo(Evil());
}