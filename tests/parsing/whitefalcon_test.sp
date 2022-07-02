#define Bar \
	PrintToServer("Hello, World!")

#define Foo(%0) \
	Bar; \
	PrintToServer(#%0)


public void OnPluginStart()
{
	Foo(1);
	PrintToServer("Hello, World!");
}
