#define A(%1) %12

public void OnPluginStart() {
    PrintToServer("%i", A(6));
}