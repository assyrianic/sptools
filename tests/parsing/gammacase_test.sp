#define A(%12) %12

public void OnPluginStart() {
    PrintToServer("%i", A(6));
}