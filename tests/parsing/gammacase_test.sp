#define A(%1) %12
#define ASSERT2(%1) #%1


#define __SNAME "plugin"
#define ASSERT(%1) if(!(%1)) FAILSTATE_FUNC(__SNAME..."Assertion failed: \""...#%1..."\"")
#define ASSERT_MSG(%1,%2) if(!(%1)) FAILSTATE_FUNC(__SNAME...%2)
#define ASSERT_FINAL(%1) if(!(%1)) SetFailState(__SNAME..."Assertion failed: \""...#%1..."\"")
#define ASSERT_FINAL_MSG(%1,%2) if(!(%1)) SetFailState(__SNAME...%2)

public void OnPluginStart() {
	PrintToServer("%i", A(6));
	
	ASSERT(DHookEnableDetour(dhook, false, TryPlayerMove_Dhook));
}