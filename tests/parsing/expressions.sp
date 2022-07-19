public stock native SharedPlugin __pl_vsh2 = {
	name = "VSH2",
	file = "vsh2.smx",
#if defined REQUIRE_PLUGIN
	required = 1,
#else
	required = 0,
#endif
};

#define INT_SQR    function int(int a){ return a * a; }


static_assert('''
kektus''');

methodmap A {
	public native void B();
}

int a = access ? INT_SQR (10) > 0 ? "❎" : "☑" : "🔒";

void InitNatives()
{
	CreateNative("name", function any(Handle plugin, int numParams){
		return( FF2Player(GetNativeCell(1), GetNativeCell(2)) );
	});
	
	/// Natives For FF2GameMode
	#define CREATE_NATIVE(%0)    CreateNative("FF2GameMode."...#%0   , Native_FF2GameMode_%0)


	CREATE_NATIVE(IsOn);
}
/*
	CREATE_NATIVE(PluginVersion);
	CREATE_NATIVE(ForkVersion);

	CREATE_NATIVE(Cheats);
	
	CREATE_NATIVE(FindVSH2IDByName);

	CREATE_NATIVE(LoadAbility);
	CREATE_NATIVE(SubPlugins);

	/// Natives For FF2Player
	#undef CREATE_NATIVE
	#define CREATE_NATIVE(%0)        CreateNative("FF2Player."...#%0,          Native_FF2Player_%0    )
	#define CREATE_NATIVE_GET(%0)    CreateNative("FF2Player."...#%0...".get", Native_FF2Player_%0_Get)


	CREATE_NATIVE(FF2Player);
	CREATE_NATIVE(GetArgB);
	CREATE_NATIVE(GetArgI);
	CREATE_NATIVE(GetArgF);
	CREATE_NATIVE(GetArgS);

	CREATE_NATIVE(GetInt);
	CREATE_NATIVE(GetFloat);
	CREATE_NATIVE(GetString);
	CREATE_NATIVE(GetSection);

	CREATE_NATIVE(GetConfigName);

	CREATE_NATIVE(HasAbility);
	CREATE_NATIVE(DoAbility);
	CREATE_NATIVE(ForceAbility);

	CREATE_NATIVE(RandomSound);
	CREATE_NATIVE(RageDist);

	CREATE_NATIVE_GET(SoundMap);
	CREATE_NATIVE_GET(HookedAbilities);
	CREATE_NATIVE(PlayBGM);


	#undef CREATE_NATIVE
	#undef CREATE_NATIVE_GET
}
*/


/*
public any Native_ZZZ(Handle plugin, int numParams)
{
	return 0;
}
*/


/*
stock bool FF2_Debug() {
#if defined DEBUG || defined _DEBUG
	return true;
#else
	return false;
#endif
}

static stock void ReplaceEscapeSeq(char[] str, int size) {
	char list[][][] = {
		{ "\t", "\\t" },
		{ "\n", "\\n" },
		{ "\r", "\\r" }
	};
	
	for( int i; i < sizeof(list); i++ ) {
		ReplaceString(str, size, list[i][0], list[i][1]);
	}
}


methodmap foo {
	public native GlobalForward(const char[] name, ExecType type, ParamType ...);
	
	public KeyValType GetIntKeyValType(int key) {
		return this.GetKeyValType(key_str);
	}
	
	property int bar {
		public get() {
			return bar_peanuts;
		}
	}
	property Decimal baz {
		public native get();
		public native set(Decimal value);
	}
	
	property int barnaby {
		public set(int v) {
			this.baz = view_as< Decimal >(v);
		}
	}
}


public Extension __ext_core = {
	name = "Core",
	file = "core",
	autoload = 0,
	required = 0,
};

using __intrinsics__.Handle;

typeset SDKHookCB {
	function void (int client);
	
	function Action (int victim, int &attacker, int &inflictor, float &damage, int &damagetype, int &weapon, float damageForce[3], float damagePosition[3]);
};

enum NamedEnum {
	one = 1 << 0,
	two = 1 << 1,
	three = 1 << 2,
};

enum {
	one,
	two,
	three
};

typedef Foo = function void (int n[3][4]);

public void OnPluginStart() {
	int[] i = new int[10];
}

native float GetVectorLength(const float vec[3][4] = {0000}, const float vec2[3]);

stock void MakeVectorFromPoints(const float pt1[3], const float pt2[3], float output[3]) {
	output[0] = pt2[0] - pt1[0];
	output[1] = pt2[1] - pt1[1];
	output[2] = pt2[2] - pt1[2];
}

native void RequireFeature(FeatureType type, const char[] name, const char[] fmt="", any ...);

struct PlVers {
	public int version;
	public const char[] filevers;
	public const char[] date;
	public const char[] time;
};

enum struct Donor {
	Cookie m_hDonors;
	
	int GetDonorTime(int userid) {
		int client = GetClientOfUserId(userid);
		if( !IsClientValid(client) )
			return 0;
		
		char timestamp_str[TIMESTAMP_SIZE];
		this.m_hDonors.Get(client, timestamp_str, sizeof(timestamp_str));
		return StringToInt(timestamp_str);
	}
	
	bool MakeDonor(int userid, int weeks) {
		int client = GetClientOfUserId(userid);
		if( !IsClientValid(client) )
			return false;
		
		int donor_time = this.GetDonorTime(userid);
		int curr_time = GetTime();
		int week_stamp = weeks * DONOR_TIME;
		PrintToConsole(client, "[DonorLite] :: weeks %i, DONOR_TIME %i, week_stamp %i, donor_time %i", weeks, DONOR_TIME, week_stamp, donor_time);
		if( donor_time > 0 && donor_time > curr_time ) {
			donor_time += week_stamp;
		} else {
			donor_time = curr_time + week_stamp;
		}
		
		PrintToConsole(client, "[DonorLite] :: curr_time %i, donor_time %i", curr_time, donor_time);
		char new_timestamp[TIMESTAMP_SIZE]; IntToString(donor_time, new_timestamp, sizeof(new_timestamp));
		PrintToConsole(client, "[DonorLite] :: new_timestamp = '%s'", new_timestamp);
		this.m_hDonors.Set(client, new_timestamp);
		AddUserFlags(client, Admin_Custom1);
		return true;
	}
	
	bool RemoveDonor(int userid) {
		int client = GetClientOfUserId(userid);
		if( !IsClientValid(client) || this.GetDonorStatus(userid) != DonorStatus_Active )
			return false;
		
		int donor_time = this.GetDonorTime(userid);
		if( donor_time != 0 ) {
			this.m_hDonors.Set(client, "0");
			return true;
		}
		return false;
	}
	
	DonorStatus GetDonorStatus(int userid) {
		int client = GetClientOfUserId(userid);
		if( !IsClientValid(client) )
			return DonorStatus_Inactive;
		
		int donor_time = this.GetDonorTime(userid);
		if( donor_time != 0 && donor_time <= GetTime() ) {
			this.m_hDonors.Set(client, "0");
			return DonorStatus_Expired;
		} else if( donor_time==0 ) {
			return DonorStatus_Inactive;
		}
		return DonorStatus_Active;
	}
	
	int GetDonorTimeLeft(int userid, int time[6]) {
		int client = GetClientOfUserId(userid);
		if( !IsClientValid(client) )
			return -1;
		else if( this.GetDonorStatus(userid) != DonorStatus_Active )
			return 0;
		
		int timestamp = this.GetDonorTime(userid) - GetTime();
		time[0] = timestamp / UNIX_MONTH;
		timestamp %= UNIX_MONTH;
		
		time[1] = timestamp / UNIX_WEEK;
		timestamp %= UNIX_WEEK;
		
		time[2] = timestamp / UNIX_DAY;
		timestamp %= UNIX_DAY;
		
		time[3] = timestamp / UNIX_HOUR;
		timestamp %= UNIX_HOUR;
		
		time[4] = timestamp / UNIX_MINUTE;
		timestamp %= UNIX_MINUTE;
		
		time[5] = timestamp;
		return 1;
	}
	
	void CheckDonor(int userid) {
		int client = GetClientOfUserId(userid);
		DonorStatus status = this.GetDonorStatus(userid);
		switch( status ) {
			case DonorStatus_Active:  AddUserFlags(client, Admin_Custom1);
			case DonorStatus_Expired: CreateTimer(10.0, DonorExpiredMessage, userid);
		}
	}
};
*/
