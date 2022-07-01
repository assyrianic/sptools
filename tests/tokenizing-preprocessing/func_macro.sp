/*
#define MARK_OPTIONAL(%0) 		MarkNativeAsOptional("FF2Player."...#%0)
#define MARK_OPTIONAL_GET(%0) 	MarkNativeAsOptional("FF2Player."...#%0...".get")

MARK_OPTIONAL(FF2Player);

MARK_OPTIONAL_GET(HookedAbilities);

#define IsClientValid(%1)    (0 < %1 <= MaxClients)

IsClientValid(client) ;

#define MAXPLAYERS    34
#define PLYR          MAXPLAYERS + 1

int max[PLYR] ;

#define MARK_OPTIONAL(%0) 		MarkNativeAsOptional("FF2Player."...#%0)
#define MARK_OPTIONAL_GET(%0) 	MarkNativeAsOptional("FF2Player."...#%0...".get")

MARK_OPTIONAL(FF2Player);

MARK_OPTIONAL(GetInt);
MARK_OPTIONAL(GetFloat);
MARK_OPTIONAL(GetString);
MARK_OPTIONAL(GetSection);
MARK_OPTIONAL(GetArgB);
MARK_OPTIONAL(GetArgI);
MARK_OPTIONAL(GetArgF);
MARK_OPTIONAL(GetArgS);

MARK_OPTIONAL(HasAbility);
MARK_OPTIONAL(DoAbility);
MARK_OPTIONAL(ForceAbility);
MARK_OPTIONAL(RandomSound);
MARK_OPTIONAL(RageDist);
MARK_OPTIONAL(GetConfigName);

MARK_OPTIONAL_GET(HookedAbilities);
MARK_OPTIONAL(PlayBGM);


#undef MARK_OPTIONAL
#undef MARK_OPTIONAL_GET



#define MARK_OPTIONAL(%0) 		MarkNativeAsOptional("FF2GameMode."...#%0)


CREATE_NATIVE(IsOn);
CREATE_NATIVE(PluginVersion);
CREATE_NATIVE(ForkVersion);

CREATE_NATIVE(Cheats);

CREATE_NATIVE(FindVSH2IDByName);

CREATE_NATIVE(LoadAbility);
CREATE_NATIVE(SubPlugins);


#undef MARK_OPTIONAL
#undef MARK_OPTIONAL_GET
*/

#define INVALID_FF2PLAYER    ToFF2Player(-1)
#define ToFF2Player(%0)      view_as< FF2Player >(%0)
#define FF2_PREFIX           "{olive}[FF2]{default} "
#define FF2_RESOLVE_FUNC(%0)	static stock void FF2Resolve_%0(ArrayList delete_list, const ConfigMapAllocator cfg, char[] key, int len)

INVALID_FF2PLAYER
FF2_RESOLVE_FUNC(kektus)