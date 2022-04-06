#define IsClientValid(%1)    (0 < %1 <= MaxClients)

IsClientValid(client) ;

#define MAXPLAYERS    34
#define PLYR          MAXPLAYERS + 1

int max[PLYR] ;