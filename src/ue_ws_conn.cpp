#include "kr_ws_server.h"
#include <SWI-cpp.h>

// Start websocket server on port 8080
PREDICATE(ue_start_srv, 1)
{ 
	KRWSServer::getInstance()->listen((int) A1);
	return TRUE;
}

// Display all the connected clients
PREDICATE(ue_show_clients, 0)
{ 
	KRWSServer::getInstance()->printClients();
	return TRUE;
}

// Display all the connected clients
PREDICATE(ue_num_clients, 1)
{ 
	A1 = KRWSServer::getInstance()->numClients();
	return TRUE;
}

// Shutdown the websocket server
PREDICATE(ue_close_srv, 0)
{ 
	KRWSServer::getInstance()->shutdown();
	return TRUE;
}

PREDICATE(ue_wait_client, 1)
{
	int target_num = (int)A1;
	while (KRWSServer::getInstance()->numClients() < target_num) {}
	return TRUE;	
}

PREDICATE(ue_recent_client, 1)
{
	A1 = KRWSServer::getInstance()->recentId();
	if (A1 < 0) return FALSE;
	return TRUE;
}