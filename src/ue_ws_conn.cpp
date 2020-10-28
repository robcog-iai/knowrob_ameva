#include "KRWSServer.h"
#include <SWI-cpp.h>

// Start websocket server on port 8080
PREDICATE(ue_start_srv, 1)
{ 
	KRWSServer::get_instance()->listen((int) A1);
	return TRUE;
}

// Display all the connected clients
PREDICATE(ue_show_clients, 0)
{ 
	KRWSServer::get_instance()->print_clients();
	return TRUE;
}

// Display all the connected clients
PREDICATE(ue_num_clients, 1)
{ 
	A1 = KRWSServer::get_instance()->num_clients();
	return TRUE;
}

// Shutdown the websocket server
PREDICATE(ue_close_srv, 0)
{ 
	KRWSServer::get_instance()->shutdown();
	return TRUE;
}