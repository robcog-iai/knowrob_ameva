#include "ameva.pb.h"
#include "KRWSServer.h"
#include <SWI-cpp.h>

// Set the task 
PREDICATE(hello_world, 1)
{
	sl_pb::KRAmevaEvent ameva_event;
	ameva_event.set_functocall(ameva_event.HelloWorld);
	sl_pb::HelloWorldParams* hello_world_params = ameva_event.mutable_helloworldparams();
	hello_world_params->set_msg("hello world from knowrob");
	std::string proto_str = ameva_event.SerializeAsString();

	KRMessage* message = new KRMessage((int) A1, proto_str);
	KRWSServer::get_instance()->send_message(message);
	return TRUE;
}