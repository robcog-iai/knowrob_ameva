#include "ameva.pb.h"
#include "KRWSServer.h"
#include <SWI-cpp.h>


// Start simulation
PREDICATE(ue_start_simulation, 2)
{
	sl_pb::KRAmevaEvent ameva_event;
	ameva_event.set_functocall(ameva_event.StartSimulation);
	sl_pb::StartSimulationParams* start_simulation_params = ameva_event.mutable_startsimulationparams();
	
    PlTail tail(A2);
    PlTerm e;
    while (tail.next(e)) {
        std::string* id = start_simulation_params->add_id();
        *id = (char*)e;
    }
	std::string proto_str = ameva_event.SerializeAsString();
	KRMessage* message = new KRMessage((int) A1, proto_str);
	std::string response = KRWSServer::get_instance()->send_message(message);
	std::cout << LOG_LABEL<< response << "\n";
    return TRUE;
}


// Stop simulation
PREDICATE(ue_stop_simulation, 2)
{
	sl_pb::KRAmevaEvent ameva_event;
	ameva_event.set_functocall(ameva_event.StopSimulation);
	sl_pb::StopSimulationParams* stop_simulation_params = ameva_event.mutable_stopsimulationparams();
	
    PlTail tail(A2);
    PlTerm e;
    while (tail.next(e)) {
        std::string* id = stop_simulation_params->add_id();
        *id = (char*)e;
    }
	std::string proto_str = ameva_event.SerializeAsString();
	KRMessage* message = new KRMessage((int) A1, proto_str);
	std::string response = KRWSServer::get_instance()->send_message(message);
	std::cout << LOG_LABEL<< response << "\n";
    return TRUE;
}

// Start Symbolic Log
PREDICATE(ue_start_symbolic_log, 3)
{
	sl_pb::KRAmevaEvent ameva_event;
	ameva_event.set_functocall(ameva_event.StartSymbolicLog);
	sl_pb::StartSymbolicLogParams* start_log_params = ameva_event.mutable_startsymboliclogparams();
    start_log_params->set_taskid((char*)A2);
    start_log_params->set_episodeid((char*)A3);
	std::string proto_str = ameva_event.SerializeAsString();
	KRMessage* message = new KRMessage((int) A1, proto_str);
	std::string response = KRWSServer::get_instance()->send_message(message);
	std::cout << LOG_LABEL<< response << "\n";
    return TRUE;
}

// Stop Symbolic Log
PREDICATE(ue_stop_symbolic_log, 1)
{
	sl_pb::KRAmevaEvent ameva_event;
	ameva_event.set_functocall(ameva_event.StopSymbolicLog);
	std::string proto_str = ameva_event.SerializeAsString();
	KRMessage* message = new KRMessage((int) A1, proto_str);
	std::string response = KRWSServer::get_instance()->send_message(message);
	std::cout << LOG_LABEL<< response << "\n";
    return TRUE;
}

