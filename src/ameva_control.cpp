#include "ameva.pb.h"
#include "kr_ws_server.h"
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
	std::string response = KRWSServer::getInstance()->sendMessage(message);
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
	std::string response = KRWSServer::getInstance()->sendMessage(message);
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
	std::string response = KRWSServer::getInstance()->sendMessage(message);
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
	std::string response = KRWSServer::getInstance()->sendMessage(message);
	std::cout << LOG_LABEL << response << "\n";
    return TRUE;
}

// Move Individual
PREDICATE(ue_move_individual, 9)
{
	sl_pb::KRAmevaEvent ameva_event;
	ameva_event.set_functocall(ameva_event.MoveIndividual);
	sl_pb::MoveIndividualParams* move_individual_params = ameva_event.mutable_moveindividualparams();
	move_individual_params->set_id((char*)A2);
	move_individual_params->set_vecx((int)A3);
	move_individual_params->set_vecy((int)A4);
	move_individual_params->set_vecz((int)A5);
	move_individual_params->set_quatw((int)A6);
	move_individual_params->set_quatx((int)A7);
	move_individual_params->set_quaty((int)A8);
	move_individual_params->set_quatz((int)A9);
	std::string proto_str = ameva_event.SerializeAsString();
	KRMessage* message = new KRMessage((int) A1, proto_str);
	std::string response = KRWSServer::getInstance()->sendMessage(message);
	std::cout << LOG_LABEL<< response << "\n";
    return TRUE;
}

// Start simulation and symbolic logging for seconds
PREDICATE(ue_simulate_and_log, 5)
{
	sl_pb::KRAmevaEvent ameva_event;
	ameva_event.set_functocall(ameva_event.SimulateAndLogForSeconds);
	sl_pb::SimulateAndLogForSecondsParams* simulate_and_log_params = ameva_event.mutable_simulateandlogforsecondsparams();
	simulate_and_log_params->set_taskid((char*)A2);
	simulate_and_log_params->set_episodeid((char*)A3);
	simulate_and_log_params->set_seconds((int)A4);
	PlTail tail(A5);
    PlTerm e;
    while (tail.next(e)) {
        std::string* id = simulate_and_log_params->add_id();
        *id = (char*)e;
    }
	std::string proto_str = ameva_event.SerializeAsString();
	KRMessage* message = new KRMessage((int) A1, proto_str);
	std::string response = KRWSServer::getInstance()->sendMessage(message);
	std::cout << LOG_LABEL<< response << "\n";
    return TRUE;
}