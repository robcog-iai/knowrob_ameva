#include "ameva.pb.h"
#include "kr_ws_server.h"
#include <SWI-cpp.h>


// Start simulation
PREDICATE(ue_start_simulation, 3)
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
	start_simulation_params->set_duration((double)A3);
	std::string proto_str = ameva_event.SerializeAsString();
	KRMessage* message = new KRMessage((int) A1, proto_str);
	KRWSServer::getInstance()->sendMessage(message);
	delete message;
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
	KRWSServer::getInstance()->sendMessage(message);
	delete message;
    return TRUE;
}

// Start Symbolic Log
PREDICATE(ue_start_loggers, 3)
{
	sl_pb::KRAmevaEvent ameva_event;
	ameva_event.set_functocall(ameva_event.StartLoggers);
	sl_pb::StartLoggersParams* start_loggers_params = ameva_event.mutable_startloggersparams();
    start_loggers_params->set_taskid((char*)A2);
    start_loggers_params->set_episodeid((char*)A3);
	std::string proto_str = ameva_event.SerializeAsString();
	KRMessage* message = new KRMessage((int) A1, proto_str);
	KRWSServer::getInstance()->sendMessage(message);
	delete message;
    return TRUE;
}

// Stop Symbolic Log
PREDICATE(ue_stop_loggers, 1)
{
	sl_pb::KRAmevaEvent ameva_event;
	ameva_event.set_functocall(ameva_event.StopLoggers);
	std::string proto_str = ameva_event.SerializeAsString();
	KRMessage* message = new KRMessage((int) A1, proto_str);
	KRWSServer::getInstance()->sendMessage(message);
	delete message;
    return TRUE;
}
// Receive symbolic log result
PREDICATE(ue_get_episode_data, 3)
{
	sl_pb::KRAmevaEvent ameva_event;
	ameva_event.set_functocall(ameva_event.GetEpisodeData);
	sl_pb::GetEpisodeDataParams* recv_log_params = ameva_event.mutable_getepisodedataparams();
    recv_log_params->set_taskid((char*)A2);
    recv_log_params->set_episodeid((char*)A3);
	std::string proto_str = ameva_event.SerializeAsString();
	KRMessage* message = new KRMessage((int) A1, proto_str);
	KRWSServer::getInstance()->sendMessage(message);
	delete message;
    return TRUE;
}

// Set individual pose
PREDICATE(ue_set_individual_pose, 9)
{
	sl_pb::KRAmevaEvent ameva_event;
	ameva_event.set_functocall(ameva_event.SetIndividualPose);
	sl_pb::SetIndividualPoseParams* set_individual_pose_params = ameva_event.mutable_setindividualposeparams();
	set_individual_pose_params ->set_id((char*)A2);
	set_individual_pose_params->set_vecx((double)A3);
	set_individual_pose_params->set_vecy((double)A4);
	set_individual_pose_params->set_vecz((double)A5);
	set_individual_pose_params->set_quatw((double)A6);
	set_individual_pose_params->set_quatx((double)A7);
	set_individual_pose_params->set_quaty((double)A8);
	set_individual_pose_params->set_quatz((double)A9);
	std::string proto_str = ameva_event.SerializeAsString();
	KRMessage* message = new KRMessage((int) A1, proto_str);
	KRWSServer::getInstance()->sendMessage(message);
	delete message;
    return TRUE;
}

// Move Individual
PREDICATE(ue_apply_force_to, 5)
{
	sl_pb::KRAmevaEvent ameva_event;
	ameva_event.set_functocall(ameva_event.ApplyForceTo);
	sl_pb::ApplyForceToParams* apply_force_params = ameva_event.mutable_applyforcetoparams();
	apply_force_params->set_id((char*)A2);
	apply_force_params->set_forcex((int)A3);
	apply_force_params->set_forcey((int)A4);
	apply_force_params->set_forcez((int)A5);
	std::string proto_str = ameva_event.SerializeAsString();
	KRMessage* message = new KRMessage((int) A1, proto_str);
	KRWSServer::getInstance()->sendMessage(message);
	delete message;
    return TRUE;
}
