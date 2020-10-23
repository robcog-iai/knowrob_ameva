#include "ameva.pb.h"
#include "KRWSServer.h"
#include <SWI-cpp.h>

sl_pb::MarkerType get_mesh_type(char* type)
{

	if (strcmp(type, "sphere") == 0)
	{
		return sl_pb::Sphere;
	}
	else if (strcmp(type, "cyclinder") == 0)
	{
		return sl_pb::Cylinder;
	}
	else if (strcmp(type, "arrow") == 0)
	{
		return sl_pb::Arrow;
	}
	else if (strcmp(type, "axis") == 0)
	{
		return sl_pb::Axis;
	}
	return sl_pb::Box;
}

// Set the task 
PREDICATE(ue_set_task, 2)
{
	sl_pb::KRAmevaEvent ameva_event;
	ameva_event.set_functocall(ameva_event.SetTask);
	sl_pb::SetTaskParams* set_task_params = ameva_event.mutable_settaskparam();
	set_task_params->set_task((char*)A2);
	std::string proto_str = ameva_event.SerializeAsString();

	KRMessage* message = new KRMessage((int) A1, proto_str);
	KRWSServer::get_instance()->send_message(message);
	return TRUE;
}


// Set the episode
PREDICATE(ue_set_episode, 2)
{
	sl_pb::KRAmevaEvent ameva_event;
	ameva_event.set_functocall(ameva_event.SetEpisode);
	sl_pb::SetEpisodeParams* set_episode_params = ameva_event.mutable_setepisodeparams();
	set_episode_params->set_episode((char*)A2);
	std::string proto_str = ameva_event.SerializeAsString();

	KRMessage* message = new KRMessage((int) A1, proto_str);
	KRWSServer::get_instance()->send_message(message);

	return TRUE;
}

// Call the draw marker function
PREDICATE(ue_draw_marker, 7)
{ 
	if (!KRWSServer::get_instance()->check_client((int)A1))
		return FALSE;

	sl_pb::KRAmevaEvent ameva_event;
	ameva_event.set_functocall(ameva_event.DrawMarkerAt);
	sl_pb::DrawMarkerAtParams* marker_params = ameva_event.mutable_drawmarkeratparams();
	marker_params->set_id((char*)A2);
	marker_params->set_timestamp((double)A3);
	marker_params->set_marker(get_mesh_type((char*)A4));
	marker_params->set_color((char*)A5);
	marker_params->set_scale((double)A6);
	marker_params->set_material((char*)A7);
	std::string proto_str = ameva_event.SerializeAsString();

	KRMessage* message = new KRMessage((int) A1, proto_str);
	KRWSServer::get_instance()->send_message(message);

	return TRUE;
}

// Call the draw marker trajectory function
PREDICATE(ue_draw_marker, 8)
{ 
	if (!KRWSServer::get_instance()->check_client((int)A1))
		return FALSE;

	sl_pb::KRAmevaEvent ameva_event;
	ameva_event.set_functocall(ameva_event.DrawMarkerTraj);
	sl_pb::DrawMarkerTrajParams* marker_traj_params = ameva_event.mutable_drawmarkertrajparams();
	marker_traj_params->set_id((char*)A2);
	marker_traj_params->set_start((double)A3);
	marker_traj_params->set_end((double)A4);
	marker_traj_params->set_marker(get_mesh_type((char*)A5));
	marker_traj_params->set_color((char*)A6);
	marker_traj_params->set_scale((double)A7);
	marker_traj_params->set_material((char*)A8);

	std::string proto_str = ameva_event.SerializeAsString();
	KRMessage* message = new KRMessage((int) A1, proto_str);
	KRWSServer::get_instance()->send_message(message);
	return TRUE;
}
