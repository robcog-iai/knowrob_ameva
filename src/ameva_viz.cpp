#include "ameva.pb.h"
#include "kr_ws_server.h"
#include <SWI-cpp.h>

sl_pb::MarkerType getMeshType(char* type)
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

// Load level 
PREDICATE(ue_load_map, 2)
{
	sl_pb::KRAmevaEvent ameva_event;
	ameva_event.set_functocall(ameva_event.LoadLevel);
	sl_pb::LoadLevelParams* load_level_params = ameva_event.mutable_loadlevelparams();
	load_level_params->set_level((char*)A2);
	std::string proto_str = ameva_event.SerializeAsString();

	PlTail ue_client_list(A1);
    PlTerm ue_client;
	while (ue_client_list.next(ue_client)) 
	{
		KRMessage* message = new KRMessage((int)ue_client, proto_str);
		KRWSServer::getInstance()->sendMessage(message);
		delete message;
    }
	return TRUE;
}

// Set the task 
PREDICATE(ue_set_task, 2)
{
	sl_pb::KRAmevaEvent ameva_event;
	ameva_event.set_functocall(ameva_event.SetTask);
	sl_pb::SetTaskParams* set_task_params = ameva_event.mutable_settaskparam();
	set_task_params->set_task((char*)A2);
	std::string proto_str = ameva_event.SerializeAsString();

	PlTail ue_client_list(A1);
    PlTerm ue_client;
	while (ue_client_list.next(ue_client)) 
	{
		KRMessage* message = new KRMessage((int)ue_client, proto_str);
		KRWSServer::getInstance()->sendMessage(message);
		delete message;
    }
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

	PlTail ue_client_list(A1);
    PlTerm ue_client;
	while (ue_client_list.next(ue_client)) 
	{
		KRMessage* message = new KRMessage((int)ue_client, proto_str);
		KRWSServer::getInstance()->sendMessage(message);
		delete message;
    }
	return TRUE;
}

// Call the draw marker function
PREDICATE(ue_draw_marker, 7)
{ 
	if (!KRWSServer::getInstance()->checkClient((int)A1))
		return FALSE;

	sl_pb::KRAmevaEvent ameva_event;
	ameva_event.set_functocall(ameva_event.DrawMarkerAt);
	sl_pb::DrawMarkerAtParams* marker_params = ameva_event.mutable_drawmarkeratparams();
	marker_params->set_id((char*)A2);
	marker_params->set_timestamp((double)A3);
	marker_params->set_marker(getMeshType((char*)A4));
	marker_params->set_color((char*)A5);
	marker_params->set_scale((double)A6);
	marker_params->set_material((char*)A7);
	std::string proto_str = ameva_event.SerializeAsString();

	PlTail ue_client_list(A1);
    PlTerm ue_client;
	while (ue_client_list.next(ue_client)) 
	{
		KRMessage* message = new KRMessage((int)ue_client, proto_str);
		KRWSServer::getInstance()->sendMessage(message);
		delete message;
    }
	return TRUE;
}

// Call the draw marker trajectory function
PREDICATE(ue_draw_marker, 8)
{ 
	if (!KRWSServer::getInstance()->checkClient((int)A1))
		return FALSE;

	sl_pb::KRAmevaEvent ameva_event;
	ameva_event.set_functocall(ameva_event.DrawMarkerTraj);
	sl_pb::DrawMarkerTrajParams* marker_traj_params = ameva_event.mutable_drawmarkertrajparams();
	marker_traj_params->set_id((char*)A2);
	marker_traj_params->set_start((double)A3);
	marker_traj_params->set_end((double)A4);
	marker_traj_params->set_marker(getMeshType((char*)A5));
	marker_traj_params->set_color((char*)A6);
	marker_traj_params->set_scale((double)A7);
	marker_traj_params->set_material((char*)A8);
	std::string proto_str = ameva_event.SerializeAsString();
	
	PlTail ue_client_list(A1);
    PlTerm ue_client;
	while (ue_client_list.next(ue_client)) 
	{
		KRMessage* message = new KRMessage((int)ue_client, proto_str);
		KRWSServer::getInstance()->sendMessage(message);
		delete message;
    }
	return TRUE;
}

PREDICATE(ue_highlight, 4)
{
	if (!KRWSServer::getInstance()->checkClient((int)A1))
		return FALSE;
	
	sl_pb::KRAmevaEvent ameva_event;
	ameva_event.set_functocall(ameva_event.Highlight);
	sl_pb::HighlightParams* highlight_params = ameva_event.mutable_highlightparams();
	highlight_params->set_id((char*)A2);
	highlight_params->set_color((char*)A3);
	highlight_params->set_material((char*)A4);
	std::string proto_str = ameva_event.SerializeAsString();
	
	PlTail ue_client_list(A1);
    PlTerm ue_client;
	while (ue_client_list.next(ue_client)) 
	{
		KRMessage* message = new KRMessage((int)ue_client, proto_str);
		KRWSServer::getInstance()->sendMessage(message);
		delete message;
    }
	return TRUE;
}

PREDICATE(ue_remove_highlight, 2)
{
	if (!KRWSServer::getInstance()->checkClient((int)A1))
		return FALSE;
	
	sl_pb::KRAmevaEvent ameva_event;
	ameva_event.set_functocall(ameva_event.RemoveHighlight);
	sl_pb::RemoveHighlightParams* highlight_params = ameva_event.mutable_removehighlightparams();
	highlight_params->set_id((char*)A2);
	std::string proto_str = ameva_event.SerializeAsString();	

	PlTail ue_client_list(A1);
    PlTerm ue_client;
	while (ue_client_list.next(ue_client)) 
	{
		KRMessage* message = new KRMessage((int)ue_client, proto_str);
		KRWSServer::getInstance()->sendMessage(message);
		delete message;
    }
	return TRUE;
}


PREDICATE(ue_remove_all_highlight, 1)
{
	if (!KRWSServer::getInstance()->checkClient((int)A1))
		return FALSE;
	
	sl_pb::KRAmevaEvent ameva_event;
	ameva_event.set_functocall(ameva_event.RemoveAllHighlight);
	std::string proto_str = ameva_event.SerializeAsString();
	
	PlTail ue_client_list(A1);
    PlTerm ue_client;
	while (ue_client_list.next(ue_client)) 
	{
		KRMessage* message = new KRMessage((int)ue_client, proto_str);
		KRWSServer::getInstance()->sendMessage(message);
		delete message;
    }
	return TRUE;
}