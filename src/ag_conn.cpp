#include <SWI-cpp.h>
#include <sstream> 
#include <curlpp/cURLpp.hpp>
#include <curlpp/Easy.hpp>
#include <curlpp/Options.hpp>
#include "kr_ws_server.h"
#include "str_trim.h"

using namespace curlpp::options;

const char* g_gs_launcher_addr = "127.0.0.1:30002";


// Create PixelStreaming game server instance
PREDICATE(ag_create_clients, 3)
{
	int gs_num = (int) A1;
	if (gs_num < 1) return FALSE;
	std::string level_name = std::string((char*) A2); 
	PlTail list(A3);
 
	for (int i = 0; i < gs_num; i ++)
	{
		try
		{
			int last_latest_id = KRWSServer::getInstance()->latestId();
			int target_latest_id = last_latest_id + 1;
			
			std::list<std::string> header;
			header.push_back("Content-Type: application/json");

			std::string url;
			url.append(g_gs_launcher_addr);
			url.append("/game-server");

			std::string body = "{ \"KRServerPort\" : 8080, \"KRProtocol\" : \"kr_websocket\", \"LevelName\" : \"" + level_name + "\"}";

			curlpp::Cleanup cleanup;
			curlpp::Easy request;
			request.setOpt(new curlpp::options::Url(url));
			request.setOpt(new curlpp::options::HttpHeader(header));
			request.setOpt(new curlpp::options::PostFields(body));
			request.setOpt(new curlpp::options::PostFieldSize(body.length()));

			std::ostringstream response;
			request.setOpt(new curlpp::options::WriteStream(&response));

			request.perform();

			std::string addr = std::string(response.str());
			std::cout << addr << std::endl;
			
			while (KRWSServer::getInstance()->latestId() < target_latest_id) {}
			list.append((int64_t)target_latest_id);
			KRWSServer::getInstance()->setClientAddr(target_latest_id, addr);
		}
		catch(curlpp::RuntimeError & e)
		{
			std::cout << e.what() << std::endl;
		}
		catch(curlpp::LogicError & e)
		{
			std::cout << e.what() << std::endl;
		}
	}

	return  list.close();
}

// List all the PixelStreaming game server 
PREDICATE(ag_list_clients, 0)
{
	try
	{
		// That's all that is needed to do cleanup of used resources (RAII style).
		curlpp::Cleanup cleanup;
		// request to be sent.
		curlpp::Easy request;
		// Set the URL.
		std::string url;
		url.append(g_gs_launcher_addr);
		url.append("/game-servers");
		request.setOpt<Url>(url);
		
		// Send request and get a result.
		// By default the result goes to standard output.
		request.perform();
	}
	catch(curlpp::RuntimeError & e)
	{
		std::cout << e.what() << std::endl;
	}
	catch(curlpp::LogicError & e)
	{
		std::cout << e.what() << std::endl;
	}
	return TRUE;
}

// Shutdown PixelStreaming game server
PREDICATE(ag_close_clients, 1)
{
	PlTail ue_client_list(A1);
    PlTerm ue_client;
	
	while (ue_client_list.next(ue_client)) 
	{
		std::string addr = KRWSServer::getInstance()->getClientAddr((int)ue_client);
		try
		{
			// That's all that is needed to do cleanup of used resources (RAII style).
			curlpp::Cleanup cleanup;
			// request to be sent.
			curlpp::Easy request;
			// Set the URL.
			std::string url;
			url.append(trim(addr));
			url.append("/shutdown");
			request.setOpt<Url>(url);
			
			// Send request and get a result.
			// By default the result goes to standard output.
			std::ostringstream response;
			request.setOpt(new curlpp::options::WriteStream(&response));
			request.perform();
			std::cout << response.str() << std::endl;
		}
		catch(curlpp::RuntimeError & e)
		{
			std::cout << e.what() << std::endl;
		}
		catch(curlpp::LogicError & e)
		{
			std::cout << e.what() << std::endl;
		}

		KRWSServer::getInstance()->removeClientAddr((int)ue_client);
	}
	
	return TRUE;
}