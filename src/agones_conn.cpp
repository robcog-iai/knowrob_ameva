#include <SWI-cpp.h>
#include <sstream> 
#include <curlpp/cURLpp.hpp>
#include <curlpp/Easy.hpp>
#include <curlpp/Options.hpp>
#include "kr_ws_server.h"

using namespace curlpp::options;

const char* g_gs_launcher_addr = "127.0.0.1:9090";

// Create PixelStreaming game server instance
PREDICATE(ue_create_gs, 2)
{
	int gs_num = (int) A1;
	if (gs_num < 1) return FALSE;
	int last_latest_id = KRWSServer::getInstance()->latestId();
	int target_latest_id = last_latest_id + gs_num;
 
	for (int i = 0; i < gs_num; i ++)
	{
		try
		{
			std::list<std::string> header;
			header.push_back("Content-Type: application/json");

			std::string url;
			url.append(g_gs_launcher_addr);
			url.append("/game-server");

			std::string body = "{ \"KRServerPort\" : 8080, \"KRProtocol\" : \"kr_websocket\" }";

			curlpp::Cleanup cleanup;
			curlpp::Easy request;
			request.setOpt(new curlpp::options::Url(url));
			request.setOpt(new curlpp::options::HttpHeader(header));
			request.setOpt(new curlpp::options::PostFields(body));
			request.setOpt(new curlpp::options::PostFieldSize(body.length()));

			std::ostringstream response;
			request.setOpt(new curlpp::options::WriteStream(&response));

			request.perform();
			std::cout << std::string(response.str());
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
	
	while (KRWSServer::getInstance()->latestId() < target_latest_id) {}

	PlTail list(A2);
	for(int i = last_latest_id + 1; i <= target_latest_id; i++)
	{
		list.append((int64_t)i);
	}	
  	list.close();

	return  list.close();
}

// List all the PixelStreaming game server 
PREDICATE(ue_list_gs, 0)
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