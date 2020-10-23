#include <SWI-cpp.h>
#include <sstream> 
#include <curlpp/cURLpp.hpp>
#include <curlpp/Easy.hpp>
#include <curlpp/Options.hpp>

using namespace curlpp::options;

const char* gs_launcher_addr = "127.0.0.1:9090";

// Create a PixelStreaming game server instance
PREDICATE(ue_create_gs, 0)
{
	try
	{
		std::list<std::string> header;
		header.push_back("Content-Type: application/json");

		std::string url;
		url.append(gs_launcher_addr);
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
	return TRUE;
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
		url.append(gs_launcher_addr);
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