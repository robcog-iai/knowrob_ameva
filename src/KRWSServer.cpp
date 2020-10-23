#include "KRWSServer.h"

std::mutex KRWSServer::lock;
std::map<int, struct lws *> KRWSServer::client_ws;
std::queue<KRMessage*> KRWSServer::queue;
int KRWSServer::unique_id = 0;
bool KRWSServer::is_listen = false;
bool KRWSServer::is_finish = false;

// HTTP handler
int KRWSServer::callback_http( struct lws *wsi, enum lws_callback_reasons reason, void *user, void *in, size_t len )
{
	return 0;
}

// Websocket handler
int KRWSServer::callback_krwebsocket( struct lws *wsi, enum lws_callback_reasons reason, void *user, void *in, size_t len )
{
	int m;
	switch( reason )
	{
		case LWS_CALLBACK_ESTABLISHED:
		{
			// save new connected client
			std::cout << LOG_LABEL << "A new client connected. Client id : " << unique_id << "\n";
            client_ws.insert(std::pair<int, struct lws *>(unique_id++, wsi));
			break;
		}	
		case LWS_CALLBACK_RECEIVE:
		{
            // print the message received from clients
            std::map<int, struct lws *>::iterator itr;
            for (itr = client_ws.begin(); itr != client_ws.end(); ++itr) 
            { 
                if (itr->second == wsi)
                {
                    std::cout << LOG_LABEL << "Message from client - " << itr->first << " : " << (char*) in <<"\n";
                    break;
                }
            } 
			break;
		}
		case LWS_CALLBACK_SERVER_WRITEABLE:
		{
			// check messages queue and send message
			lock.lock();
            if (queue.size() > 0)
			{
				KRMessage* kr_message = queue.front();
				// LWS_PRE bytes before buffer for adding protocal info
				std::string padding(LWS_PRE, ' ');
				padding += kr_message->message;
				m = lws_write(wsi, (unsigned char*)&padding[LWS_PRE], padding.size(), LWS_WRITE_TEXT);
				queue.pop();
				delete kr_message;
			}
            lock.unlock();
			break;
		}
		case LWS_CALLBACK_CLOSED:
		{
			// close connection
            std::map<int, struct lws *>::iterator itr;
            for (itr = client_ws.begin(); itr != client_ws.end(); ++itr) 
            { 
                if (itr->second == wsi)
                {
                    // TODO: probably need to delete wsi from memory or not necessary
                    std::cout << LOG_LABEL << "Client - " << itr->first << " disconnected.\n";
                    client_ws.erase(client_ws.begin(), client_ws.find(itr->first));
                    break;
                }
            }
			break;
		}
		default:
			break;
	}

	return 0;
}

struct lws_protocols KRWSServer::protocols[] =
{
	/* The first protocol must always be the HTTP handler */
	{
		"http-only",   	/* name */
		callback_http, 	/* callback */
		0,             	/* No per session data. */
		128,           	/* max frame size / rx buffer */
	},
	{
		"kr_websocket",
		callback_krwebsocket,
		0,
		128,
	},
	{ NULL, NULL, 0, 0 } /* terminator */
};


KRWSServer* KRWSServer::get_instance()
{
	static KRWSServer instance;
	return &instance;
}

void KRWSServer::server_thread(int port) 
{
    struct lws_context_creation_info info;
	struct lws_context *context;
	int n = 0;

    // websocket handler parameter
	memset( &info, 0, sizeof(info) );
	info.port = port;
	info.protocols = protocols;
	info.gid = -1;
	info.uid = -1;

    // create websocket handler
	context = lws_create_context( &info );

	if (!context) 
	{
		return;
	}

    while( n >= 0 && !is_finish )
	{
		// service andy pending websocket activity, non-blocking
		n = lws_service( context, /* timeout_ms = */ 50 );	
		
		// request a callback to write message
        lock.lock();
		if (queue.size() > 0)
		{
			KRMessage* kr_message = queue.front();
			if (client_ws.find(kr_message->client_id) == client_ws.end()) 
			{
				std::cout << LOG_LABEL << "Client " << kr_message->client_id << " is not connected\n";
				queue.pop();
				delete kr_message;
			} else{
				lws_callback_on_writable(client_ws.find(kr_message->client_id)->second);
			}			
		}	
        lock.unlock();
	}
	lws_context_destroy( context );

}
void KRWSServer::listen(int port)
{
    if (is_listen)
    {
        std::cout << LOG_LABEL << "Server is already listening";
        return;
    }
    std::thread (server_thread, port).detach();
    is_listen = true;
    is_finish = false;
}

void KRWSServer::print_clients()
{
    std::cout << LOG_LABEL << "Connected clients:\n";
    std::map<int, struct lws *>::iterator itr;
    for (itr = client_ws.begin(); itr != client_ws.end(); ++itr) 
    { 
        std::cout << "client - " << itr->first << "\n";
    } 
}

bool KRWSServer::check_client(int client_id)
{
    return !(client_ws.find(client_id) == client_ws.end());
}


void KRWSServer::send_message(KRMessage* message)
{
    if (!is_listen)
    {
        std::cout << LOG_LABEL << "Server is not started yet";
        return;
    }
    lock.lock();
    queue.push(message);
    lock.unlock();
}

void KRWSServer::shutdown()
{
    unique_id = 0;
    client_ws.clear();
    std::queue<KRMessage*> empty;
    std::swap(queue, empty);

    is_finish = true;
    is_listen = false;
}