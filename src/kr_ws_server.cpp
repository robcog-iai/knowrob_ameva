#include "kr_ws_server.h"
#include "ameva.pb.h"


std::map<int, struct lws *> KRWSServer::client_ws_;
KRMessage* KRWSServer::send_buff_;
std::string KRWSServer::recv_buff_;
std::string KRWSServer::file_name_;
std::ofstream KRWSServer::recv_file_;
int KRWSServer::unique_id_ = 0;
bool KRWSServer::is_listen_ = false;
bool KRWSServer::is_finish_ = false;
bool KRWSServer::ready_to_send_ = false;
bool KRWSServer::wait_for_recv_ = false;

std::thread KRWSServer::thrd_;

// HTTP handler
int KRWSServer::callbackHttp( struct lws *wsi, enum lws_callback_reasons reason, void *user, void *in, size_t len )
{
	return 0;
}

// Websocket handler
int KRWSServer::callbackKRWebsocket( struct lws *wsi, enum lws_callback_reasons reason, void *user, void *in, size_t len )
{
	int m;
	switch( reason )
	{
		case LWS_CALLBACK_ESTABLISHED:
		{
			// save new connected client
			std::cout << LOG_LABEL << "A new client connected. Client id : " << unique_id_ << "\n";
            client_ws_.insert(std::pair<int, struct lws *>(unique_id_++, wsi));
			break;
		}	
		case LWS_CALLBACK_RECEIVE:
		{
            // print the message received from clients
            std::map<int, struct lws *>::iterator itr;
            for (itr = client_ws_.begin(); itr != client_ws_.end(); ++itr) 
            { 
                if (itr->second == wsi)
                {
					recv_buff_ = std::string((char*) in, len);
					parseResponse(itr->first);
					break;
                }
            } 
			break;
		}
		case LWS_CALLBACK_SERVER_WRITEABLE:
		{
			// check messages queue and send message
            if (send_buff_ != NULL)
			{
				// LWS_PRE bytes before buffer for adding protocal info
				std::string padding(LWS_PRE, ' ');
				padding += send_buff_->message_;
				m = lws_write(wsi, (unsigned char*)&padding[LWS_PRE], padding.size(), LWS_WRITE_TEXT);
				send_buff_ = NULL;
			}
			break;
		}
		case LWS_CALLBACK_CLOSED:
		{
			// close connection
            std::map<int, struct lws *>::iterator itr;
            for (itr = client_ws_.begin(); itr != client_ws_.end(); ++itr) 
            { 
                if (itr->second == wsi)
                {
                    // TODO: probably need to delete wsi from memory or not necessary
                    std::cout << LOG_LABEL << "Client - " << itr->first << " disconnected.\n";
                    itr->second = nullptr;
					client_ws_.erase(client_ws_.find(itr->first));
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

struct lws_protocols KRWSServer::protocols_[] =
{
	/* The first protocol must always be the HTTP handler */
	{
		"http-only",   	/* name */
		callbackHttp, 	/* callback */
		0,             	/* No per session data. */
		128,           	/* max frame size / rx buffer */
	},
	{
		"kr_websocket",
		callbackKRWebsocket,
		0,
		128,
	},
	{ NULL, NULL, 0, 0 } /* terminator */
};

void KRWSServer::parseResponse(int client)
{
	if (recv_buff_.size() == 0) return;
	sl_pb::KRAmevaResponse response;
	response.ParseFromString(recv_buff_);
	switch (response.type())
	{
	case sl_pb::KRAmevaResponse::Text:
	{
		recv_buff_ = response.text();
		wait_for_recv_ = false;
		std::cout << LOG_LABEL << "CLIENT-" << client << " : " << recv_buff_ << std::endl;
		break;
	}
	case sl_pb::KRAmevaResponse::FileCreation:
	{
		file_name_ = response.filename();
		recv_file_.open( RECV_OWL_DIR + response.filename(), std::ios::binary );
		break;
	}
	case sl_pb::KRAmevaResponse::FileData:
	{
		recv_file_.write(response.filedata().data(), response.datalength());
		recv_file_ << std::flush;
		break;
	}
	case sl_pb::KRAmevaResponse::FileFinish:
	{
		if (file_name_.compare(response.filename()) == 0)
		{
			recv_file_.close();
			recv_buff_ = "Received file " + file_name_;
			wait_for_recv_ = false;
			std::cout << LOG_LABEL << "CLIENT-" << client << " : " << recv_buff_ << std::endl;
		}
		break;
	}
	default:
		break;
	}	
}

KRWSServer* KRWSServer::getInstance()
{
	static KRWSServer instance;
	return &instance;
}

void KRWSServer::serverThread(int port) 
{
	std::cout << LOG_LABEL << "Starting thread.." << std::endl;

    struct lws_context_creation_info info;
	struct lws_context *context;
	int n = 0;

    // websocket handler parameter
	memset( &info, 0, sizeof(info) );
	info.port = port;
	info.protocols = protocols_;
	info.gid = -1;
	info.uid = -1;

    // create websocket handler
	context = lws_create_context( &info );

	if (!context) 
	{
		return;
	}

	std::cout << LOG_LABEL << "Thread running.." << std::endl;
    while( n >= 0 && !is_finish_ )
	{
		// service andy pending websocket activity, non-blocking
		n = lws_service( context, /* timeout_ms = */ 50 );	
		
		// request a callback to write message
		if (!ready_to_send_) continue;
		if (client_ws_.find(send_buff_->client_id_) == client_ws_.end()) 
		{
			std::cout << LOG_LABEL << "Client " << send_buff_->client_id_ << " is not connected\n";
			send_buff_ = NULL;
			wait_for_recv_ = false;
		} else {
			lws_callback_on_writable(client_ws_.find(send_buff_->client_id_)->second);
		}		
		ready_to_send_ = false;	
	}
	lws_context_destroy( context );
	std::cout << LOG_LABEL << "Thread finished.." << std::endl;
}

void KRWSServer::listen(int port)
{
    if (is_listen_)
    {
        std::cout << LOG_LABEL << "Server is already listening" << std::endl;
        return;
    }
    //std::thread (serverThread, port).detach();
	thrd_ = std::thread(serverThread, port);
	thrd_.detach();
    is_listen_ = true;
    is_finish_ = false;
}

void KRWSServer::printClients()
{
    std::cout << LOG_LABEL << "Connected clients:\n";
    std::map<int, struct lws *>::iterator itr;
    for (itr = client_ws_.begin(); itr != client_ws_.end(); ++itr) 
    { 
        std::cout << "client - " << itr->first << "\n";
    } 
}

int KRWSServer::numClients()
{
	return client_ws_.size();
}

bool KRWSServer::checkClient(int client_id)
{
    return !(client_ws_.find(client_id) == client_ws_.end());
}


void KRWSServer::sendMessage(KRMessage* message)
{
    if (!is_listen_)
    {
        std::cout << LOG_LABEL << "Server is not started yet\n";
    }
	send_buff_ = message;
	wait_for_recv_ = true;
	ready_to_send_ = true;
	while (wait_for_recv_) { }
}

void KRWSServer::shutdown()
{
	std::cout << LOG_LABEL << "shutdown" << std::endl;
    unique_id_ = 0;
    client_ws_.clear();
    recv_buff_.empty();
	send_buff_ = NULL;

	ready_to_send_ = false;
    wait_for_recv_ = false;
    is_finish_ = true;
    is_listen_ = false;
}

// Dtor
KRWSServer::~KRWSServer()
{
	std::cout << LOG_LABEL << "Dtor" << std::endl;
	shutdown();
}