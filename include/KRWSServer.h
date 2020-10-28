#ifndef KR_WSSERVER_H
#define KR_WSSERVER_H

#define WS_PROTOCOL "kr_websocket"
#define LOG_LABEL "[AMEVA] "

#include <libwebsockets.h>
#include <map>
#include <queue>
#include <mutex>
#include <iostream>
#include <thread>
#include "KRMessage.h"

class KRWSServer 
{
public:
    static KRWSServer* get_instance();
    void listen(int port);
    void print_clients();
    int num_clients();
    bool check_client(int client_id);
    void send_message(KRMessage* message);
    void shutdown();

private:
    KRWSServer() {}
    ~KRWSServer() {}

    static void server_thread(int port);

    static bool is_listen;
    static bool is_finish;

    static std::map<int, struct lws *> client_ws;
    static std::queue<KRMessage*> queue;
    static std::mutex lock;
    static int unique_id;

private:
    static int callback_http( struct lws *wsi, enum lws_callback_reasons reason, void *user, void *in, size_t len );
    static int callback_krwebsocket( struct lws *wsi, enum lws_callback_reasons reason, void *user, void *in, size_t len );
    
    static struct lws_protocols protocols[];
};

#endif