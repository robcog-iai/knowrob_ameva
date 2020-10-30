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
    std::string send_message(KRMessage* message);
    void shutdown();

private:
    KRWSServer() {}
    ~KRWSServer();

    static void server_thread(int port);

    //static void start_thread(int port);
    //static void stop_thread(int port);

    static bool is_listen;
    static bool is_finish;
    static bool ready_to_send;
    static bool wait_for_recv;

    static std::map<int, struct lws *> client_ws;
    static KRMessage* send_buff;
    static std::string recv_buff;
    static int unique_id;

    static std::thread thrd;

private:
    static int callback_http( struct lws *wsi, enum lws_callback_reasons reason, void *user, void *in, size_t len );
    static int callback_krwebsocket( struct lws *wsi, enum lws_callback_reasons reason, void *user, void *in, size_t len );
    
    static struct lws_protocols protocols[];
};

#endif