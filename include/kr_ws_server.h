#ifndef KR_WSSERVER_H
#define KR_WSSERVER_H

#define WS_PROTOCOL "kr_websocket"
#define LOG_LABEL "[AMEVA] "
#define RECV_OWL_DIR "/home/robcog/catkin_ws/data/"

#include <libwebsockets.h>
#include <map>
#include <queue>
#include <mutex>
#include <iostream>
#include <thread>
#include <fstream>
#include "kr_message.h"

class KRWSServer 
{
public:
    static KRWSServer* getInstance();
    void listen(int port);
    void printClients();
    int numClients();
    int latestId();
    bool checkClient(int client_id);
    void sendMessage(KRMessage* message);
    void shutdown();
    
    void setClientAddr(int client_id, std::string client_addr);
    std::string getClientAddr(int client_id);
    void removeClientAddr(int client_id);

private:
    KRWSServer() {}
    ~KRWSServer();

    static void serverThread(int port);

    //static void start_thread(int port);
    //static void stop_thread(int port);

    static bool is_listen_;
    static bool is_finish_;
    static bool ready_to_send_;

    static bool wait_for_recv_;

    static std::map<int, struct lws *> client_ws_;
    static std::map<int, std::string> client_addrs_;
    static KRMessage* send_buff_;
    static std::string recv_buff_;
    static std::string file_name_;
    static std::ofstream recv_file_;
    static int unique_id_;

    static std::thread thrd_;

private:
    static int callbackHttp( struct lws *wsi, enum lws_callback_reasons reason, void *user, void *in, size_t len );
    static int callbackKRWebsocket( struct lws *wsi, enum lws_callback_reasons reason, void *user, void *in, size_t len );
    
    static void parseResponse(int client); 

    static struct lws_protocols protocols_[];
};

#endif