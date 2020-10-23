#ifndef KR_MESSAGE_H
#define KR_MESSAGE_H

#include <string>

class KRMessage 
{
public:
    int client_id;
    std::string message;
    KRMessage(int id, std::string msg) : client_id(id), message(msg) {}
};

#endif