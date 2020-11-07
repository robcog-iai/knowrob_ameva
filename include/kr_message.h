#ifndef KR_MESSAGE_H
#define KR_MESSAGE_H

#include <string>

class KRMessage 
{
public:
    int client_id_;
    std::string message_;
    KRMessage(int id, std::string msg) : client_id_(id), message_(msg) {}
};

#endif