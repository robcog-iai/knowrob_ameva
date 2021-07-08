
# knowrob_ameva

![](documentation/GIF/dw_sim_prospection.gif)

![](documentation/GIF/drawer_sim_prospection.gif)

### 18.04

* sudo apt install libcurlpp-dev
 
## Usage

* `rosrun rosprolog rosprolog knowrob_ameva`
* `ue_start_srv(8080).`
* `ue_close_srv().`

## Tutorial of creating query to communicate with Unreal
Goal : Create prolog query `hello_world('Current time is', 12.43)`, which will send a string and a float to KnowrobManager in Unreal.

1. Add new event in knowrob_ameva.proto file

```cpp
// Define parameters to send to Unreal
// In this case a string and a float
message PrintHelloWorldParams {
	required string strToPrint = 1;
	required float floatToPrint = 2;
}

message KRAmevaEvent {
  enum FuncToCall {
    ...
    // Define your own event name
    PrintHelloWorld = 2
    ...
  } 
  ....
  // Add parameters in the event, make sure it's optional
  optional PrintHelloWorldParams = 3;
  ...
}
```

2.  Compile the knowrob_ameva.proto file
- Compile in knowrob_ameva (linux)
Put the the knowrob_ameva.proto file in  
`knowrob_ameva/src/proto/` and use catkin_make to compile the package, and the cakin_make will compile the proto file.
- Compile in Unreal Engine (Windows)
	- Use the `\Plugins\UProtobuf\Source\ThirdParty\tools\protoc.exe` to compile the proto file to generate cpp file.
	- Use this command to compile the proto file. `protoc.exe --cpp_out=. knowrob_ameva.proto`
	- Put the generated *.pb.cpp and *.pd.h files in `\USemLog\Source\USemLog\Classes\Knowrob\Proto`
3. Create predicate in knowrob_ameva

```cpp
// Add one parameter to specify which unreal client to send
PREDICATE(ue_hello_world, 3)
{
// Check if the client is still connected to knowrob
if (!check_client_connected((int)A1))
	return FALSE;

// Create a new event
sl_pb::KRAmevaEvent ameva_event;
// Set the event to PrintHelloWorld
ameva_event.set_functocall(ameva_event.PrintHelloWorld);

// Create paramters
sl_pb::PrintHelloWorldParams* print_hello_world_params = ameva_event.mutable_printhelloworldparams();
print_hello_world_params->set_strToPrint((char*)A2);
print_hello_world_params->set_floatToPrint((double)A3);

// Serialize the proto message
std::string proto_str = ameva_event.SerializeAsString();

// Create a request task,and add it to the queue. The websocket will send it to unreal.
RequestTask* task = new  RequestTask((int) A1, proto_str);
queue.push(task);
return TRUE;
}
```

4. Process data in KnowrobManager
- Check the event in `FSLKREventDispatcher::ProcessProtobuf(std::string  ProtoStr)`
	```cpp
	void FSLKREventDispatcher::ProcessProtobuf(std::string ProtoStr)
	{
		sl_pb::KRAmevaEvent AmevaEvent;
		AmevaEvent.ParseFromString(ProtoStr);
		...
		// Check the event
		else if (AmevaEvent.functocall() == AmevaEvent.PrintHelloWorld)
		{
			// Use pre-defined the function and pass the paramters to it  
			CustomHanderForPrintHelloWorld(AmevaEvent.printhelloworldparam());
		}
		...
	```
	
- Get the parameters 

	```cpp	
	void  FSLKREventDispatcher::CustomHanderForPrintHelloWorld(sl_pb::PrintHelloWorldParams  params)
	{
		// Get and log the paramter value and 
		UE_LOG(LogTemp, Warning, TEXT("[ProtoMessage]string: %s"), UTF8_TO_TCHAR(params.strtoprint().c_str()));
		UE_LOG(LogTemp, Warning, TEXT("[ProtoMessage]float: %ld"), params.floattoprint());
	}
	```
