:- module(am_tasks,
    [
        am_get_drawer_stack_max/18,
        am_stack_in_drawer/13
    ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%   Stack cups in the drawer in parallel      %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% am_stack_in_drawer(+Client, +Map, +Task, +Episode, +StackObjClass, +ObjNum, +DrawerInst, +PushX, +PushY, +PushZ, +DrawerOffsetX, +DrawerOffsetY, +DrawerOffsetZ)
% Client - client id
% Map - semantic map name
% Task - define task name
% Episode - define episode name
% ObjectToStackClass - the class of the object to be stacked
% ObjNum - the number of object to stack
% DrawerInst - the drawer individual
% PushX - push force vector x value; push the drawer
% PushY - push force vector y value; push the drawer
% PushZ - push force vector z value; push the drawer
%
am_stack_in_drawer(Client, Map, Task, Episode, StackObjClass, ObjNum, DrawerInst, PushX, PushY, PushZ, DrawerOffsetX, DrawerOffsetY, DrawerOffsetZ) :-
    am_load_semantic_map(Map, MapInst),
    writeln(MapInst),
    ue_start_logging(Client, Task, Episode),
    sleep(2),
    DrawerPullX is DrawerOffsetX * 0.5,
    DrawerPullY is DrawerOffsetY * 0.5,
    DrawerPullZ is DrawerOffsetZ * 0.5,
    am_get_id(DrawerInst, DrawerId),
    ue_apply_force_to(Client, DrawerId, DrawerPullX, DrawerPullY, DrawerPullZ),
    sleep(10),
    am_stack_obj_in_drawer_action(Client, MapInst, DrawerInst, DrawerOffsetX, DrawerOffsetY, DrawerOffsetZ, StackObjClass, ObjNum, PushX, PushY, PushZ),
    sleep(10),
    ue_stop_logging(Client),
    ue_get_episode_data(Client,Task, Episode).

% am_get_drawer_stack_max(-MaxStacked, +Task, +Map, +StackObjClass, +DrawerInst, +PushX, +PushY, +PushZ, +DrawerOffsetX, +DrawerOffsetY, +DrawerOffsetZ, +DWDoorInst, +DWUpperRackInst, +DWLowerRackInst, +PullX, +PullY, +PullZ)
% MaxStacked - return value; the maximum number of objects can be stacked 
% Task - define task name
% Map - semantic map name
% StackObjClass - the class of the object to be stacked
% ObjNum - the number of object to stack
% DrawerInst - the drawer individual
% PushX - push force vector x value; push the drawer
% PushY - push force vector y value; push the drawer
% PushZ - push force vector z value; push the drawer
% DWDoorInst - the dishwasher door individual
% DWUpperRackInst - the upper racket individual in the dishwasher
% DWLowerRackInst - the lower racket individual in the dishwasher
% PullX - pull force vector x value; open the dishwasher
% PullY - pull force vector y value; open the dishwasher
% PullZ - pull force vector z value; open the dishwasher
% BatchSize - the number of simulation in parallel
%
am_get_drawer_stack_max(MaxStacked, Task, Map, StackObjClass, DrawerInst, DrawerForceX, DrawerForceY, DrawerForceZ, DrawerOffsetX, DrawerOffsetY, DrawerOffsetZ, DWDoorInst, DWUpperRackInst, DWLowerRackInst, DWForceX, DWForceY, DWForceZ, BatchSize) :-
    am_load_semantic_map(Map, MapInst),
    am_get_level_name(MapInst,LevelName),

    instance_of(DrawerInst, DrawerClass),

    am_get_drawer_capacity(DrawerClass, StackObjClass, Capacity),
    am_get_individual_num(StackObjClass, MapInst, TotalObjNum),
    (Capacity > TotalObjNum -> MaxNum is TotalObjNum; MaxNum is Capacity),

    am_stack_in_batch(Task, LevelName, 1, MaxNum, BatchSize, MapInst, DrawerInst, StackObjClass, DrawerForceX, DrawerForceY, DrawerForceZ, DrawerOffsetX, DrawerOffsetY, DrawerOffsetZ, DWDoorInst, DWUpperRackInst, DWLowerRackInst, DWForceX, DWForceY, DWForceZ, [], AllEpNames),   

    am_create_stack_episode_params(MaxNum, AllEpParams),
    am_build_param_list(DrawerInst, MaxNum, DrawerInsts),
    maplist(am_check_stack_episode, AllEpNames, AllEpParams, DrawerInsts, Results),
    max_list(Results, MaxStacked).

%% am_stack_in_batch(+Task, +LevelName, +EpIdx, +EpTotal, +BatchSize, +MapInst, +DrawerInst, +StackObjClass, +PushX, +PushY, +PushZ, +DrawerOffsetX, +DrawerOffsetY, +DrawerOffsetZ, +DWDoorInst, +DWUpperRackInst, +DWLowerRackInst, +PullX, +PullY, +PullZ, +SubEpNames, +AllEpNames) 
% am_stack_in_batch(Task, LevelName, EpIdx, EpTotal, BatchSize, MapInst, DrawerInst, StackObjClass, PushX, PushY, PushZ, DrawerOffsetX, DrawerOffsetY, DrawerOffsetZ, DWDoorInst, DWUpperRackInst, DWLowerRackInst, PullX, PullY, PullZ, SubEpNames, AllEpNames) :-
%     (EpIdx =< EpTotal -> Start is EpIdx,
%         (EpTotal - EpIdx + 1 < BatchSize -> End is EpTotal; End is EpIdx + BatchSize -1),
%         EpisodeNum is End - Start + 1,
%         ag_create_clients(EpisodeNum, LevelName, UEClients),
    
%         am_build_param_list(Task, EpisodeNum, Tasks),
%         am_create_stack_episode_params(Start, End, EpParams),
%         am_create_unique_episode_names(EpParams, EpNames),
%         append(SubEpNames, EpNames, NewSubEpNames),

%         % start logging
%         maplist(ue_start_logging, UEClients, Tasks, EpNames),

%         % open the drawer
%         DrawerPullX is DrawerOffsetX * 0.5,
%         DrawerPullY is DrawerOffsetY * 0.5,
%         DrawerPullZ is DrawerOffsetZ * 0.5,
%         am_get_id(DrawerInst, DrawerId),
%         am_build_param_list(DrawerId, EpisodeNum, DrawerIds), 
%         am_build_param_list(DrawerPullX, EpisodeNum, DrawerPullXs), 
%         am_build_param_list(DrawerPullY, EpisodeNum, DrawerPullYs), 
%         am_build_param_list(DrawerPullZ, EpisodeNum, DrawerPullZs),
%         maplist(ue_apply_force_to, UEClients, DrawerIds, DrawerPullXs, DrawerPullYs, DrawerPullZs),
%         sleep(10),
%         % pull the dishwasher
%         am_build_param_list(PullX, EpisodeNum, PullXs), 
%         am_build_param_list(PullY, EpisodeNum, PullYs), 
%         am_build_param_list(PullZ, EpisodeNum, PullZs), 
%         am_build_param_list(DWLowerRackInst, EpisodeNum, DWLowerRackInsts),
%         am_build_param_list(DWUpperRackInst,EpisodeNum,DWUpperRackInsts),
%         am_build_param_list(DWDoorInst,EpisodeNum,DWDoorInsts),
%         maplist(am_pull_objct, UEClients, DWDoorInsts, PullXs, PullYs, PullZs),
%         maplist(am_pull_objct, UEClients, DWUpperRackInsts, PullXs, PullYs, PullZs),
%         maplist(am_pull_objct, UEClients, DWLowerRackInsts, PullXs, PullYs, PullZs),
        
%         % stack objects in the drawer
%         am_get_simulation_time(Duration), 
%         am_build_param_list(MapInst, EpisodeNum, MapInsts),
%         am_build_param_list(DrawerInst, EpisodeNum, DrawerInsts),
%         am_build_param_list(StackObjClass, EpisodeNum, StackObjClasses),
%         am_build_param_list(PushX, EpisodeNum, PushXs),
%         am_build_param_list(PushY, EpisodeNum, PushYs),
%         am_build_param_list(PushZ, EpisodeNum, PushZs),
%         am_build_param_list(DrawerOffsetX, EpisodeNum, DrawerOffsetXs),
%         am_build_param_list(DrawerOffsetY, EpisodeNum, DrawerOffsetYs),
%         am_build_param_list(DrawerOffsetZ, EpisodeNum, DrawerOffsetZs),
%         maplist(am_stack_obj_in_drawer_action, UEClients, MapInsts, DrawerInsts, DrawerOffsetXs, DrawerOffsetYs, DrawerOffsetZs, StackObjClasses, EpParams, PushXs, PushYs, PushZs),
%         ue_wait_simulation(Duration),
        
%         maplist(ue_stop_logging, UEClients),
%         maplist(ue_get_episode_data,  UEClients, Tasks, EpNames),
%         ag_close_clients(UEClients),
%         ag_wait_close_clients,

%         NextIdx is End + 1,
%         am_stack_in_batch(Task, LevelName, NextIdx, EpTotal, BatchSize, MapInst, DrawerInst, StackObjClass, PushX, PushY, PushZ, DrawerOffsetX, DrawerOffsetY, DrawerOffsetZ, DWDoorInst, DWUpperRackInst, DWLowerRackInst, PullX, PullY, PullZ, NewSubEpNames, AllEpNames)
%         ; 
%         AllEpNames = SubEpNames,
%         true
%     ).

am_stack_in_batch(Task, LevelName, EpIdx, EpTotal, BatchSize, MapInst, DrawerInst, StackObjClass, DrawerForceX, DrawerForceY, DrawerForceZ, DrawerOffsetX, DrawerOffsetY, DrawerOffsetZ, DWDoorInst, DWUpperRackInst, DWLowerRackInst, DWForceX, DWForceY, DWForceZ, SubEpNames, AllEpNames) :-
    (EpIdx =< EpTotal -> Start is EpIdx,
        (EpTotal - EpIdx + 1 < BatchSize -> End is EpTotal; End is EpIdx + BatchSize -1),
        EpisodeNum is End - Start + 1,
        ag_create_clients(EpisodeNum, LevelName, UEClients),
    
        am_build_param_list(Task, EpisodeNum, Tasks),
        am_create_stack_episode_params(Start, End, EpParams),
        am_create_unique_episode_names(EpParams, EpNames),
        append(SubEpNames, EpNames, NewSubEpNames),

        % start ue loggers
        maplist(ue_start_logging, UEClients, Tasks, EpNames),
 

        %%%
        % set dishwasher force mappings
        am_build_param_list(DWForceX, EpisodeNum, DWForceXs),
        am_build_param_list(DWForceY, EpisodeNum, DWForceYs), 
        am_build_param_list(DWForceZ, EpisodeNum, DWForceZs),
        SmallDWForceX is DWForceX * 0.4,
        SmallDWForceY is DWForceY * 0.4,
        SmallDWForceZ is DWForceZ * 0.4,
        am_build_param_list(SmallDWForceX, EpisodeNum, SmallDWForceXs),
        am_build_param_list(SmallDWForceY, EpisodeNum, SmallDWForceYs),
        am_build_param_list(SmallDWForceZ, EpisodeNum, SmallDWForceZs),
        
        % open diswhasher door
        am_get_id(DWDoorInst, DWDoorId),
        am_build_param_list(DWDoorId, EpisodeNum, DWDoorIds),
        maplist(ue_apply_force_to, UEClients, DWDoorIds, DWForceXs, DWForceYs, DWForceZs),
        sleep(2),

        % pull upper rack
        am_get_id(DWUpperRackInst, DWUpperRackId),
        am_build_param_list(DWUpperRackId, EpisodeNum, DWUpperRackIds),
        maplist(ue_apply_force_to, UEClients, DWUpperRackIds, SmallDWForceXs, SmallDWForceYs, SmallDWForceZs),
        sleep(0.2),
        maplist(ue_apply_force_to, UEClients, DWUpperRackIds, SmallDWForceXs, SmallDWForceYs, SmallDWForceZs),
        sleep(0.2),
        maplist(ue_apply_force_to, UEClients, DWUpperRackIds, SmallDWForceXs, SmallDWForceYs, SmallDWForceZs),
        sleep(0.2),
        maplist(ue_apply_force_to, UEClients, DWUpperRackIds, SmallDWForceXs, SmallDWForceYs, SmallDWForceZs),
        sleep(1),

        % pull lower rack
        am_get_id(DWLowerRackInst, DWLowerRackId),
        am_build_param_list(DWLowerRackId, EpisodeNum, DWLowerRackIds),
        maplist(ue_apply_force_to, UEClients, DWLowerRackIds, SmallDWForceXs, SmallDWForceYs, SmallDWForceZs),
        sleep(0.2),
        maplist(ue_apply_force_to, UEClients, DWLowerRackIds, SmallDWForceXs, SmallDWForceYs, SmallDWForceZs),
        sleep(0.2),
        maplist(ue_apply_force_to, UEClients, DWLowerRackIds, SmallDWForceXs, SmallDWForceYs, SmallDWForceZs),
        sleep(0.2),
        maplist(ue_apply_force_to, UEClients, DWLowerRackIds, SmallDWForceXs, SmallDWForceYs, SmallDWForceZs),
        sleep(0.2),
        maplist(ue_apply_force_to, UEClients, DWLowerRackIds, SmallDWForceXs, SmallDWForceYs, SmallDWForceZs),
        sleep(0.2),
        maplist(ue_apply_force_to, UEClients, DWLowerRackIds, SmallDWForceXs, SmallDWForceYs, SmallDWForceZs),
        sleep(0.2),
        maplist(ue_apply_force_to, UEClients, DWLowerRackIds, SmallDWForceXs, SmallDWForceYs, SmallDWForceZs),
        sleep(0.2),
        maplist(ue_apply_force_to, UEClients, DWLowerRackIds, SmallDWForceXs, SmallDWForceYs, SmallDWForceZs),
        sleep(0.2),
        maplist(ue_apply_force_to, UEClients, DWLowerRackIds, SmallDWForceXs, SmallDWForceYs, SmallDWForceZs),
        sleep(5),


        %%%
        % set drawer open force mappings
        am_build_param_list(DrawerForceX, EpisodeNum, DrawerForceXs),
        am_build_param_list(DrawerForceY, EpisodeNum, DrawerForceYs),
        am_build_param_list(DrawerForceZ, EpisodeNum, DrawerForceZs),
        NegDrawerForceX is -0.35 * DrawerForceX,
        NegDrawerForceY is -0.35 * DrawerForceY,
        NegDrawerForceZ is -0.35 * DrawerForceZ,
        am_build_param_list(NegDrawerForceX, EpisodeNum, NegDrawerForceXs), 
        am_build_param_list(NegDrawerForceY, EpisodeNum, NegDrawerForceYs), 
        am_build_param_list(NegDrawerForceZ, EpisodeNum, NegDrawerForceZs),

        % open the drawer
        am_get_id(DrawerInst, DrawerId),
        am_build_param_list(DrawerId, EpisodeNum, DrawerIds), 
        maplist(ue_apply_force_to, UEClients, DrawerIds, DrawerForceXs, DrawerForceYs, DrawerForceZs),
        sleep(5),


        %%%
        % set stack location offset mappings
        am_build_param_list(DrawerOffsetX, EpisodeNum, DrawerOffsetXs),
        am_build_param_list(DrawerOffsetY, EpisodeNum, DrawerOffsetYs),
        am_build_param_list(DrawerOffsetZ, EpisodeNum, DrawerOffsetZs),

        % set objects to stack mappings
        am_build_param_list(MapInst, EpisodeNum, MapInsts),
        am_build_param_list(DrawerInst, EpisodeNum, DrawerInsts),
        am_build_param_list(StackObjClass, EpisodeNum, StackObjClasses),

        % stack items and close the drawer
        maplist(am_stack_obj_in_drawer_action, UEClients, MapInsts, DrawerInsts, DrawerOffsetXs, DrawerOffsetYs, DrawerOffsetZs, StackObjClasses, EpParams, NegDrawerForceXs, NegDrawerForceYs, NegDrawerForceZs),
        sleep(7),


        %%%
        % re-open the drawer
        maplist(ue_apply_force_to, UEClients, DrawerIds, DrawerForceXs, DrawerForceYs, DrawerForceZs),
        sleep(2),


        %%%
        % wait for the simulation to settle
        %am_get_simulation_time(Duration), 
        %ue_wait_simulation(Duration),
        sleep(10),


        % stop logger, get the data, close current clients
        maplist(ue_stop_logging, UEClients),
        sleep(2),
        maplist(ue_get_episode_data,  UEClients, Tasks, EpNames),
        sleep(2),
        ag_close_clients(UEClients),
        %ag_wait_close_clients,
        sleep(30),

        % continue with the next batch
        NextIdx is End + 1,
        am_stack_in_batch(Task, LevelName, NextIdx, EpTotal, BatchSize, MapInst, DrawerInst, StackObjClass, DrawerForceX, DrawerForceY, DrawerForceZ, DrawerOffsetX, DrawerOffsetY, DrawerOffsetZ, DWDoorInst, DWUpperRackInst, DWLowerRackInst, DWForceX, DWForceY, DWForceZ, NewSubEpNames, AllEpNames)
        ; 
        AllEpNames = SubEpNames,
        true
    ).

% check how many cups can be stacked in the drawer
% am_get_drawer_capacity(+DrawerClass, +StackObjClass, -MaxNum) 
%
am_get_drawer_capacity(DrawerClass, StackObjClass, MaxNum) :-
    am_get_height(DrawerClass, DrawerHeight),
    am_get_height(StackObjClass, CupHeight),
    Factor is DrawerHeight / CupHeight,
    MaxNum is floor(Factor).

% stack cups on the drawer
% am_stack_obj_in_drawer_action(+UEClient, +MapInst, +DrawerInst, +DrawerOffsetX, +DrawerOffsetY, +DrawerOffsetZ, +ObjClass, +ObjNum, +PushX, +PushY, +PushZ)
%
am_stack_obj_in_drawer_action(UEClient, MapInst, DrawerInst, DrawerOffsetX, DrawerOffsetY, DrawerOffsetZ, ObjClass, ObjNum, PushX, PushY, PushZ) :-
    am_get_individual_list(ObjClass, MapInst, ObjList),
    \+am_stack_up_on(UEClient, MapInst, DrawerInst, DrawerOffsetX, DrawerOffsetY, DrawerOffsetZ, ObjList, 0, ObjNum),
    am_get_id(DrawerInst, DrawerId),
    ue_apply_force_to(UEClient, DrawerId, PushX, PushY, PushZ).


% stack a list of objects on top of a base objects 
% am_stack_up_on(+UEClient, +MapInst, +Base, +BaseOffsetX, +BaseOffsetY, +BaseOffsetZ, +ObjList, +Index, +Count)
%
am_stack_up_on(UEClient, MapInst, Base, BaseOffsetX, BaseOffsetY, BaseOffsetZ, ObjList, Index, Count) :-
    Index < Count,
    nth0(Index, ObjList, Obj),
    am_get_pose(MapInst, Base, BaseX, BaseY, BaseZ, _BaseQX, _BaseQY, _BaseQZ, _BaseQW),
    NewBaseX is BaseX + BaseOffsetX,
    NewBaseY is BaseY + BaseOffsetY,
    NewBaseZ is BaseZ + BaseOffsetZ,
    instance_of(Base, BaseClass),
    instance_of(Obj, ObjClass),
    am_get_height(BaseClass, BaseHeight),
    am_get_height(ObjClass, ObjHeight),
    NewZ is NewBaseZ - 0.2 * BaseHeight + 1.01 * ObjHeight * Index,
    am_get_id(Obj, ObjId),
    writeln(Obj),
    % ue_set_individual_pose(UEClient, ObjId, NewBaseX, NewBaseY, NewZ, 0, 0, 0, 1),
    % 90 deg rotate on Z axis
    ue_set_individual_pose(UEClient, ObjId, NewBaseX, NewBaseY, NewZ, 0, 0, 0.707, 0.707),
    % uside down and 90 deg rot on Z
    %ue_set_individual_pose(UEClient, ObjId, NewBaseX, NewBaseY, NewZ, 0.707, -0.707, 0, 0),
    sleep(1),
    ue_start_simulation(UEClient, [ObjId], -1),
    sleep(1),
    N is Index + 1,
    am_stack_up_on(UEClient, MapInst, Base, BaseOffsetX, BaseOffsetY, BaseOffsetZ, ObjList, N, Count).

% check if stack episode work
% am_check_stack_episode(+EpName, +EpParam, +DrawerInst, -Result)
%
am_check_stack_episode(EpName, EpParam, DrawerInst, Result) :-
    am_load_episode(EpName, EpInst),
    EventType = 'http://knowrob.org/kb/knowrob.owl#TouchingSituation',
    findall(EventInst, 
        (
            am_occurs(EpInst, EventInst, EventType),
            triple(EventInst, knowrob:inContact, DrawerInst)
        ), EvtList),
    length(EvtList, Count),
    (Count =:= 1 -> Result is EpParam; Result is 0 ).

% am_pull_objct(+Client, +Obj, +PullX, +PullY, +PullZ) 
%
am_pull_objct(Client, Obj, PullX, PullY, PullZ) :-
    am_get_id(Obj, Id),
    ue_apply_force_to(Client, Id, PullX, PullY, PullZ),
    sleep(2).

% get the id of the individual
% am_get_id(+Individual, -Id)
%
am_get_id(Individual, Id) :-
    split_string(Individual, "#", "", StrList),
    nth1(2, StrList, Id).

% am_create_stack_episode_params(+Start, +End, -EpParams)
%
am_create_stack_episode_params(Start, End, EpParams) :-
    findall(Num, between(Start, End, Num), EpParams).

% am_create_stack_episode_params(+MaxCupNum, -EpParams) 
%
am_create_stack_episode_params(MaxCupNum, EpParams) :-
    findall(Num, between(1, MaxCupNum, Num), EpParams).

% am_create_unique_episode_names(+EpParams, -EpNames) 
%
am_create_unique_episode_names(EpParams, EpNames) :-
    maplist(am_create_unique_name, EpParams,EpNames).

% am_create_unique_name(+Param, -Name) 
%
am_create_unique_name(Param, Name) :-
    uuid(X),
    atom_concat(Param, X, Name).

% am_build_param_list(+Obj, +Num, -List)
%
am_build_param_list(Obj, Num, List) :-
    length(List, Num), 
    maplist(=(Obj), List).

ue_wait_simulation(Duration) :-
    SleepTime is Duration +  1,  
    sleep(SleepTime).

ag_wait_close_clients :-
    sleep(80).

am_get_simulation_time(Duration) :-
    Duration is 10.
