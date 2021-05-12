:- module(am_tasks,   
    [
        am_get_drawer_stack_max/15,
        am_stack_in_drawer/10
    ]).
:- use_foreign_library('libknowrob_ameva.so').
:- use_module('./am_semantic_map.pl').
:- use_module('./am_episode.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%   Stack cups in the drawer in parallel      %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% am_stack_in_drawer(+Client, +Map, +Task, +Episode, +OjectToStackClass, +ObjNum, +DrawerInst, +PushX, +PushY, +PushZ)
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
am_stack_in_drawer(Client, Map, Task, Episode, OjectToStackClass, ObjNum, DrawerInst, PushX, PushY, PushZ) :-
    ue_start_logging(Client, Task, Episode),
    am_load_semantic_map(Map, MapInst),
    sleep(2),
    am_stack_obj_in_drawer_action(Client, MapInst, DrawerInst, OjectToStackClass, ObjNum, PushX, PushY, PushZ),
    sleep(10),
    ue_stop_logging(Client),
    ue_get_episode_data(Client,Task, Episode).

% am_get_drawer_stack_max(-MaxNumToStack, +Task, +Map, +OjectToStackClass, +DrawerInst, +PushX, +PushY, +PushZ, +DishWasherDoor, +UpperRacket, +LowerRacket, +PullX, +PullY, +PullZ)
% MaxNumToStack - return value; the maximum number of objects can be stacked 
% Map - semantic map name
% Task - define task name
% ObjectToStackClass - the class of the object to be stacked
% ObjNum - the number of object to stack
% DrawerInst - the drawer individual
% PushX - push force vector x value; push the drawer
% PushY - push force vector y value; push the drawer
% PushZ - push force vector z value; push the drawer
% DishWasherDoor - the dishwasher door individual
% UpperRacket - the upper racket individual in the dishwasher
% LowerRacket - the lower racket individual in the dishwasher
% PullX - pull force vector x value; open the dishwasher
% PullY - pull force vector y value; open the dishwasher
% PullZ - pull force vector z value; open the dishwasher
% BatchSize - the number of simulation in parallel
%
am_get_drawer_stack_max(MaxNumToStack, Task, Map, OjectToStackClass, DrawerInst, PushX, PushY, PushZ, DishWasherDoor, UpperRacket, LowerRacket, PullX, PullY, PullZ, BatchSize) :-
    am_load_semantic_map(Map, MapInst),
    am_get_level_name(MapInst,LevelName),

    instance_of(DrawerInst, DrawerClass),

    am_get_drawer_capacity(DrawerClass, OjectToStackClass, MaxNum),
    
    am_stack_in_batch(Task, LevelName, 1, MaxNum, BatchSize, MapInst, DrawerInst, OjectToStackClass, PushX, PushY, PushZ, DishWasherDoor, UpperRacket, LowerRacket, PullX, PullY, PullZ, [], AllEpNames),   
    am_create_stack_episode_params(MaxNum, AllEpParams),
    am_build_param_list(DrawerInst, MaxNum, DrawerInsts),
    maplist(am_check_stack_episode, AllEpNames, AllEpParams, DrawerInsts, Results),
    max_list(Results, MaxNumToStack).

% am_stack_in_batch(+Task, +LevelName, +EpisodeIdx, +TotalEpisode, +BatchSize, +MapInst, +DrawerInst, +OjectToStackClass, +PushX, +PushY, +PushZ, +DishWasherDoor, +UpperRacket, +LowerRacket, +PullX, +PullY, +PullZ, +SubEpNames, +AllEpNames) 
%
am_stack_in_batch(Task, LevelName, EpisodeIdx, TotalEpisode, BatchSize, MapInst, DrawerInst, OjectToStackClass, PushX, PushY, PushZ, DishWasherDoor, UpperRacket, LowerRacket, PullX, PullY, PullZ, SubEpNames, AllEpNames) :-
    (EpisodeIdx =< TotalEpisode -> Start is EpisodeIdx,
        (TotalEpisode - EpisodeIdx + 1 < BatchSize -> End is TotalEpisode; End is EpisodeIdx + BatchSize -1),
        EpisodeNum is End - Start + 1,
        ag_create_clients(EpisodeNum, LevelName, UEClients),
    
        am_build_param_list(Task, EpisodeNum, Tasks),
        am_create_stack_episode_params(Start, End, EpParams),
        am_create_unique_episode_names(EpParams, EpNames),
        append(SubEpNames, EpNames, NewSubEpNames),

        % start logging
        maplist(ue_start_logging, UEClients, Tasks, EpNames),

        % pull the dishwasher
        am_build_param_list(PullX, EpisodeNum, PullXs), 
        am_build_param_list(PullY, EpisodeNum, PullYs), 
        am_build_param_list(PullZ, EpisodeNum, PullZs), 
        am_build_param_list(LowerRacket, EpisodeNum, LowerRackets),
        am_build_param_list(UpperRacket,EpisodeNum,UpperRackets),
        am_build_param_list(DishWasherDoor,EpisodeNum,DishWasherDoors),
        maplist(am_pull_objct, UEClients, DishWasherDoors, PullXs, PullYs, PullZs),
        maplist(am_pull_objct, UEClients, UpperRackets, PullXs, PullYs, PullZs),
        maplist(am_pull_objct, UEClients, LowerRackets, PullXs, PullYs, PullZs),
        
        % stack objects in the drawer
        am_get_simulation_time(Duration), 
        am_build_param_list(MapInst, EpisodeNum, MapInsts),
        am_build_param_list(DrawerInst, EpisodeNum, DrawerInsts),
        am_build_param_list(OjectToStackClass, EpisodeNum, OjectToStackClasses),
        am_build_param_list(PushX, EpisodeNum, PushXs),
        am_build_param_list(PushY, EpisodeNum, PushYs),
        am_build_param_list(PushZ, EpisodeNum, PushZs),
        maplist(am_stack_obj_in_drawer_action, UEClients, MapInsts, DrawerInsts, OjectToStackClasses, EpParams, PushXs, PushYs, PushZs),
        ue_wait_simulation(Duration),
        
        maplist(ue_stop_logging, UEClients),
        maplist(ue_get_episode_data,  UEClients, Tasks, EpNames),
        ag_close_clients(UEClients),
        ag_wait_close_clients,

        NextIdx is End + 1,
        am_stack_in_batch(Task, LevelName, NextIdx, TotalEpisode, BatchSize, MapInst, DrawerInst, OjectToStackClass, PushX, PushY, PushZ, DishWasherDoor, UpperRacket, LowerRacket, PullX, PullY, PullZ, NewSubEpNames, AllEpNames)
        ; 
        AllEpNames = SubEpNames,
        true
    ).

% check how many cups can be stacked in the drawer
% am_get_drawer_capacity(+DrawerClass, +OjectToStackClass, -MaxNum) 
%
am_get_drawer_capacity(DrawerClass, OjectToStackClass, MaxNum) :-
    am_get_height(DrawerClass, DrawerHeight),
    am_get_height(OjectToStackClass, CupHeight),
    Factor is DrawerHeight / CupHeight,
    MaxNum is floor(Factor).

% stack cups on the drawer
% am_stack_obj_in_drawer_action(+UEClient, +MapInst, +DrawerInst, +ObjClass, +ObjNum, +PushX, +PushY, +PushZ)
%
am_stack_obj_in_drawer_action(UEClient, MapInst, DrawerInst, ObjClass, ObjNum, PushX, PushY, PushZ) :-
    am_get_individual_list(ObjClass, MapInst, ObjList),
    \+am_stack_up_on(UEClient, MapInst, DrawerInst, ObjList, 0, ObjNum),
    am_get_id(DrawerInst, DrawerId),
    ue_apply_force_to(UEClient, DrawerId, PushX, PushY, PushZ).

% stack a list of objects on top of a base objects 
% am_stack_up_on(+UEClient, +MapInst, +Base, +ObjList, +Index, +Count)
%
am_stack_up_on(UEClient, MapInst, Base, ObjList, Index, Count) :-
    Index < Count,
    nth0(Index, ObjList, Obj),
    am_get_pose(MapInst, Base, BaseX, BaseY, BaseZ, BaseQX, BaseQY, BaseQZ, BaseQW),
    instance_of(Base, BaseClass),
    instance_of(Obj, ObjClass),
    am_get_height(BaseClass, BaseHeight),
    am_get_height(ObjClass, ObjHeight),
    NewZ is BaseZ - 0.2 * BaseHeight + 1.1 * ObjHeight * Index,
    am_get_id(Obj, ObjId),
    ue_set_individual_pose(UEClient, ObjId, BaseX, BaseY, NewZ, 0, 0, 0, 1),
    sleep(1),
    ue_start_simulation(UEClient, [ObjId], -1),
    sleep(1),
    N is Index + 1,
    am_stack_up_on(UEClient, MapInst, Base, ObjList, N, Count).

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
    sleep(50).

am_get_simulation_time(Duration) :-
    Duration is 10.
