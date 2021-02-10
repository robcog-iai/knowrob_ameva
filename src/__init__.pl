:- module(am_tasks,   
    [
        am_task_stack/1,
        am_check_episode/3
    ]).
:- use_foreign_library('libknowrob_ameva.so').
:- use_module('./am_semantic_map.pl').
:- use_module('./am_episode.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%   Stack cups in the drawer in parallel      %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

am_get_task(Task) :-
    Task = 'SneNouIEZEyd'.

am_get_semantic_map(SM) :-
    SM = 'utJDwYBP8CA'.

am_get_drawer_class(DrawerClass) :- 
    DrawerClass = 'http://knowrob.org/kb/knowrob.owl#IAIDrawerW60H29'.

am_get_cup_class(CupClass) :-
    CupClass = 'http://knowrob.org/kb/knowrob.owl#MPCupSet01'.

am_select_drawer(DrawerClass, DrawerInst) :-
    DrawerInst = 'http://knowrob.org/kb/ameva_log.owl#zVBHGrf9n0qEVqc8aDbF-w'.

am_task_stack(MaxCups) :-
    am_get_task(Task),
    am_get_semantic_map(Map),
    am_load_semantic_map(Map, MapInst),
    am_get_level_name(MapInst,LevelName),

    am_get_drawer_class(DrawerClass),
    am_get_cup_class(CupClass),
    am_select_drawer(DrawerClass, DrawerInst),

    am_get_drawer_capacity(DrawerClass, CupClass, MaxNum),
    am_get_max_batch_size(BatchSize),
    
    am_stack_in_batch(Task, LevelName, 1, MaxNum, BatchSize, MapInst, DrawerInst, CupClass, [], AllEpNames),
    am_create_stack_episode_params(MaxNum, AllEpParams),
    maplist(am_check_stack_episode, AllEpNames, AllEpParams, Results),
    max_list(Results, MaxCups).

am_stack_in_batch(Task, LevelName, EpisodeIdx, TotalEpisode, BatchSize, MapInst, DrawerInst, CupClass, SubEpNames, AllEpNames) :-
    (EpisodeIdx =< TotalEpisode -> Start is EpisodeIdx,
        (TotalEpisode - EpisodeIdx + 1 < BatchSize -> End is TotalEpisode; End is EpisodeIdx + BatchSize -1),
        EpisodeNum is End - Start + 1,
        ag_create_clients(EpisodeNum, LevelName, UEClients),
    
        am_build_param_list(Task, EpisodeNum, Tasks),
        am_create_stack_episode_params(Start, End, EpParams),
        am_create_unique_episode_names(EpParams, EpNames),
        append(SubEpNames, EpNames, NewSubEpNames),

        maplist(ue_start_logging, UEClients, Tasks, EpNames),
          
        am_find_dishwasher(DishWasher),  
        am_find_dishwasher_door(DishWasher, DishWasherDoor),
        am_build_param_list(DishWasherDoor,EpisodeNum,DishWasherDoors),
        maplist(am_pull_objct, UEClients, DishWasherDoors),
        am_find_upper_racket(DishWasher, UpperRacket),
        am_build_param_list(UpperRacket,EpisodeNum,UpperRackets),
        maplist(am_pull_objct, UEClients, UpperRackets),
        am_find_lower_racket(DishWasher, LowerRacket),
        am_build_param_list(LowerRacket, EpisodeNum, LowerRackets),
        maplist(am_pull_objct, UEClients, LowerRackets),

        am_get_push_force(PushForce),
        am_get_simulation_time(Duration), 
        am_build_param_list(MapInst, EpisodeNum, MapInsts),
        am_build_param_list(DrawerInst, EpisodeNum, DrawerInsts),
        am_build_param_list(CupClass, EpisodeNum, CupClasses),
        am_build_param_list(PushForce, EpisodeNum, PushForces),
        am_build_param_list(Duration, EpisodeNum, Durations),
        maplist(am_stack_obj_in_drawer_action, UEClients, MapInsts, DrawerInsts, CupClasses, EpParams, PushForces, Durations),
        ue_wait_simulation(Duration),
        
        maplist(ue_stop_logging, UEClients),
        maplist(ue_get_episode_data,  UEClients, Tasks, EpNames),
        ag_close_clients(UEClients),
        ag_wait_close_clients,

        NextIdx is End + 1,
        am_stack_in_batch(Task, LevelName, NextIdx, TotalEpisode, BatchSize, MapInst, DrawerInst, CupClass, NewSubEpNames, AllEpNames)
        ; 
        AllEpNames = SubEpNames,
        true
    ).

% check how many cups can be stacked in the drawer
am_get_drawer_capacity(DrawerClass, CupClass, MaxNum) :-
    am_get_height(DrawerClass, DrawerHeight),
    am_get_height(CupClass, CupHeight),
    Factor is DrawerHeight / CupHeight,
    MaxNum is floor(Factor).

% stack cups on the drawer
am_stack_obj_in_drawer_action(UEClient, MapInst, DrawerInst, ObjClass, ObjNum, PushForce, SimTime) :-
    am_get_individual_list(ObjClass, MapInst, ObjList),
    \+am_stack_up_on(UEClient, MapInst, DrawerInst, ObjList, 0, ObjNum),
    am_get_id(DrawerInst, DrawerId),
    ue_start_simulation(UEClient, [DrawerId], SimTime),
    ue_apply_force_to(UEClient, DrawerId, PushForce, 0, 0).

% stack a list of objects on top of a base objects 
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

%check if stack episode work
am_check_stack_episode(EpName, EpParam, Result) :-
    am_load_episode(EpName, EpInst),
    EventType = 'http://knowrob.org/kb/knowrob.owl#TouchingSituation',
    DrawerInst = 'http://knowrob.org/kb/ameva_log.owl#zVBHGrf9n0qEVqc8aDbF-w',
    findall(EventInst, 
        (
            am_occurs(EpInst, EventInst, EventType),
            triple(EventInst, knowrob:inContact, DrawerInst)
        ), EvtList),
    length(EvtList, Count),
    (Count =:= 1 -> Result is EpParam; Result is 0 ).

am_find_dishwasher(DishWasher) :-
    DishWasher = '###'.

am_find_dishwasher_door(DishWasher, DishWasherDoor) :-
    DishWasherDoor = 'http://knowrob.org/kb/ameva_log.owl#zoAfiCIbi0KJP-fecLJkoQ'.

am_find_upper_racket(DishWasher, Racket) :-
    Racket = 'http://knowrob.org/kb/ameva_log.owl#61nqVeqryECsFlyF5fjcBQ'.

am_find_lower_racket(DishWasher, Racket) :-
    Racket = 'http://knowrob.org/kb/ameva_log.owl#Dkkpmdu-L0Ssc3R5YJTftw'.

am_pull_objct(Client, Obj) :-
    am_get_id(Obj, Id),
    ue_apply_force_to(Client, Id, 10, 0, 0),
    sleep(2).

% get the id of the individual
am_get_id(Individual, Id) :-
    split_string(Individual, "#", "", StrList),
    nth1(2, StrList, Id).

am_create_stack_episode_params(Start, End, EpParams) :-
    findall(Num, between(Start, End, Num), EpParams).

am_create_stack_episode_params(MaxCupNum, EpParams) :-
    findall(Num, between(1, MaxCupNum, Num), EpParams).

am_create_unique_episode_names(EpParams, EpNames) :-
    maplist(am_create_unique_name, EpParams,EpNames).

am_create_unique_name(Param, Name) :-
    uuid(X),
    atom_concat(Param, X, Name).

ue_wait_simulation(Duration) :-
    SleepTime is Duration +  1,  
    sleep(SleepTime).

ag_wait_close_clients :-
    sleep(50).

am_get_push_force(Force) :-
    Force is 23.

am_get_simulation_time(Duration) :-
    Duration is 6.

am_build_param_list(Obj, Num, List) :-
    length(List, Num), 
    maplist(=(Obj), List).

am_get_max_batch_size(BatchSize)  :-
    BatchSize is 2.
