:- module(ameva_scenario,   
    [
        stack_sim/1,
        check_stack_episode/3,
        create_stack_episode_params/2,
        create_stack_episode_names/2,
        episodes_param_to_name/2,
        build_param_list/3,
        re/1
    ]).
:- use_foreign_library('libknowrob_ameva.so').
:- use_module('./ue_ameva_semantic_map.pl').
:- use_module('./ue_ameva_episode.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%   Stack cups in the drawer in parallel      %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stack_sim(MaxCups) :-
    Task = 'TaskStack',
    DrawerClass = 'http://knowrob.org/kb/knowrob.owl#IAIDrawerW60H29',
    CupClass = 'http://knowrob.org/kb/knowrob.owl#MPCupSet01',
    DrawerInst = 'http://knowrob.org/kb/Experiment.owl#zVBHGrf9n0qEVqc8aDbF-w',
    load_ameva_semantic_map('TaskStack', MapInst),

    get_stack_capacity(DrawerClass, CupClass, MaxNum),
    get_max_batch_size(BatchSize),
 
    stack_simulation_in_batch(Task, 1, MaxNum, BatchSize, MapInst, DrawerInst, CupClass),
    
    create_stack_episode_params(MaxNum, AllEpParams),
    create_stack_episode_names(AllEpParams, AllEpNames),
    maplist(check_stack_episode, AllEpNames, AllEpParams, Results),
    max_list(Results, MaxCups).


stack_simulation_in_batch(Task, EpisodeIdx, TotalEpisode, BatchSize, MapInst, DrawerInst, CupClass) :-
    (EpisodeIdx =< TotalEpisode -> Start is EpisodeIdx,
        (TotalEpisode - EpisodeIdx + 1 < BatchSize -> End is TotalEpisode; End is EpisodeIdx + BatchSize -1),
        EpisodeNum is End - Start + 1,
        ag_create_clients(EpisodeNum, UEClients),
    
        build_param_list(Task, EpisodeNum, Tasks),
        create_stack_episode_params(Start, End, EpParams),
        create_stack_episode_names(EpParams, EpNames),
        maplist(ue_start_loggers, UEClients, Tasks, EpNames),
       
        get_push_force(PushForce),
        get_simulation_time(Duration), 
        build_param_list(MapInst, EpisodeNum, MapInsts),
        build_param_list(DrawerInst, EpisodeNum, DrawerInsts),
        build_param_list(CupClass, EpisodeNum, CupClasses),
        build_param_list(PushForce, EpisodeNum, PushForces),
        build_param_list(Duration, EpisodeNum, Durations),
        maplist(stack_cup_in_drawer_action, UEClients, MapInsts, DrawerInsts, CupClasses, EpParams, PushForces, Durations),
        ue_wait_simulation(Duration),
        
        maplist(ue_stop_loggers, UEClients),
        maplist(ue_get_episode_data,  UEClients, Tasks, EpNames),
        ag_close_clients(UEClients),
        ag_wait_close_clients,

        NextIdx is End + 1,
        stack_simulation_in_batch(Task, NextIdx, TotalEpisode, BatchSize, MapInst, DrawerInst, CupClass)
        ; true
    ).


% check how many cups can be stacked in the drawer
get_stack_capacity(DrawerClass, CupClass, MaxNum) :-
    get_height(DrawerClass, DrawerHeight),
    get_height(CupClass, CupHeight),
    Factor is DrawerHeight / CupHeight,
    MaxNum is floor(Factor).

% stack cups on the drawer
stack_cup_in_drawer_action(UEClient, MapInst, DrawerInst, CupClass, CupNum, PushForce, SimTime) :-
    get_individual_list(CupClass, MapInst, CupList),
    \+stack_up_on(UEClient, MapInst, DrawerInst, CupList, 0, CupNum),
    get_id(DrawerInst, DrawerId),
    ue_start_simulation(UEClient, [DrawerId], SimTime),
    ue_apply_force_to(UEClient, DrawerId, PushForce, 0, 0).

% stack a list of objects on top of a base objects 
stack_up_on(UEClient, MapInst, Base, ObjList, Index, Count) :-
    Index < Count,
    nth0(Index, ObjList, Obj),
    get_pose(MapInst, Base, BaseX, BaseY, BaseZ, BaseQX, BaseQY, BaseQZ, BaseQW),
    instance_of(Base, BaseClass),
    instance_of(Obj, ObjClass),
    get_height(BaseClass, BaseHeight),
    get_height(ObjClass, ObjHeight),
    NewZ is BaseZ - 0.2 * BaseHeight + 1.1 * ObjHeight * Index,
    get_id(Obj, ObjId),
    ue_set_individual_pose(UEClient, ObjId, BaseX, BaseY, NewZ, 0, 0, 0, 1),
    sleep(1),
    ue_start_simulation(UEClient, [ObjId], -1),
    sleep(1),
    N is Index + 1,
    stack_up_on(UEClient, MapInst, Base, ObjList, N, Count).

%check if stack episode work
check_stack_episode(EpName, EpParam, Result) :-
    u_load_ameva_episode(EpName, EpInst),
    EventType = 'http://knowrob.org/kb/knowrob.owl#TouchingSituation',
    DrawerInst = 'http://knowrob.org/kb/Experiment.owl#zVBHGrf9n0qEVqc8aDbF-w',
    findall(EventInst, 
        (
            u_occurs(EpInst, EventInst, EventType),
            triple(EventInst, knowrob:inContact, DrawerInst)
        ), EvtList),
    length(EvtList, Count),
    (Count =:= 1 -> Result is EpParam; Result is 0 ).

% create a list that containe episode names
create_episode_names(CupNum, EpNames) :-
    findall(Num, between(1, CupNum, Num), EpNames).

% get the id of the individual
get_id(Individual, Id) :-
    split_string(Individual, "#", "", StrList),
    nth1(2, StrList, Id).

create_stack_episode_params(Start, End, EpParams) :-
    findall(Num, between(Start, End, Num), EpParams).

create_stack_episode_params(MaxCupNum, EpParams) :-
    findall(Num, between(1, MaxCupNum, Num), EpParams).

episodes_param_to_name(EpParam, EpName) :-
    atom_concat('Ep', EpParam, EpName).

create_stack_episode_names(EpParams, EpNames) :-
    maplist(episodes_param_to_name, EpParams, EpNames).

ue_wait_simulation(Duration) :-
    SleepTime is Duration +  1,  
    sleep(SleepTime).

ag_wait_close_clients :-
    sleep(50).

get_push_force(Force) :-
    Force is 22.

get_simulation_time(Duration) :-
    Duration is 6.

build_param_list(Obj, Num, List) :-
    length(List, Num), 
    maplist(=(Obj), List).

get_max_batch_size(BatchSize)  :-
    BatchSize is 2.

re(X) :-
 (X > 0 -> writeln(X),X1 is X -1, re(X1);true).