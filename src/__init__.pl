:- module(ameva_scenario,   
    [
        stack_sim/0
    ]).
:- use_foreign_library('libknowrob_ameva.so').
:- use_module('./ue_ameva_semantic_map.pl').
:- use_module('./ue_ameva_episode.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%   Stack cups in the drawer in parallel      %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stack_sim :-
    DrawerClass = 'http://knowrob.org/kb/knowrob.owl#IAIDrawerW60H29',
    CupClass = 'http://knowrob.org/kb/knowrob.owl#MPCupSet01',
    DrawerInst = 'http://knowrob.org/kb/Experiment.owl#zVBHGrf9n0qEVqc8aDbF-w',
    load_ameva_semantic_map('TaskStack', MapInst),
    get_stack_capacity(DrawerClass, CupClass, MaxNum),
    parallel_simulation('TaskStack', MapInst, DrawerInst, CupClass, MaxNum).

% check how many cups can be stacked in the drawer
get_stack_capacity(DrawerClass, CupClass, MaxNum) :-
    get_height(DrawerClass, DrawerHeight),
    get_height(CupClass, CupHeight),
    Factor is DrawerHeight / CupHeight,
    MaxNum is floor(Factor).

% Create instances and start simulation in parallel
parallel_simulation(Task, MapInst, DrawerInst, CupClass, Num) :-
    ue_create_gs(Num, UEClients),
    create_episode_names(Num, EpNames),
    ue_start_loggers(UEClients, Task, EpNames),
    stack_cup_in_drawer_action(UEClients, MapInst, DrawerInst, CupClass, Num, 22, 5),
    sleep(6),
    ue_stop_loggers(UEClients),
    ue_get_episode_data(UEClients, Task, EpNames).

% stack cups on the drawer
stack_cup_in_drawer_action(UEClients, MapInst, DrawerInst, CupClass, CupNum, PushForce, SimTime) :-
    get_individual_list(CupClass, MapInst, CupList),
    \+stack_up_on(UEClients, MapInst, DrawerInst, CupList, 0, CupNum),
    get_id(DrawerInst, DrawerId),
    ue_start_simulation(UEClients, [DrawerId], SimTime),
    ue_apply_force_to(UEClients, DrawerId, PushForce, 0, 0).

% stack a list of objects on top of a base objects 
stack_up_on(UEClients, MapInst, Base, ObjList, Index, Count) :-
    Index < Count,
    nth0(Index, ObjList, Obj),
    get_pose(MapInst, Base, BaseX, BaseY, BaseZ, BaseQX, BaseQY, BaseQZ, BaseQW),
    instance_of(Base, BaseClass),
    instance_of(Obj, ObjClass),
    get_height(BaseClass, BaseHeight),
    get_height(ObjClass, ObjHeight),
    NewZ is BaseZ - 0.2 * BaseHeight + 1.1 * ObjHeight * Index,
    get_id(Obj, ObjId),
    ue_set_individual_pose(UEClients, ObjId, BaseX, BaseY, NewZ, 0, 0, 0, 1),
    sleep(1),
    ue_start_simulation(UEClients, [ObjId], -1),
    sleep(1),
    N is Index + 1,
    stack_up_on(UEClients, MapInst, Base, ObjList, N, Count).

% create a list that containe episode names
create_episode_names(CupNum, EpNames) :-
    findall(Num, between(1, CupNum, Num), EpNames).

% get the id of the individual
get_id(Individual, Id) :-
    split_string(Individual, "#", "", StrList),
    nth1(2, StrList, Id).