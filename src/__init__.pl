:- module(testcase,   
    [
        place_apple/2,
        show_apple_traj/1,
        highlight_support/2,
        stack_cup_in_drawer/3,
        show_cups_traj/4,
        drawer_capacity/3,
        parallel_stack/2,
        check_episode_success/1,
        test_query/2
    ]).
:- use_foreign_library('libknowrob_ameva.so').
:- use_module('./ue_semantic_map.pl').
:- use_module('./ue_episode.pl').
:- use_module('./ue_namespace.pl').

%:- rdf_db:rdf_register_ns(knowrob, 'http://knowrob.org/kb/knowrob.owl#',  [keep(true)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%      Stack cups in the drawer         %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% stack cups in the drawer
stack_cup_in_drawer(Client, CupNum, PushForce) :-
    load_semantic_map('TaskTwo', MapInst),
    add_knowrob_namespace('MPCupSet01', CupClass),
    add_knowrob_namespace('IAIDrawerW60H29', DrawerClass),
    individual_list(CupClass, MapInst, CupList),
    ue_start_loggers(Client, 'Task2','Episode2-1'),
    add_iai_kitchen_namespace('zVBHGrf9n0qEVqc8aDbF-w', DrawerInst),
    \+stack_up_on(Client, MapInst, DrawerInst, CupList, 0, CupNum),
    ue_start_simulation(Client, ['zVBHGrf9n0qEVqc8aDbF-w'], -1),
    ue_apply_force_to(Client, 'zVBHGrf9n0qEVqc8aDbF-w', PushForce, 0, 0),
    sleep(3),
    ue_stop_loggers(Client),
    ue_get_episode_data(Client, 'Task2', 'Episode2-1'),
    u_load_episode('Episode2-1', EpInst),
    add_knowrob_namespace('TouchingSituation', EventType),
    add_log_namespace('zVBHGrf9n0qEVqc8aDbF-w', DrawerInstLog),
    findall(EventInst, 
        (
            u_occurs(EpInst, EventInst, EventType), 
            triple(EventInst, knowrob:inContact, DrawerInstLog)
        ), EvtList),
    length(EvtList, Count),
    Count =:= 1.

show_cups_traj(Client, Task, Episode,Cup) :-
    load_semantic_map('TaskTwo', MapInst),
    add_knowrob_namespace('MPCupSet01', CupClass),
    individual(CupClass, MapInst, Cup),
    remove_namespace(Cup, CupId),
    ue_set_task(Client, Task),
    ue_set_episode(Client,Episode),
    ue_draw_marker(Client, CupId, 0, 1000, 'sphere', 'red', 0.01, 'Translucent').

% stack a list of objects on top of a base objects 
stack_up_on(Client, MapInst, Base, ObjList, Index, Count) :-
    Index < Count,
    nth0(Index, ObjList, Obj),
    pose(MapInst, Base, BaseX, BaseY, BaseZ, BaseQX, BaseQY, BaseQZ, BaseQW),
    instance_of(Base, BaseClass),
    instance_of(Obj, ObjClass),
    height(BaseClass, BaseHeight),
    height(ObjClass, ObjHeight),
    NewZ is BaseZ - 0.2 * BaseHeight + 1.1 * ObjHeight * Index,
    remove_namespace(Obj, ObjId),
    ue_set_individual_pose(Client, ObjId, BaseX, BaseY, NewZ, 0, 0, 0, 1),
    sleep(1),
    ue_start_simulation(Client, [ObjId], -1),
    sleep(1),
    N is Index + 1,
    stack_up_on(Client, MapInst, Base, ObjList, N, Count).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%      Place apple in the plate        %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% put an apple on the plate and simulate for 5s
place_apple(Client, Height) :-
    load_semantic_map('TaskOne', MapInst),
    add_knowrob_namespace('MPAppleRed', AppleClass),
    add_knowrob_namespace('ClassicPlate16cm', PlateClass),
    individual(AppleClass, MapInst, AppleInMap),
    individual(PlateClass, MapInst, PlateInMap),
    ue_start_loggers(Client, 'Task1','Episode1'),
    remove_namespace(AppleInMap, AppleId),
    remove_namespace(PlateInMap, PlateId),
    ue_start_simulation(Client, [AppleId, PlateId], 5),
    place_above(Client, MapInst, AppleInMap, PlateInMap, 0, 0, Height),
    sleep(6),
    ue_stop_loggers(Client),
    ue_get_episode_data(Client, 'Task1', 'Episode1').

% put an apple on the plate and simulate for 5s
show_apple_traj(Client) :-
    load_semantic_map('TaskOne', MapInst),
    add_knowrob_namespace('MPAppleRed', AppleClass),
    individual(AppleClass, MapInst, AppleInMap),
    remove_namespace(AppleInMap, AppleId),
    u_load_episode('Episode1', EpInst),
    ue_set_task(Client, 'Task1'),
    ue_set_episode(Client,'Episode1'),
    ue_draw_marker(Client, AppleId, 0, 1000, 'sphere', 'red', 0.01, 'Translucent').

% put an apple on the plate and simulate for 5s
highlight_support(Client, SupportingObj) :-
    load_semantic_map('TaskOne', MapInst),
    add_knowrob_namespace('MPAppleRed', AppleClass),
    individual(AppleClass, MapInst, AppleInMap),
    remove_namespace(AppleInMap, AppleId),
    u_load_episode('Episode1', EpInst),
    add_log_namespace(AppleId, AppleInLog),
    add_knowrob_namespace('SupportedBySituation', EventType),
    u_occurs(EpInst, EventInst, EventType),
    triple(EventInst, knowrob:isSupported, AppleInLog),
    triple(EventInst, knowrob:isSupporting, SupportingInLog),
    remove_namespace(SupportingInLog, SupportingId),
    add_iai_kitchen_namespace(SupportingId, SupportingInMap),
    triple(SupportingInMap, rdf:type, SupportingObj),
    not(SupportingObj == 'http://www.w3.org/2002/07/owl#NamedIndividual'),
    ue_highlight(Client, SupportingId,'red','Translucent').


% place ToPlace object above the PlaceAt object
place_above(Client, MapInst, ToPlace, PlaceAt, RelativeX, RelativeY, RelativeZ) :-
    translation(MapInst, PlaceAt, PAX, PAY, PAZ),
    quaternion(MapInst, ToPlace, TPQX, TPQY, TPQZ, TPQW),
    NewX is PAX+RelativeX,
    NewY is PAY+RelativeY,
    NewZ is PAZ+RelativeZ,
    remove_namespace(ToPlace, ToPlaceId),
    ue_set_individual_pose(Client, ToPlaceId, NewX, NewY, NewZ, TPQX, TPQY, TPQZ, TPQW).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%      Parallel stacking                %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parallel_stack(Ret1,Ret2) :-
    ue_create_gs,
    ue_create_gs,
    ue_wait_client(2),
    ue_recent_client(ClientOne),
    ClientTwo is ClientOne - 1,
    load_semantic_map('TaskTwo', MapInst),
    ue_start_loggers(ClientOne, 'TaskPa','2Cup'),
    ue_start_loggers(ClientTwo, 'TaskPa','3Cup'),
    stack_cup_in_drawer_action(ClientOne, 2, 22, 10),
    stack_cup_in_drawer_action(ClientTwo, 3, 22, 10),
    sleep(10),
    ue_stop_loggers(ClientOne),
    ue_stop_loggers(ClientTwo),
    ue_get_episode_data(ClientOne, 'TaskPa', '2Cup'),
    ue_get_episode_data(ClientTwo, 'TaskPa', '3Cup'),
    
    u_load_episode('2Cup', EpInstOne),
    u_load_episode('3Cup', EpInstTwo),

    (check_episode_success(EpInstOne) -> Ret1 = '2Cup-success'; Ret2 = '2Cup-fail'),
    (check_episode_success(EpInstTwo) -> Ret2 = '3Cup-success'; Ret2 = '3Cup-fail').

check_episode_success(EpInst) :-
    add_knowrob_namespace('TouchingSituation', EventType),
    add_log_namespace('zVBHGrf9n0qEVqc8aDbF-w', DrawerInstLog),
    findall(EventInst, 
        (
            u_occurs(EpInst, EventInst, EventType), 
            triple(EventInst, knowrob:inContact, DrawerInstLog)
        ), EvtList),
    length(EvtList, Count),
    Count =:= 1.

% stack cups in the drawer
stack_cup_in_drawer_action(Client, CupNum, PushForce, SimTime) :-
    add_knowrob_namespace('MPCupSet01', CupClass),
    add_knowrob_namespace('IAIDrawerW60H29', DrawerClass),
    individual_list(CupClass, MapInst, CupList),
    add_iai_kitchen_namespace('zVBHGrf9n0qEVqc8aDbF-w', DrawerInst),
    \+stack_up_on(Client, MapInst, DrawerInst, CupList, 0, CupNum),
    ue_start_simulation(Client, ['zVBHGrf9n0qEVqc8aDbF-w'], SimTime),
    ue_apply_force_to(Client, 'zVBHGrf9n0qEVqc8aDbF-w', PushForce, 0, 0).


% get the number of objects that is able to stack in the drawer
drawer_capacity(DrawerClass, ObjClass, Num) :-
    get_height(DrawerClass, DrawerHeight),
    get_height(ObjClass, ObjHeight),
    Factor is DrawerHeight / ObjHeight,
    Num is floor(Factor).

% get the smaller number
min_num(A, B, Min) :-
    A < B -> Min is A  ; Min is B.



%%%%%%%%%
test_query(ClientOne, Ret1) :-
    load_semantic_map('TaskTwo', MapInst),
    ue_start_loggers(ClientOne, 'TaskPa','2Cup'),
    stack_cup_in_drawer_action(ClientOne, 2, 2, 5),
    sleep(5),
    ue_stop_loggers(ClientOne),
    ue_get_episode_data(ClientOne, 'TaskPa', '2Cup'),
    
    u_load_episode('2Cup', EpInstOne),

    check_episode_success(EpInstOne) -> append([], '2Cup', Ret1).

