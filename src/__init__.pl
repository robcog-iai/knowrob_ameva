:- module(testcase,   
    [
        place_apple/3,
        stack_cup_in_drawer/3,
        drawer_capacity/3,
        %test_query/1,
        %parallel_stack/0
    ]).
:- use_foreign_library('libknowrob_ameva.so').
:- use_module('./ue_semantic_map.pl').
:- use_module('./ue_episode.pl').
:- use_module('./ue_namespace.pl').

:- rdf_db:rdf_register_ns(knowrob, 'http://knowrob.org/kb/knowrob.owl#',  [keep(true)]).

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
    sleep(5),
    ue_stop_loggers(Client),
    ue_get_episode_data(Client, 'Task2', 'Episode2-1'),
    u_load_episode('Episode2-1_ED', EpInst),
    add_knowrob_namespace('TouchingSituation', EventType),
    add_log_namespace('zVBHGrf9n0qEVqc8aDbF-w', DrawerInstLog),
    findall(EventInst, 
        (
            u_occurs(EpInst, EventInst, EventType), 
            triple(EventInst, knowrob:inContact, DrawerInstLog)
        ), EvtList),
    length(EvtList, Count),
    Count =:= 1.

% Put an apple on the plate and simulate for 10s
place_apple(Client, Height, SupportingObj) :-
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
    sleep(5),
    ue_stop_loggers(Client),
    ue_get_episode_data(Client, 'Task1', 'Episode1'),
    u_load_episode('Episode1_ED', EpInst),
    add_log_namespace(AppleId, AppleInLog),
    add_knowrob_namespace('SupportedBySituation', EventType),
    u_occurs(EpInst, EventInst, EventType),
    triple(EventInst, knowrob:isSupported, AppleInLog),
    triple(EventInst, knowrob:isSupporting, Supporting),
    triple(Supporting, rdf:type, SupportingObj),
    not(SupportingObj == 'http://www.w3.org/2002/07/owl#NamedIndividual').

% place ToPlace object above the PlaceAt object
place_above(Client, MapInst, ToPlace, PlaceAt, RelativeX, RelativeY, RelativeZ) :-
    translation(MapInst, PlaceAt, PAX, PAY, PAZ),
    quaternion(MapInst, ToPlace, TPQX, TPQY, TPQZ, TPQW),
    NewX is PAX+RelativeX,
    NewY is PAY+RelativeY,
    NewZ is PAZ+RelativeZ,
    remove_namespace(ToPlace, ToPlaceId),
    ue_set_individual_pose(Client, ToPlaceId, NewX, NewY, NewZ, TPQX, TPQY, TPQZ, TPQW).

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
    ue_set_individual_pose(Client, ObjId, BaseX, BaseY, NewZ, BaseQX, BaseQY, BaseQZ, BaseQW),
    ue_start_simulation(Client, [ObjId], -1),
    N is Index + 1,
    stack_up_on(Client, MapInst, Base, ObjList, N, Count).
