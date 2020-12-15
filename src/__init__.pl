:- module(testcase,   
    [
        
        remove_namespace/2,
        place_above/7,
        scenario_apple/2,
        scenario_stack/1
    ]).

:- use_foreign_library('libknowrob_ameva.so').
:- use_module('./ue_symbolic_log.pl').
:- use_module('./ue_semantic_map.pl').

% Put an apple on the plate and simulate for 10s
scenario_apple(Client, X) :-
    import_semantic_map('TaskOne', Map),
    get_individual_by_class('http://knowrob.org/kb/knowrob.owl#MPAppleRed', Map, AppleInMap),
    get_individual_by_class('http://knowrob.org/kb/knowrob.owl#ClassicPlate16cm', Map, PlateInMap),
    ue_start_symbolic_log(Client, 'Task1','Episode1-1'),
    remove_namespace(AppleInMap, AppleId),
    remove_namespace(PlateInMap, PlateId),
    ue_start_simulation(Client, [AppleId, PlateId], 10),
    place_above(Client, Map, AppleInMap, PlateInMap, 0, 0, 12),
    sleep(10),
    ue_stop_symbolic_log(Client),
    ue_recv_symbolic_log(Client, 'Task1', 'Episode1-1'),
    tripledb_load('/home/robcog/catkin_ws/data/Episode1-1_ED.owl'),
    add_log_namespace(AppleId, AppleInLog),
    get_supporting_individual(AppleInLog, X).

scenario_stack(Client) :-
    import_semantic_map('TaskTwo', Map),
    get_individual_list_by_class('http://knowrob.org/kb/knowrob.owl#MPCupSet01', Map, List),
    add_ue_iai_kitchen_namespace('KnCVm0S4cUSrMiMcX_6q3w', BaseInMap),
    ue_start_symbolic_log(Client, 'Task2','Episode2-1'),
    \+stack_up_on(Client, Map, BaseInMap, List, 0, 3),
    ue_start_simulation(Client, ['KnCVm0S4cUSrMiMcX_6q3w'], -1),
    ue_apply_force_to(Client, 'KnCVm0S4cUSrMiMcX_6q3w', 40, 0, 0),
    sleep(5),
    ue_stop_symbolic_log(Client),
    ue_recv_symbolic_log(Client, 'Task2', 'Episode2-1'),
    import_symbolic_log('Episode2-1_ED', Experiment),
    add_log_namespace('KnCVm0S4cUSrMiMcX_6q3w', BaseInLog),
    findall(Event, (triple(Event, knowrob:inEpisode, Experiment), triple(Event, knowrob:inContact, BaseInLog)), EvtList),
    length(EvtList, Len),
    Len =:= 1.

%% place_above(+Client, +Map, +IndividualToPlace, +IndividualToPlaceAt, +RelativeX, +RelativeY, +RelativeZ)
%
% Place an individual above other given individual  
% @param Client the unreal client to send 
% @param Map the semantic environment map
% @param IndividualToPlace the given individual to move
% @param IndividualToPlaceAt the individual which is placed at
% @param RelativeX the relative x value
% @param RelativeY the relative y value
% @param RelativeZ the relative z value
%
place_above(Client, Map, IndividualToPlace, IndividualToPlaceAt, RelativeX, RelativeY, RelativeZ) :-
    get_translation(Map, IndividualToPlaceAt, PATX, PATY, PATZ),
    get_quaternion(Map, IndividualToPlace, PQX, PQY, PQZ, PQW),
    NewX is PATX+RelativeX,
    NewY is PATY+RelativeY,
    NewZ is PATZ+RelativeZ,
    remove_namespace(IndividualToPlace, IndividualToPlaceId),
    ue_set_individual_pose(Client, IndividualToPlaceId, NewX, NewY, NewZ, PQX, PQY, PQZ, PQW).

%% place_above(+Client, +Map, +IndividualToPlace, +IndividualToPlaceAt, +RelativeX, +RelativeY, +RelativeZ)
%
% Place an individual above other given individual
%
% @param Client the unreal client to send
% @param Map the semantic environment map
% @param Base the object to be stacked on
% @param IndividualList the list of individuals that can be moved
% @param Index index of individuals
% @param Count the number of individuals that need to be moved
%
stack_up_on(Client, Map, Base, IndividualList, Index, Count) :-
    Index < Count,
    nth0(Index, IndividualList, Individual),
    get_pose(Map, Base, BaseX, BaseY, BaseZ, BaseQX, BaseQY, BaseQZ, BaseQW),
    instance_of(Base, BaseClass),
    instance_of(Individual, IndividualClass),
    get_height(BaseClass, BaseHeight),
    get_height(IndividualClass, IndividualHeight),
    NewZ is  BaseZ - 0.2 * BaseHeight + 1.1 * IndividualHeight * Index,
    remove_namespace(Individual, IndividualId),
    ue_set_individual_pose(Client, IndividualId, BaseX, BaseY, NewZ, BaseQX, BaseQY, BaseQZ, BaseQW),
    ue_start_simulation(Client, [IndividualId], -1),
    N is Index + 1,
    stack_up_on(Client, Map, Base, IndividualList, N, Count).

%% remove_namespace(+Individual, -Id) is nondet
%
% Remove namespace to get the individual id
%
% @param Individual the named individual
% @param Id the individual without namespace
%
remove_namespace(Individual, Id) :-
    split_string(Individual, "#", "", StrList),
    nth1(2, StrList, Id).
