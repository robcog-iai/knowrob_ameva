:- module(testcase,   
    [
        
        remove_namespace/2,
        place_above/7,
        scenario_apple/2,
        scenario_apple_two/2
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
    place_above(Client, Map, AppleInMap, PlateInMap, 0, 0, 10),
    sleep(10),
    ue_stop_symbolic_log(Client),
    ue_recv_symbolic_log(Client, 'Task1', 'Episode1-1'),
    tripledb_load('/home/robcog/catkin_ws/data/Episode1-1_ED.owl'),
    add_log_namespace(AppleId, AppleInLog),
    get_supporting_individual(AppleInLog, X).

%% place_above(+Client, +Map, +IndividualToPlace, +IndividualToPlaceAt, +RelativeX, +RelativeY, +RelativeZ)
%
% Place an individual above other given individual
%
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