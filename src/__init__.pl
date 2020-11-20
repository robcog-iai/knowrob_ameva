
:- module(testcase,   
    [
        test_case_one/2,
        test_case_two/2,
        test_case_three/2
    ]).

:- use_foreign_library('libknowrob_ameva.so').
:- use_module('./simulation_valiadtion.pl').

% hard code test case
test_case_one(Client, Supporting) :-
    get_named_individual_id('mCjrWZo54kyS8eIV9f4MFA', OwlId),
    ue_move_individual(Client, 'mCjrWZo54kyS8eIV9f4MFA', -240, -20, 110, 0, 0, 0, 0),
    ue_simulate_and_log(Client, 'Task2','Episode2', 10, ['mCjrWZo54kyS8eIV9f4MFA']),
    tripledb_load('/home/robcog/catkin_ws/data/Episode2_ED.owl'),
    get_supporting_individual(OwlId, Supporting).

test_case_two(Client, Supporting) :-
    get_named_individual_id('mCjrWZo54kyS8eIV9f4MFA', OwlId),
    ue_move_individual(Client, 'mCjrWZo54kyS8eIV9f4MFA', -240, -20, 130, 0, 0, 0, 0),
    ue_simulate_and_log(Client, 'Task2','Episode3', 10, ['mCjrWZo54kyS8eIV9f4MFA']),
    tripledb_load('/home/robcog/catkin_ws/data/Episode3_ED.owl'),
    get_supporting_individual(OwlId, Supporting).

test_case_three(Client, Supporting) :-
    get_named_individual_id('mCjrWZo54kyS8eIV9f4MFA', OwlId),
    ue_move_individual(Client, 'mCjrWZo54kyS8eIV9f4MFA', -240, -20, 150, 0, 0, 0, 0),
    ue_simulate_and_log(Client, 'Task2','Episode4', 10, ['mCjrWZo54kyS8eIV9f4MFA']),
    tripledb_load('/home/robcog/catkin_ws/data/Episode4_ED.owl'),
    get_supporting_individual(OwlId, Supporting).
        
get_named_individual_id(Id, OwlId) :- 
    atom_concat('http://knowrob.org/kb/Experiment.owl#', Id, OwlId).