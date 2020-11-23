
:- module(testcase,   
    [
        test_case_one_A/2,
        test_case_one_B/2,
        test_case_one_C/2,
        test_case_two_A/1,
        test_case_two_B/1
    ]).

:- use_foreign_library('libknowrob_ameva.so').
:- use_module('./simulation_valiadtion.pl').

% hard code test case one
test_case_one_A(Client, Supporting) :-
    get_named_individual_id('mCjrWZo54kyS8eIV9f4MFA', OwlId),
    ue_move_individual(Client, 'mCjrWZo54kyS8eIV9f4MFA', -240, -20, 110, 0, 0, 0, 0),
    ue_simulate_and_log(Client, 'Task2','Episode1-1', 5, ['mCjrWZo54kyS8eIV9f4MFA']),
    tripledb_load('/home/robcog/catkin_ws/data/Episode2_ED.owl'),
    get_supporting_individual(OwlId, Supporting).

test_case_one_B(Client, Supporting) :-
    get_named_individual_id('mCjrWZo54kyS8eIV9f4MFA', OwlId),
    ue_move_individual(Client, 'mCjrWZo54kyS8eIV9f4MFA', -240, -20, 130, 0, 0, 0, 0),
    ue_simulate_and_log(Client, 'Task2','Episode1-2', 5, ['mCjrWZo54kyS8eIV9f4MFA']),
    tripledb_load('/home/robcog/catkin_ws/data/Episode3_ED.owl'),
    get_supporting_individual(OwlId, Supporting).

test_case_one_C(Client, Supporting) :-
    get_named_individual_id('mCjrWZo54kyS8eIV9f4MFA', OwlId),
    ue_move_individual(Client, 'mCjrWZo54kyS8eIV9f4MFA', -240, -20, 150, 0, 0, 0, 0),
    ue_simulate_and_log(Client, 'Task2','Episode1-2', 5, ['mCjrWZo54kyS8eIV9f4MFA']),
    tripledb_load('/home/robcog/catkin_ws/data/Episode4_ED.owl'),
    get_supporting_individual(OwlId, Supporting).

% hard code test case two
test_case_two_A(Client) :-
    get_named_individual_id('JEkITyZgxUS6KcUONncr3w', OwlIdPlateZero),
    get_named_individual_id('ZMk26jazvkyD9oPT61Coqw', OwlIdPlateOne),
    get_named_individual_id('Ncvil7sD10-wcWVQq3CuSg', OwlIdPlateTwo),
    ue_start_symbolic_log(Client,'Task2','Episode2-1-1'),
    ue_move_individual(Client, 'ZMk26jazvkyD9oPT61Coqw', -181, 111, 106, 0, 0, 0, 0),
    ue_simulate_for_seconds(Client, 5, ['ZMk26jazvkyD9oPT61Coqw', 'JEkITyZgxUS6KcUONncr3w']),
    ue_move_individual(Client, 'Ncvil7sD10-wcWVQq3CuSg', -178, 108, 115, 0, 0, 0, 0),
    ue_simulate_for_seconds(Client, 5, ['Ncvil7sD10-wcWVQq3CuSg', 'ZMk26jazvkyD9oPT61Coqw', 'JEkITyZgxUS6KcUONncr3w']),
    ue_stop_symbolic_log(Client),
    tripledb_load('/home/robcog/catkin_ws/data/Episode2-1-1_ED.owl'),
    check_if_supported(OwlIdPlateOne, OwlIdPlateZero),
    check_if_supported(OwlIdPlateTwo, OwlIdPlateOne).

test_case_two_B(Client) :-
    get_named_individual_id('JEkITyZgxUS6KcUONncr3w', OwlIdPlateZero),
    get_named_individual_id('ZMk26jazvkyD9oPT61Coqw', OwlIdPlateOne),
    get_named_individual_id('Ncvil7sD10-wcWVQq3CuSg', OwlIdPlateTwo),
    ue_start_symbolic_log(Client,'Task2','Episode2-1-1'),
    ue_move_individual(Client, 'ZMk26jazvkyD9oPT61Coqw', -181, 111, 120, 0, 0, 0, 0),
    ue_simulate_for_seconds(Client, 5, ['ZMk26jazvkyD9oPT61Coqw', 'JEkITyZgxUS6KcUONncr3w']),
    ue_move_individual(Client, 'Ncvil7sD10-wcWVQq3CuSg', -160, 95, 150, 0, 0, 0, 0),
    ue_simulate_for_seconds(Client, 5, ['Ncvil7sD10-wcWVQq3CuSg', 'ZMk26jazvkyD9oPT61Coqw', 'JEkITyZgxUS6KcUONncr3w']),
    ue_stop_symbolic_log(Client),
    tripledb_load('/home/robcog/catkin_ws/data/Episode2-1-1_ED.owl'),
    check_if_supported(OwlIdPlateOne, OwlIdPlateZero),
    check_if_supported(OwlIdPlateTwo, OwlIdPlateOne).

get_named_individual_id(Id, OwlId) :- 
    atom_concat('http://knowrob.org/kb/Experiment.owl#', Id, OwlId).