
:- module(ue_namespace,   
    [
        add_knowrob_namespace/2,
        add_iai_kitchen_namespace/2,
        add_log_namespace/2,
        remove_namespace/2
    ]).

% add knowrob namespace
add_knowrob_namespace(Id, Instance) :-
    atom_concat('http://knowrob.org/kb/knowrob.owl#', Id, Instance).

% add ue-iai-kitchen namespace
add_iai_kitchen_namespace(Id, Instance) :-
    atom_concat('http://knowrob.org/kb/UE-IAI-Kitchen.owl#', Id, Instance).

% add log namespace
add_log_namespace(Id, Instance) :-
    atom_concat('http://knowrob.org/kb/Experiment.owl#', Id, Instance).

% remove namespace to get the individual id
remove_namespace(Individual, Id) :-
    split_string(Individual, "#", "", StrList),
    nth1(2, StrList, Id).

