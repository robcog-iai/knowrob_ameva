
:- module(ue_ameva_episode,
    [
        ep_inst/1,
        u_occurs/2,
        u_occurs/3,
        u_load_ameva_episode/2
    ]).

% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

% get the instance of all the episodes
ep_inst(EpInst) :-
    triple(EpInst, rdf:type, knowrob:'UnrealExperiment').

% get events which occured in the experiment
u_occurs(EpInst, EventInst) :-
    triple(EventInst, knowrob:inEpisode, EpInst).

% get events which occured in the episodes
u_occurs(EpInst, EventInst, EventType) :-
    triple(EventInst, knowrob:inEpisode, EpInst),
    triple(EventInst, rdf:type, EventType).

% load the episode by giving episode name
u_load_ameva_episode(EpName, EpInst) :-
    atomic_list_concat(['/home/robcog/catkin_ws/data/', EpName, '_ED.owl'], OwlFile),
    tripledb_load(OwlFile),
    atom_concat('http://knowrob.org/kb/Experiment.owl#', EpName, EpInst).
