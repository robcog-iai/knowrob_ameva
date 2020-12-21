
:- module(ue_episode,
    [
        ep_inst/1,
        u_occurs/2,
        u_occurs/3,
        u_load_episode/2
    ]).

% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
:- use_module('./ue_namespace.pl').

% get the instance of the episode
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
u_load_episode(EpName, EpInst) :-
    atomic_list_concat(['/home/robcog/catkin_ws/data/', EpName, '.owl'], OwlFile),
    tripledb_load(OwlFile),
    ep_inst(EpInst).
