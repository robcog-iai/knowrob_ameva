
:- module(am_episode,
    [
        am_ep_inst/1,
        am_occurs/2,
        am_occurs/3,
        am_load_episode/2
    ]).

% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

% get the instance of all the episodes
% am_ep_inst(-EpInst)
%
am_ep_inst(EpInst) :-
    triple(EpInst, rdf:type, knowrob:'UnrealExperiment').


% get events which occured in the experiment
% am_occurs(+EpInst, -EventInst)
%
am_occurs(EpInst, EventInst) :-
    triple(EventInst, knowrob:inEpisode, EpInst).

% get events which occured in the episodes
% am_occurs(+EpInst, ?EventInst, ?EventType) 
%
am_occurs(EpInst, EventInst, EventType) :-
    triple(EventInst, knowrob:inEpisode, EpInst),
    triple(EventInst, rdf:type, EventType).

% load the episode by giving episode name
% am_load_episode(+EpName, -EpInst) 
%
am_load_episode(EpName, EpInst) :-
    atomic_list_concat(['package://knowrob_ameva/episodes/', EpName, '_ED.owl'], OwlFile),
    tripledb_load(OwlFile),
    atom_concat('http://knowrob.org/kb/ameva_log.owl#', EpName, EpInst).
