:- module(ue_symbolic_log,   
    [
        add_log_namespace/2,
        check_if_supported/2,
        get_supporting_individual/2,
        import_symbolic_log/2
    ]).

%% add_log_namespace(+IndividualId, -Individual) is det
%
% Add log namespace to individual id for querying in symbolic log owl file
%
% @param IndividualId the individual id without namespace
% @param Individual the named individual
%
add_log_namespace(IndividualId, Individual) :-
    atom_concat('http://knowrob.org/kb/Experiment.owl#', IndividualId, Individual).

%% get_supporting_individual(+Supported, -Supporting) is nondet
%
% Get the supporting individual of the given individual
%
% @param Supported the individual being supported
% @param Supporting the individual that support given individual
%
get_supporting_individual(Supported, Supporting) :-
    triple(Evt, knowrob:isSupported, Supported),
    triple(Evt, knowrob:isSupporting, SupportingId),
    is_a(SupportingId, Supporting).


%% check_if_supported(+Supported, +Supporting) is det
%
% Check if the individual is supported by other given individual
%
% @param Supported the individual being supported
% @param Supporting the individual that support given individual
%
check_if_supported(Supported, Supporting) :- 
    triple(Evt, knowrob:isSupported, Supported),
    triple(Evt, knowrob:isSupporting, Supporting).

%% import_symbolic_log(+Log, -Experiment) is det
%
% Load the semantic map and return the map id with namespace
%
% @param Log the symbolic log file
% @param Experiment the experiment individual
%
import_symbolic_log(Log, Experiment) :-
    atomic_list_concat(['/home/robcog/catkin_ws/data/', Log, '.owl'], OwlFile),
    tripledb_load(OwlFile),
    triple(Map, rdf:type, 'http://knowrob.org/kb/knowrob.owl#UnrealExperiment').
