:- module(ue_symbolic_log,   
    [
        add_log_namespace/2,
        check_if_supported/2,
        get_supporting_individual/2
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
