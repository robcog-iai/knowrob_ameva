:- module(ue_symbolic_log,   
    [
        add_log_namespace/2,
        check_if_supported/2,
        get_supporting_individual/2
    ]).

% Add log namespace to individual id for querying in symbolic log owl file
add_log_namespace(IndividualId, Individual) :-
    atom_concat('http://knowrob.org/kb/Experiment.owl#', IndividualId, Individual).

% Get the supporting individual of the given individual
get_supporting_individual(Supported, Supporting) :-
    triple(Evt, knowrob:isSupported, Supported),
    triple(Evt, knowrob:isSupporting, SupportingId),
    is_a(SupportingId, Supporting).

% Check if the individual is supported by other given individual
check_if_supported(Supported, Supporting) :- 
    triple(Evt, knowrob:isSupported, Supported),
    triple(Evt, knowrob:isSupporting, Supporting).
