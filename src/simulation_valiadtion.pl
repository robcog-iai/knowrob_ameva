:- module(simulation_validation,   
    [
        check_if_supported/2,
        get_supporting_individual/2
    ]).

check_if_supported(Supported, Supporting) :-
    triple(Evt, knowrob:isSupported, Supported),
    triple(Evt, knowrob:isSupporting, SupportingId),
    is_a(SupportingId, Supporting).

get_supporting_individual(Supported, Supporting) :- 
    triple(Evt, knowrob:isSupported, Supported),
    triple(Evt, knowrob:isSupporting, Supporting).
