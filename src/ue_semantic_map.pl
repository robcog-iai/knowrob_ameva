:- module(ue_semantic_map,   
    [
        import_semantic_map/2,
        get_individual_by_class/3,
        get_translation/5,
        get_quaternion/6,
        get_pose/9,
        add_ue_iai_kitchen_namespace/2
    ]).

% Add namespace to individual id for querying individuals in semantic map owl file
add_ue_iai_kitchen_namespace(IndividualId, Individual) :-
    atom_concat('http://knowrob.org/kb/UE-IAI-Kitchen.owl#', IndividualId, Individual).

% Get the translation and quaternion of the individual
get_pose(Map, Individual, TX, TY, TZ, QX, QY, QZ, QW) :-
    get_translation(Map, Individual, TX, TY, TZ),
    get_quaternion(Map, Individual, QX, QY, QZ, QW).

% Get the translation of the individual
get_translation(Map, Individual, X, Y, Z) :-
    triple(Individual, knowrob:describedInMap, Map),
    triple(Individual, knowrob:pose, Pose),
    triple(Pose, knowrob:translation, TranslationStr),
    split_string(TranslationStr, " ", "", TranslationList),
    nth1(1, TranslationList, XStr),
    nth1(2, TranslationList, YStr),
    nth1(3, TranslationList, ZStr),
    number_string(X, XStr),
    number_string(Y, YStr),
    number_string(Z, ZStr).

% Get the quaternion of the individual
get_quaternion(Map, Individual, X, Y, Z, W) :-
    triple(Individual, knowrob:describedInMap, Map),
    triple(Individual, knowrob:pose, Pose),
    triple(Pose, knowrob:quaternion, QuaternionStr),
    split_string(QuaternionStr, " ", "", QuaternionList),
    nth1(1, QuaternionList, XStr),
    nth1(2, QuaternionList, YStr),
    nth1(3, QuaternionList, ZStr),
    nth1(3, QuaternionList, WStr),
    number_string(X, XStr),
    number_string(Y, YStr),
    number_string(Z, ZStr),
    number_string(W, WStr).

% Get all individuals of specific class
get_individual_by_class(Class, Map, Individual) :-
    triple(Individual, knowrob:describedInMap, Map),
    triple(Individual, rdf:type, Class).
    
% Load the semantic map and return the map id with namespace
import_semantic_map(Task, Map) :-
    atomic_list_concat(['package://knowrob_ameva/maps/', Task, '.owl'], OwlFile),
    tripledb_load(OwlFile),
    triple(Map, rdf:type, 'http://knowrob.org/kb/knowrob.owl#SemanticEnvironmentMap').
