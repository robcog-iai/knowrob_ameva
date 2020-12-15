:- module(ue_semantic_map,   
    [
        import_semantic_map/2,
        get_individual_by_class/3,
        get_individual_list_by_class/3,
        get_translation/5,
        get_quaternion/6,
        get_pose/9,
        get_height/2,
        get_depth/2,
        add_ue_iai_kitchen_namespace/2
    ]).

%% add_ue_iai_kitchen_namespace(+IndividualId, -Individual) is det
%
% Add namespace to individual id for querying individuals in semantic map owl file
%
% @param IndividualId the individual id without namespace
% @param Individual the named individual
%
add_ue_iai_kitchen_namespace(IndividualId, Individual) :-
    atom_concat('http://knowrob.org/kb/UE-IAI-Kitchen.owl#', IndividualId, Individual).

%% get_pose(+Map, +Individual, -TX, -TY, -TZ, -QX, -QY, -QZ, -QW) is det
%
% Get the translation and quaternion of the individual
%
% @param Map the semantic environment map 
% @param Individual the named individual
% @param TX the x of translation
% @param TY the y of translation
% @param TZ the z of translation
% @param TX the x of quaternion
% @param TY the y of quaternion
% @param TZ the z of quaternion
% @param TW the w of quaternion
%
get_pose(Map, Individual, TX, TY, TZ, QX, QY, QZ, QW) :-
    get_translation(Map, Individual, TX, TY, TZ),
    get_quaternion(Map, Individual, QX, QY, QZ, QW).

%% get_translation(+Map, +Individual, -X, -Y, -Z) is det
%
% Get the translation of the individual
%
% @param Map the semantic environment map 
% @param Individual the named individual
% @param X the x of translation
% @param Y the y of translation
% @param Z the z of translation
%
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

%% get_quaternion(+Map, +Individual, -X, -Y, -Z, -W) is det
%
% Get the quaternion of the individual
%
% @param Map the semantic environment map 
% @param Individual the named individual
% @param X the x of quaternion
% @param Y the y of quaternion
% @param Z the z of quaternion
% @param W the w of quaternion
%
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

%% get_height(+Class, -Height) is det
%
% Get the heigth property of the class
%
% @param Map the semantic environment map 
% @param Class the calss
% @param Height the heigth property of the class
%
get_height(Class, Height) :-
    triple(Class,rdfs:subClassOf, Description),
    triple(Description, owl:onProperty, 'http://knowrob.org/kb/knowrob.owl#heightOfObject'),
    triple(Description, owl:hasValue, Height).

%% get_depth(+Class, -Depth) is det
%
% Get the heigth property of the class
%
% @param Map the semantic environment map 
% @param Class the class
% @param Depth the depth property of the class
%
get_depth(Class, Depth) :-
    triple(Class,rdfs:subClassOf, Description),
    triple(Description, owl:onProperty, 'http://knowrob.org/kb/knowrob.owl#depthOfObject'),
    triple(Description, owl:hasValue, Depth).

%% get_individual_by_class(+Class, +Map, -Individual) is nondet
%
% Get all individuals of given class
%
% @param Class the class of named individual
% @param Map the semantic environment map 
% @param Individual the named individual
%
get_individual_by_class(Class, Map, Individual) :-
    triple(Individual, knowrob:describedInMap, Map),
    triple(Individual, rdf:type, Class).

%% get_individual_list_by_class(+Class, +Map, -List) is det
%
% Get a list of individuals of given class
%
% @param Class the class of named individual
% @param Map the semantic environment map 
% @param List the list of the individual
%
get_individual_list_by_class(Class, Map, List) :-
    findall(Individual, (triple(Individual, rdf:type, Class), triple(Individual, knowrob:describedInMap, Map)), List).
    
%% import_semantic_map(+Task, -Map) is det
%
% Load the semantic map and return the map id with namespace
%
% @param Task the file name of the semantice map
% @param Map the semantic environment map 
%
import_semantic_map(Task, Map) :-
    atomic_list_concat(['package://knowrob_ameva/maps/', Task, '.owl'], OwlFile),
    tripledb_load(OwlFile),
    triple(Map, rdf:type, 'http://knowrob.org/kb/knowrob.owl#SemanticEnvironmentMap').
