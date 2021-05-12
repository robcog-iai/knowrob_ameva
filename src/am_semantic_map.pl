:- module(am_semantic_map,   
    [
        am_load_semantic_map/2,
        am_get_individual/3,
        am_get_individual_list/3,
        am_get_individual_num/3,
        am_get_translation/5,
        am_get_quaternion/6,
        am_get_pose/9,
        am_get_height/2,
        am_get_depth/2,
        am_get_level_name/2
    ]).


% Get the translation and quaternion of given individual
% am_get_pose(+MapInst, +Individual, -X, -Y, -Z, -QX, -QY, -QZ, -QW)
%
am_get_pose(MapInst, Individual, X, Y, Z, QX, QY, QZ, QW) :-
    am_get_translation(MapInst, Individual, X, Y, Z),
    am_get_quaternion(MapInst, Individual, QX, QY, QZ, QW).

% get the translation of given individual
% am_get_translation(+MapInst, +Individual, -X, -Y, -Z)
%
am_get_translation(MapInst, Individual, X, Y, Z) :-
    triple(Individual, knowrob:describedInMap, MapInst),
    triple(Individual, knowrob:pose, Pose),
    triple(Pose, knowrob:translation, TranslationStr),
    split_string(TranslationStr, " ", "", TranslationList),
    nth1(1, TranslationList, XStr),
    nth1(2, TranslationList, YStr),
    nth1(3, TranslationList, ZStr),
    number_string(X, XStr),
    number_string(Y, YStr),
    number_string(Z, ZStr).

% get the quaternion of given individual
% am_get_quaternion(+MapInst, +Individual, -X, -Y, -Z, -W) 
%
am_get_quaternion(MapInst, Individual, X, Y, Z, W) :-
    triple(Individual, knowrob:describedInMap, MapInst),
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

% get the heigth property of given class
% am_get_height(+Class, -Height)
%
am_get_height(Class, Height) :-
    triple(Class,rdfs:subClassOf, Description),
    triple(Description, owl:onProperty, knowrob:'heightOfObject'),
    triple(Description, owl:hasValue, Height).

% get the heigth property of given class
% am_get_depth(+Class, -Depth) 
%
am_get_depth(Class, Depth) :-
    triple(Class,rdfs:subClassOf, Description),
    triple(Description, owl:onProperty, knowrob:'depthOfObject'),
    triple(Description, owl:hasValue, Depth).

% am_get_level_name(+MapInst, -LevelName)
%
am_get_level_name(MapInst, LevelName) :-
    triple(MapInst, knowrob:'levelName', LevelName).

% get individuals of given class
% am_get_individual(+Class, +MapInst, -Individual)
%
am_get_individual(Class, MapInst, Individual) :-
    triple(Individual, knowrob:describedInMap, MapInst),
    triple(Individual, rdf:type, Class).

% get a list of individuals of given class
% am_get_individual_list(+Class, +MapInst, -IndiList)
%
am_get_individual_list(Class, MapInst, IndiList) :-
    findall(Individual, 
        (
            triple(Individual, rdf:type, Class), 
            triple(Individual, knowrob:describedInMap, MapInst)
        ), 
        IndiList).

% get the totol number of individuals of given class
% am_get_individual_num(+Class, +MapInst, -Num)
%
am_get_individual_num(Class, MapInst, Num) :-
    get_individual_list_by_class(Class, MapInst, IndiList),
    length(IndiList, Num).

% goad the semantic map and return instance of map
% am_load_semantic_map(+Map, -MapInst)
%
am_load_semantic_map(Map, MapInst) :-
    atomic_list_concat(['package://knowrob_ameva/maps/', Map, '_SM.owl'], OwlFile),
    tripledb_load(OwlFile),
    triple(MapInst, rdf:type, 'http://knowrob.org/kb/knowrob.owl#SemanticEnvironmentMap').