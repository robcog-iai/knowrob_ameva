:- module(ue_semantic_map,   
    [
        load_semantic_map/2,
        individual/3,
        individual_list/3,
        translation/5,
        quaternion/6,
        pose/9,
        height/2,
        depth/2
    ]).


% Get the translation and quaternion of given individual
pose(MapInst, Individual, X, Y, Z, QX, QY, QZ, QW) :-
    translation(MapInst, Individual, X, Y, Z),
    quaternion(MapInst, Individual, QX, QY, QZ, QW).

% get the translation of given individual
translation(MapInst, Individual, X, Y, Z) :-
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
quaternion(MapInst, Individual, X, Y, Z, W) :-
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
height(Class, Height) :-
    triple(Class,rdfs:subClassOf, Description),
    triple(Description, owl:onProperty, knowrob:'heightOfObject'),
    triple(Description, owl:hasValue, Height).

% get the heigth property of given class
depth(Class, Depth) :-
    triple(Class,rdfs:subClassOf, Description),
    triple(Description, owl:onProperty, knowrob:'depthOfObject'),
    triple(Description, owl:hasValue, Depth).

% get individuals of given class
individual(Class, MapInst, Individual) :-
    triple(Individual, knowrob:describedInMap, MapInst),
    triple(Individual, rdf:type, Class).

% get a list of individuals of given class
individual_list(Class, MapInst, IndiList) :-
    findall(Individual, 
        (
            triple(Individual, rdf:type, Class), 
            triple(Individual, knowrob:describedInMap, MapInst)
        ), 
        IndiList).

% get the totol number of individuals of given class
individual_num(Class, MapInst, Num) :-
    get_individual_list_by_class(Class, MapInst, IndiList),
    length(IndiList, Num).

% goad the semantic map and return instance of map
load_semantic_map(Task, MapInst) :-
    atomic_list_concat(['package://knowrob_ameva/maps/', Task, '.owl'], OwlFile),
    tripledb_load(OwlFile),
    triple(MapInst, rdf:type, 'http://knowrob.org/kb/knowrob.owl#SemanticEnvironmentMap').