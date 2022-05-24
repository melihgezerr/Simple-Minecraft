% melih gezer
% 2020400156
% compiling: yes
% complete: yes
:- ['cmpecraft.pro'].

:- init_from_map.

% 10 points
% manhattan_distance(+A, +B, -Distance) :- .
manhattan_distance([X1,Y1], [X2,Y2], Distance) :-
    abs(X1-X2, Result1),
    abs(Y1-Y2, Result2),
    Distance is Result1+Result2.


% 10 points
% minimum_of_list(+List, -Minimum) :- .
find_min([],Curr,Curr).
find_min([H|T],Temp,Last) :-
    Min is min(H,Temp),
    find_min(T,Min,Last).
minimum_of_list([H|T], Minimum) :- 
    find_min(T, H, Minimum).


% 10 points
% find_nearest_type(+State, +ObjectType, -ObjKey, -Object, -Distance) :- .
find_type(_,_,[],_,TempDistt,TempDistt).

find_type(AgentDict,ObjDict,[Head|Tail],Key,TempDist,LastDist) :-
    manhattan_distance([AgentDict.x,AgentDict.y],[ObjDict.Head.x,ObjDict.Head.y],NewDist),
    Min is min(TempDist,NewDist),
    (not(Min = TempDist) -> NewKey is Head ; NewKey is Key),
    find_type(AgentDict,ObjDict,Tail,NewKey,Min,LastDist).

find_nearest_type([AgentDict,ObjectDict,_], ObjectType, ObjKey, ObjectDict.ObjKey, Distance) :-
    findall(Keys,ObjectDict.Keys.type=ObjectType,[First|Tail]),
    manhattan_distance([AgentDict.x,AgentDict.y],[ObjectDict.First.x,ObjectDict.First.y],Disst),
    find_type(AgentDict,ObjectDict,Tail,ObjKey,Disst,Distance),
    manhattan_distance([AgentDict.x,AgentDict.y],[ObjectDict.ObjKey.x,ObjectDict.ObjKey.y],Distance),
    ObjectDict.ObjKey.type=ObjectType, !.



% 10 points
% navigate_to(+State, +X, +Y, -ActionList, +DepthLimit) :- .
add_left(ActionList,NewActionList) :-
    append([go_left], ActionList, NewActionList).
add_right(ActionList,NewActionList) :-
    append([go_right], ActionList, NewActionList).
add_up(ActionList,NewActionList) :-
    append([go_up], ActionList, NewActionList).
add_down(ActionList,NewActionList) :-
    append([go_down], ActionList, NewActionList).

handle_move_x(AgentDict,X,ActionList,ActionList) :-
    AgentDict.x=X,!.

handle_move_x(AgentDict,X,ActionList,LastList) :-
    AgentDict.x>X,
    add_left(ActionList,NewList),
    New is X+1,
    handle_move_x(AgentDict,New,NewList,LastList).

handle_move_x(AgentDict,X,ActionList,LastList) :-
    AgentDict.x<X,
    add_right(ActionList,NewList),
    New is X-1,
    handle_move_x(AgentDict,New,NewList,LastList).


handle_move_y(AgentDict,Y,ActionList,ActionList) :-
    AgentDict.y=Y,!.

handle_move_y(AgentDict,Y,ActionList,LastList) :-
    AgentDict.y>Y,
    add_up(ActionList,NewList),
    New is Y+1,
    handle_move_y(AgentDict,New,NewList,LastList).

handle_move_y(AgentDict,Y,ActionList,LastList) :-
    AgentDict.y<Y,
    add_down(ActionList,NewList),
    New is Y-1,
    handle_move_y(AgentDict,New,NewList,LastList).

navigate_to([AgentDict,_,_], X, Y, ActionList, DepthLimit) :- 
    manhattan_distance([AgentDict.x,AgentDict.y],[X,Y],NewDist),
    DepthLimit>=NewDist,
    handle_move_x(AgentDict,X,[],Action1),
    handle_move_y(AgentDict,Y,[],Action2),
    append(Action1,Action2,ActionList),!.


% 10 points
% chop_nearest_tree(+State, -ActionList) :- .

chop_nearest_tree([A,O,_],ActionList) :-
    find_nearest_type([A,O,_],tree,_,Object,Distance),
    navigate_to([A,O,_],Object.x,Object.y,ActList1,Distance),
    reverse(ActList1, ActList1R),
    append([left_click_c,left_click_c,left_click_c,left_click_c],ActList1R,ActionListR),
    reverse(ActionListR, ActionList),!.
% 10 points
% mine_nearest_stone(+State, -ActionList) :- .
mine_nearest_stone([A,O,_],ActionList) :-
    find_nearest_type([A,O,_],stone,_,Object,Distance),
    navigate_to([A,O,_],Object.x,Object.y,ActList1,Distance),
    reverse(ActList1, ActList1R),
    append([left_click_c,left_click_c,left_click_c,left_click_c],ActList1R,ActionListR),
    reverse(ActionListR, ActionList),!.
% 10 points
% gather_nearest_food(+State, -ActionList) :- .
gather_nearest_food([A,O,_],ActionList) :-
    find_nearest_type([A,O,_],food,_,Object,Distance),
    navigate_to([A,O,_],Object.x,Object.y,ActList1,Distance),
    reverse(ActList1, ActList1R),
    append([left_click_c],ActList1R,ActionListR),
    reverse(ActionListR, ActionList),!.
% 10 points
% collect_requirements(+State, +ItemType, -ActionList) :- .
clone([],[]).
clone([H|T],[H|Z]):- clone(T,Z).


mine_nearest_cobblestone1([A,O,_],ActionList) :-
    find_nearest_type([A,O,_],cobblestone,_,Object,Distance),
    navigate_to([A,O,_],Object.x,Object.y,ActList1,Distance),
    reverse(ActList1, ActList1R),
    append([left_click_c,left_click_c,left_click_c,left_click_c],ActList1R,ActionListR),
    reverse(ActionListR, ActionList),!.

mine_nearest_cobblestone([A,_,_],ActionList,ActionList) :-
    get_dict(cobblestone,A.inventory,Value),Value>=3,!.

mine_nearest_cobblestone([A,O,T],ActionList,FinalList) :-
    mine_nearest_cobblestone1([A,O,T],ActionList1),
    execute_actions([A,O,T],ActionList1,[A2,O2,T2]),
    append(ActionList,ActionList1,ActionListT),
    mine_nearest_cobblestone([A2,O2,T2],ActionListT,FinalList),!.
    


collect_stick([A,_,_],ActionList) :-
    (get_dict(stick,A.inventory,Value) -> true ; Value is 0),
    Value>=2,
    length(ActionList,0),!.
collect_stick([A,O,T],ActionList) :-
    (get_dict(log,A.inventory,Value) -> true ; Value is 0),
    (Value>=3 -> ActionList = [craft_stick] ; chop_nearest_tree([A,O,T],ActionList)).


collect_stone_pickaxe([A,O,T],ActionList) :-

    (get_dict(stick,A.inventory,Value) -> true ; Value is 0),
    (Value >= 2 -> length(ActionListStick,0) ; collect_stick([A,O,T],ActionListStickT), append(ActionListStickT,[craft_stick],ActionListStick)),
    (length(ActionListStick,0) -> clone([A,O,T],[A1,O1,T1]) ; execute_actions([A,O,T], ActionListStick,[A1,O1,T1])),

    (get_dict(log,A1.inventory,Value2) -> true ; Value2 is 0),
    (Value2>=3 -> length(ActionListLog,0) ; chop_nearest_tree([A1,O1,T1],ActionListLog)),
    (length(ActionListLog,0) -> clone([A1,O1,T1],[A2,O2,T2]) ; execute_actions([A1,O1,T1], ActionListLog,[A2,O2,T2])),

    (get_dict(cobblestone,A2.inventory,Value3) -> true ; Value3 is 0),
    (Value3>=3 -> length(ActionListCobble,0) ; (mine_nearest_stone([A2,O2,T2],ActionListCobble) -> true ; mine_nearest_cobblestone([A2,O2,T2],[],ActionListCobble))),
    
    append(ActionListStick,ActionListLog,Temp),
    append(Temp,ActionListCobble,ActionList),!.

collect_stone_axe([A,O,T],ActionList) :-

    (get_dict(stick,A.inventory,Value) -> true ; Value is 0),
    (Value >= 2 -> length(ActionListStick,0) ; collect_stick([A,O,T],ActionListStickT), append(ActionListStickT,[craft_stick],ActionListStick)),
    (length(ActionListStick,0) -> clone([A,O,T],[A1,O1,T1]) ; execute_actions([A,O,T], ActionListStick,[A1,O1,T1])),

    (get_dict(log,A1.inventory,Value2) -> true ; Value2 is 0),
    (Value2>=3 -> length(ActionListLog,0) ; chop_nearest_tree([A1,O1,T1],ActionListLog)),
    (length(ActionListLog,0) -> clone([A1,O1,T1],[A2,O2,T2]) ; execute_actions([A1,O1,T1], ActionListLog,[A2,O2,T2])),

    (get_dict(cobblestone,A2.inventory,Value3) -> true ; Value3 is 0),
    (Value3>=3 -> length(ActionListCobble,0) ; (mine_nearest_stone([A2,O2,T2],ActionListCobble) -> true ; mine_nearest_cobblestone([A2,O2,T2],[],ActionListCobble))),
    
    append(ActionListStick,ActionListLog,Temp),
    append(Temp,ActionListCobble,ActionList),!.


collect_requirements([A,O,T], ItemType, ActionList) :-
    ItemType = stick,
    collect_stick([A,O,T],ActionList),!.
collect_requirements([A,O,T], ItemType, ActionList) :-
    ItemType = stone_pickaxe,
    collect_stone_pickaxe([A,O,T],ActionList),!.
collect_requirements([A,O,T], ItemType, ActionList) :-
    ItemType = stone_axe,
    collect_stone_axe([A,O,T],ActionList),!.

% 5 points
% find_castle_location(+State, -XMin, -YMin, -XMax, -YMax) :- .
inter([], _, []).
inter([H1|T1], L2, [H1|Res]) :-
    member(H1, L2),
    inter(T1, L2, Res),!.
inter([_|T1], L2, Res) :-
    inter(T1, L2, Res),!.

occupied(X, Y, State) :-
    State = [_, ObjectDict, _],
    findall(Keys,ObjectDict.Keys.x=X,LocationX),
    findall(Keys,ObjectDict.Keys.y=Y,LocationY),
    inter(LocationX,LocationY,Var),
    length(Var,Length),
    (Length = 0 -> true ; false),!.

find_location(State,X,Y) :-

    occupied(X, Y, State),
    Q is X+1, QW is X+2 ,
    R is Y+1, RT is Y+2 ,
    occupied(Q, Y, State),
    occupied(QW, Y, State),
    occupied(X, R, State),
    occupied(X, RT, State),
    occupied(Q, R, State),
    occupied(QW, RT, State) ,!.


find_castle_location(State, Xs, Ys , Xm , Ym ) :-
    width(W), height(H),
    Wi is W-4, He is H-4,
    between(1, Wi, Xs),
    between(1, He, Ys),
    find_location(State, Xs, Ys),
    Xm is Xs+2, Ym is Ys+2,!.


% 15 points
% make_castle(+State, -ActionList) :- .

occupied_castle(X, Y, State) :-
    State = [_, ObjectDict, _],
    findall(Keys,ObjectDict.Keys.x=X,LocationX),
    findall(Keys,ObjectDict.Keys.y=Y,LocationY),
    inter(LocationX,LocationY,Var),
    length(Var,Length),
    (Length = 0 -> true ; Var = [Head|_], (ObjectDict.Head.type = stone ; ObjectDict.Head.type = cobblestone)),!.

find_location_castle(State,X,Y) :-
    occupied_castle(X, Y, State),
    Q is X+1, QW is X+2 ,
    R is Y+1, RT is Y+2 ,
    occupied_castle(Q, Y, State),
    occupied_castle(QW, Y, State),
    occupied_castle(X, R, State),
    occupied_castle(X, RT, State),
    occupied_castle(Q, R, State),
    occupied_castle(QW, RT, State) ,!.

find_for_location(State, Xs, Ys , Xm , Ym ) :-
    width(W), height(H),
    Wi is W-4, He is H-4,
    between(1, Wi, Xs),
    between(1, He, Ys),
    find_location_castle(State, Xs, Ys),
    Xm is Xs+2, Ym is Ys+2,!.

make_castle(State, ActionList) :-
    State = [A,_,_],
    width(W), height(H), Depth is W+H,
    (get_dict(cobblestone,A.inventory,Value) -> true ; Value is 0),
    ( Value < 9 ->
    

((
    mine_nearest_cobblestone(State,[],Act1),
    execute_actions(State,Act1,State1),
    mine_nearest_cobblestone(State1,[],Act2),
    execute_actions(State1,Act2,State2),
    mine_nearest_cobblestone(State2,[],Act3),
    execute_actions(State2,Act3,State3)

    
)
    ->

    true 

    ;

(
    mine_nearest_stone(State,Act1),
    execute_actions(State,Act1,State1),
    mine_nearest_stone(State1,Act2),
    execute_actions(State1,Act2,State2),
    mine_nearest_stone(State2,Act3),
    execute_actions(State2,Act3,State3)
    
))

;

false

)
,
    find_for_location(State3,X0,Y0,_,_),
    append(Act1,Act2,ActX),
    append(ActX,Act3,ActLast),
    XX is X0+1, YY is Y0+1,
    navigate_to(State3,XX,YY,ActionL,Depth),
    append(ActLast,ActionL,ActionLst),
    append(ActionLst,[left_click_c,left_click_c,left_click_c,left_click_c,place_c,left_click_e,left_click_e,left_click_e,left_click_e,place_e,
    left_click_n,left_click_n,left_click_n,left_click_n,place_n,left_click_w,left_click_w,left_click_w,left_click_w,place_w,left_click_s,left_click_s,left_click_s,left_click_s,place_s,
    left_click_ne,left_click_ne,left_click_ne,left_click_ne,place_ne,left_click_nw,left_click_nw,left_click_nw,left_click_nw,place_nw,
    left_click_sw,left_click_sw,left_click_sw,left_click_sw,place_sw,left_click_se,left_click_se,left_click_se,left_click_se,place_se],ActionList).


    



