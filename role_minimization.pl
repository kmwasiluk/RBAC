:- use_module(library(tabling)).
:- use_module(library(clpfd)). 
% TO find biclique cover number (and therefore the size of minimum number of roles) of a graph simply call biclique_cover_number(G,K). 
% If we receive some arbitrary graph as input we should first call user_perm_map(G,UP), and then call BCN predicate.

%Random helper functions
member(H,[H|_]).
member(H,[_|T]):- member(H,T).

non_member(H, [H2|T]):- H \== H2, non_member(H,T).
non_member(_,[]).

%set intersection
intersection([],_,[]).
intersection([H|T], X, [H|T2]):- member(H,X), !, intersection(T,X,T2).
intersection([H|T], X, Y):- non_member(H,X), !, intersection(T,X,Y).

list_len(L,K):- list_len(L,0,K).
list_len([], K, K).
list_len([_|T], K2, K):- K3 is K2+1, list_len(T,K3,K).

vertex(G,X):- member(e(_,X), G); member(e(X,_), G).

vertex_set(G,V):- setof(X, vertex(G,X), V).

path(G,X,Y):- member(e(X,Y), G).
path(G,X,Y):- member(e(X,Z), G), path(G,Z,Y).


%Users do not have any incoming edges and permissions do not have any outgoing edges.
user_perm(G,X,Y):- path(G,X,Y), \+ member(e(_,X),G), \+ member(e(Y,_),G).

user_perm_map(G,UP):- setof(e(X,Y), user_perm(G,X,Y), UP).



%No duplicate edges, and we are dealing with DAG.

is_valid_graph(E,V):- vertex_set(E,V), setof(e(X,Y), member(e(X,Y), E), L), list_len(L,K), list_len(E,K),  path(E,Z,W) -> \+ path(E,W,Z).


equiv_vertex_set(G,G2):- vertex_set(G,V), vertex_set(G2,V).

%Input G is assumed to be bipartite, however the graph should include e(Y,X) for all e(X,Y) 
%since the graph is assumed to be undirected for finding BCN.


biclique_cover_number(G,K):- edge_dual(G,G2), 
complement_graph(G2,G3), 
chromatic_number(G3,K).

edge_dual(G,G2):- induce_undirected(G,U), setof(e(X,Y), (adjacent_edges(U,X,Y)), G2).

%returns graph with directed edges in both directions
induce_undirected(G,U):- setof(e(X,Y), (undir_member(e(X,Y), G)), U).

undir_member(e(X,Y), G):- member(e(X,Y), G).
undir_member(e(X,Y), G):- member(e(Y,X), G).

adjacent_edges(G,e(X1,X2),e(X2,Y2)):- member(e(X1,X2), G),
member(e(X2,Y2), G).


complement_graph(G,G2):- vertex_set(G,B),
setof(e(X,Y), (member(X,B), member(Y,B), \+member(e(X,Y), G), X \= Y), G2).

g_prime_complement(G,G3):- edge_dual(G,G2), complement_graph(G2,G3).


%Predicates for finding a minimum coloring and chromatic number of a graph.

color_list(0,[]).
color_list(K,[K|T]):- K2 is K-1, color_list(K2,T).

color_graph_K_colors(G, K, Colors, R):- vertex_set(G,V),
color_list(K,Colors),
setof(color(X,_), member(X,V), R),
constrain(G,R) -> assign_color(Colors,R).


%Constraining adjacent vertices to be different colors.
constrain([], _).
constrain([e(X,Y)|T], Res):- member(color(X,C1), Res),
member(color(Y,C2), Res),
dif(C1,C2),
constrain(T,Res).

assign_color(_,[]).
assign_color(Colors,[color(_,C)| T]):-
member(C,Colors),
assign_color(Colors,T).


%Should use arguments (G, Coloring, _, 1) to start.
%First case: There is no (K2) coloring, so we increment.
min_coloring(G, Coloring, K,  K2):- 
(\+ color_graph_K_colors(G,  K2, _, Coloring) 
    -> (K is K2 + 1, K3 is K + 1,  min_coloring(G, Coloring, K3,   K))).


%Now there is a K2 coloring, so return it.
min_coloring(G,Coloring,_,K2):-
color_graph_K_colors(G, K2, _, Coloring), !.


%Finds min coloring and then returns the number of colors.
chromatic_number(G,W):- min_coloring(G, Coloring, _, 0), 
findall(X, (member(color(_,X), Coloring)), L),
setof(X, member(X,L), Z),
list_len(Z,W).


