:- use_module(library(tabling)).
:- use_module(library(clpfd)). 
%To find the general extended edge concentration for a graph call solve_naive_geec(G,Res).
%ATTN must be called with a sufficient number of additional vertices in vertex set. 
%In order to do this pass the graph with edges e(vi, $), where n is a special character needed
%to indicate no outgoing edge

%Random helper functions
member(H,[H|_]).
member(H,[_|T]):- member(H,T).

non_member(H, [H2|T]):- H \== H2, non_member(H,T).
non_member(_,[]).


list_len(L,K):- list_len(L,0,K).
list_len([], K, K).
list_len([_|T], K2, K):- K3 is K2+1, list_len(T,K3,K).

%Finds length of smallest list in a nested list.
list_minimum([H],Min):- list_len(H,Min).
list_minimum([H,H2|T],Min):- list_len(H,K), 
list_len(H2,K2), K =< K2, 
list_minimum([H|T], Min).

list_minimum([H,H2|T], Min):-list_len(H,K), 
list_len(H2,K2), K > K2, 
list_minimum([H2|T], Min).

%Only returns first solution.
list_minimum_result([H],H,Min):- list_len(H,Min).
list_minimum_result([H,H2|T],MinRes,Min):- list_len(H,K), 
list_len(H2,K2), K =< K2, 

%Returns first result.
list_minimum_result([H|T],MinRes, Min).
list_minimum_result([H,H2|T],MinRes, Min):-list_len(H,K), 
list_len(H2,K2), K > K2, 
list_minimum_result([H2|T],MinRes, Min).


%Naive implementation of GEEC.

equiv_vertex_set(G,G2):- vertex_set(G,V), vertex_set(G2,V).

subs([],[]).
subs([H|T], [H|T2]):- subs(T, T2).
subs([_|T], T2):- subs(T,T2).

%No duplicate edges, and we are dealing with DAG.

is_valid_graph(E,V):- vertex_set(E,V), setof(e(X,Y), 
	member(e(X,Y), E), L), list_len(L,K), list_len(E,K),  
	(path(E,V,Z,W) -> \+ path(E,V,W,Z)).

vertex(G,X):- member(e(_,X), G); member(e(X,_), G).


vertex_set(G,V):- setof(X, (vertex(G,X), X \= $), V).

%Must call vertex_set for this to work.
:-table path/4.
path(G,V,X,Y):- member(X,V), 
member(Y,V), 
member(e(X,Y), G).

path(G,V,X,Y):- member(X,V), member(Y,V), 
member(Z,V),
member(e(X,Z), G), 
path(G,V,Z,Y).


%Users do not have any incoming edges and permissions do not have any outgoing edges.
user_perm(G,X,Y):- vertex_set(G,V), 
path(G,V,X,Y), 
\+ member(e(_,X),G), \+ member(e(Y,_),G).

user_perm_map(G,UP):- setof(e(X,Y), user_perm(G,X,Y), UP).

ur_equivalent(G,G2):- user_perm_map(G,UR), user_perm_map(G2,UR).

%Each subset of B is the search space for our solutions.
vertex_base(G,B):- vertex_set(G,V), setof(e(X,Y), check_cond(G,V,e(X,Y)), B). 

check_cond(G,V,e(X,Y)):- member(X,V), member(Y,V), 
X\=Y, \+ path(G,V,Y,X).  


path_equivalent_graphs(G,G2):- vertex_set(G,V), setof(p(X,Y), path(G,V,X,Y), L),
vertex_set(G2,V2),setof(p(Z,W), path(G2,V2,Z,W), L2), subs(L2,L).


%Valid subsets that satisfy path constraint
get_solution_set(G,A):- vertex_set(G,V),
vertex_base(G,B), 
setof(L, (subs(B,L), path_equivalent_graphs(G,L), ur_equivalent(G,L)), A).

%Solves GEEC, returns the size of the smallest possible assignments. Assumes that we have run user_perm_map(X,G).
solve_naive_geec(G,MinRes):- get_solution_set(G,A), list_minimum_result(A,MinRes,Min).



%%?- G = [e(a,d), e(b,d), e(c,$)], solve_naive_geec(G,G2). 
%G = [e(a, d), e(b, d), e(c, $)],
%G2 = [e(a, d), e(b, d)] . 


%It can't handle inputs much larger than this unfortunately, e.g.
%G = [e(a,b),e(a,c),e(a,d),e(f,b),e(f,c),e(f,d),e(k,$)], solve_naive_geec(G,G2).
%runs out of table space...
