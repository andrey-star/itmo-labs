build(M, 0, null, M) :- !.
build(M, N, node(E, L, R), Rest) :-
	N2 is N / 2, N3 is N - N2 - 1,
	build(M, N2, L, [E | RightHalf]),
	build(RightHalf, N3, R, Rest).

tree_build(M, R) :- length(M, N), build(M, N, R, M2).

map_get(node((K, V), _, _), K, V).
map_get(node((X, Y), L, R), K, V) :-
    K < X, map_get(L, K, V);
    K > X, map_get(R, K, V).

map_replace_boolean(node((K, V), L, R), K, V1, node((K, V1), L, R)).
map_replace_boolean(node((X, Y), L, R), K, V, node((X, Y), L1, R1)) :-
    (K < X, map_replace_boolean(L, K, V, L1), R1 = R);
    (K > X, map_replace_boolean(R, K, V, R1), L1 = L).

map_replace(M, K, V, R) :-  map_replace_boolean(M, K, V, R), !; R = M.

% tree_build([(1, k), (2, r), (3, m)], R), map_replace(R, 1, V).