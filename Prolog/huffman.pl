%he_generate_huffman_tree([(a,2), (b,3), (c,1)], X), he_generate_symbol_bits_table(X, Y).

he_generate_huffman_tree(SymbolsAndWeights, HuffmanTree) :- 
	initialize_tree(SymbolsAndWeights, Leaf),
	genera_albero(Leaf, HuffmanTree).

initialize_tree([(Symbol, Weight)|SymbolsAndWeights], Leaf) :-
	initialize_tree(SymbolsAndWeights, Temp),
	append([t(([Symbol], Weight), nil, nil)], Temp, Leaf).
initialize_tree([],[]).

genera_albero(SymbolsAndWeights, HuffmanTree) :-
	bubble_sort(SymbolsAndWeights, List),	
	tree_algorithm(List, HuffmanTree).

bubble_sort(List,Sorted):-b_sort(List,[],Sorted).
b_sort([],Acc,Acc).
b_sort([H|T],Acc,Sorted):-bubble(H,T,NT,Max),b_sort(NT,[Max|Acc],Sorted).
   
bubble(t((NodoA,PesoA), LeftA, RightA),[],[],t((NodoA,PesoA), LeftA, RightA)).
bubble(t((NodoA,PesoA), LeftA, RightA), [t((NodoB,Ys), Ax,Bx)|T], [t((NodoB,Ys), Ax,Bx)|NT],Max):-
    PesoA > Ys, !,
    bubble(t((NodoA,PesoA), LeftA, RightA),T,NT,Max).
bubble(t((NodoA,PesoA), LeftA, RightA), [t((NodoB,Ys),Ax,Bx)|T], [t((NodoA,PesoA), LeftA, RightA)|NT],Max):-
    PesoA =< Ys, !,
    bubble(t((NodoB,Ys),Ax,Bx),T,NT,Max).

tree_algorithm([NodoA, NodoB|Rest], HuffmanTree):-
    unisci(NodoA, NodoB, Uniti),
    genera_albero([Uniti|Rest], HuffmanTree),!.

tree_algorithm([t((Nodo,Peso), Left, Right)|[]], t((Nodo,Peso), Left, Right)) :- !.

unisci(t((NodoA,PesoA), LeftA, RightA), t((NodoB,PesoB), LeftB, RightB),  t((Uniti, Peso), t((NodoA,PesoA), LeftA, RightA), t((NodoB,PesoB), LeftB, RightB))):-
    atomic(NodoA),
    atomic(NodoB), !,
    append([NodoA], [NodoB], Uniti),
    Peso is PesoA + PesoB.

unisci(t((NodoA,PesoA), LeftA, RightA), t((NodoB,PesoB), LeftB, RightB),  t((Uniti, Peso), t((NodoA,PesoA), LeftA, RightA), t((NodoB,PesoB), LeftB, RightB))):-
    atomic(NodoA), !,
    append([NodoA], NodoB, Uniti),
    Peso is PesoA + PesoB.

unisci(t((NodoA,PesoA), LeftA, RightA), t((NodoB,PesoB), LeftB, RightB),  t((Uniti, Peso), t((NodoA,PesoA), LeftA, RightA), t((NodoB,PesoB), LeftB, RightB))):-
    atomic(NodoB), !,
    append(NodoA, [NodoB], Uniti),
    Peso is PesoA + PesoB.

unisci(t((NodoA,PesoA), LeftA, RightA), t((NodoB,PesoB), LeftB, RightB),  t((Uniti, Peso), t((NodoA,PesoA), LeftA, RightA), t((NodoB,PesoB), LeftB, RightB))):-
    append(NodoA, NodoB, Uniti),
    Peso is PesoA + PesoB.

print(nil).
print(t((X,Peso), LeftA, RightA)):-
    write(X),
    write(Peso),
    nl,
    print(LeftA),
    print(RightA).

%Encode
he_encode([Car|Rest], HuffmanTree, Bits):-
    find_path(Car, HuffmanTree, Binary),
    he_encode(Rest, HuffmanTree, NewBits),
    append(Binary, NewBits, Bits).
he_encode([], _,[]).

find_path(Car, t(_,_,t((Nodi,_), Left, Right)), Bits):-
    member(Car, Nodi),!,
    find_path(Car, t((Nodi,_),Left,Right), NewBits),
    append([1], NewBits, Bits).

find_path(Car, t(_,t((Nodi,_), Left, Right),_), Bits):-
    member(Car, Nodi),!,
    find_path(Car, t((Nodi,_),Left,Right), NewBits),
    append([0], NewBits, Bits).
find_path(Nodo, t(([Nodo],_), nil, nil), []).

%Decode
he_decode([Bit|Rest], Tree, [NewMessage|Message]):-
    Bit == 0, !,
    left([Bit|Rest],Tree, Remain, NewMessage),
    he_decode(Remain, Tree, Message).

he_decode([Bit|Rest], Tree, [NewMessage|Message]):-
    Bit == 1, !,
    right([Bit|Rest], Tree, Remain, NewMessage),
    he_decode(Remain, Tree, Message).
he_decode([], _,[]).

left([Bit|Rest], t((_,_), Left, _), Remain, Message):-
    Bit == 0,
    Left \== nil, !,
    left(Rest, Left, Remain, Message).

left([Bit|Rest], t((_,_), _, Right), Remain, Message):-
    Bit == 1,
    Right \== nil, !,
    right(Rest, Right, Remain, Message).
left(Bit, t(([Nodo],_), _, _), Bit, Nodo).

right([Bit|Rest], t((_,_), _, Right), Remain, Message):-
    Bit == 1,
    Right \== nil, !,
    right(Rest, Right, Remain, Message).

right([Bit|Rest], t((_,_), Left, _), Remain, Message):-
    Bit == 0,
    Left \== nil, !,
    left(Rest, Left, Remain, Message).

right(Bit, t(([Nodo],_), _, _), Bit, Nodo).


he_generate_symbol_bits_table(HuffmanTree, SymbolBitsTable):-
    scan(HuffmanTree, HuffmanTree, SymbolBitsTable), !.

scan(HuffmanTree, t(([Nodo|Rest],_),_,_), SymbolBitsTable):-
    find_path(Nodo, HuffmanTree, Bits),
    scan(HuffmanTree, t((Rest,_),_,_), NewSymbolBitsTable),
    append(NewSymbolBitsTable, [(Nodo, Bits)], SymbolBitsTable).

scan(_,t(([],_),_,_),_).

%symbol_n_weights(SWs),message(M), he_generate_huffman_tree(SWs, HT), he_encode(M, HT, Bits), he_decode(Bits, HT, M).