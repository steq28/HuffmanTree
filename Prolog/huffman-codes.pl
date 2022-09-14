% Stefano Quaggio 866504
% Glauco Rinaldi 851624

%%%% -*- Mode: Prolog -*-
:- set_prolog_flag(double_quotes, chars).

% he_decode per decodificare da bits a messaggio usando un albero di Huffman
he_decode([Bit | Rest], Tree, [NewMessage | Message]) :- 
    Bit == 0, !,
    left([Bit | Rest], Tree, Remain, NewMessage),
    he_decode(Remain, Tree, Message).

he_decode([Bit | Rest], Tree, [NewMessage | Message]) :- 
    Bit == 1, !,
    right([Bit | Rest], Tree, Remain, NewMessage),
    he_decode(Remain, Tree, Message).
he_decode([], _, []).

left([Bit | Rest], t((_, _), Left, _), Remain, Message)	:- 
    Bit == 0,
    Left \== nil, !,
    left(Rest, Left, Remain, Message).

left([Bit | Rest], t((_, _), _, Right), Remain, Message) :- 
    Bit == 1,
    Right \== nil, !,
    right(Rest, Right, Remain, Message).
left(Bit, t(([Nodo], _), _, _), Bit, Nodo).

right([Bit | Rest], t((_, _), _, Right), Remain, Message) :- 
    Bit == 1,
    Right \== nil, !,
    right(Rest, Right, Remain, Message).

right([Bit | Rest], t((_, _), Left, _), Remain, Message) :- 
    Bit == 0,
    Left \== nil, !,
    left(Rest, Left, Remain, Message).

right(Bit, t(([Nodo], _), _, _), Bit, Nodo).

% he_encode per codificare un messaggio di testo in bit tramite albero di
% Huffman
he_encode([Car | Rest], HuffmanTree, Bits) :- 
    find_path(Car, HuffmanTree, Binary),
    he_encode(Rest, HuffmanTree, NewBits),
    append(Binary, NewBits, Bits).
he_encode([], _, []).

find_path(Car, t(_, _, t((Nodi, _), Left, Right)), Bits) :- 
    member(Car, Nodi), !,
    find_path(Car, t((Nodi, _), Left, Right), NewBits),
    append([1], NewBits, Bits).

find_path(Car, t(_, t((Nodi, _), Left, Right), _), Bits) :- 
    member(Car, Nodi),!,
    find_path(Car, t((Nodi, _), Left, Right), NewBits),
    append([0], NewBits, Bits).
find_path(Nodo, t(([Nodo], _), nil, nil), []).

% he_encode_file permette di leggere un file e codificare il contenuto
% appoggiandosi alla funzione he_encode
he_encode_file(File, HuffmanTree ,Bits)	:- 
    open(File, read, Stream),
    read_line(Stream, Result),
    close(Stream),
    he_encode(Result, HuffmanTree, Bits).

read_line(Stream, List) :- 
    \+ at_end_of_stream(Stream), !,
    read_line_to_codes(Stream, Line),
    atom_codes(A, Line),
    atom_chars(A, Cs),
    read_line(Stream, Res),
    append(Cs, Res, List).

read_line(Stream, []) :- 
    at_end_of_stream(Stream).

% he_generate_huffman_tree per la creazione di un albero di Huffman
% partendo da una lista di coppie (simbolo, peso)
% la struttura dell'albero è:
% t(Nodo, Peso, Sotto-albero sinistro, Sotto-albero destro)
he_generate_huffman_tree(SymbolsAndWeights, HuffmanTree) :- 
    initialize_tree(SymbolsAndWeights, Leaf),
    genera_albero(Leaf, HuffmanTree).

initialize_tree([(Symbol, Weight) | SymbolsAndWeights], Leaf) :- 
    initialize_tree(SymbolsAndWeights, Temp),
    append([t(([Symbol], Weight), nil, nil)], Temp, Leaf).
initialize_tree([], []).

genera_albero(SymbolsAndWeights, HuffmanTree) :- 
    bubble_sort(SymbolsAndWeights, List),	
    tree_algorithm(List, HuffmanTree).

bubble_sort(List,Sorted) :- bubble_sort_algo(List, [], Sorted).

bubble_sort_algo([], Rest, Rest).
bubble_sort_algo([X | Xs], Rest, Sorted) :-
    confronta(X, Xs, Y, Max),
	bubble_sort_algo(Y, [Max | Rest], Sorted).

confronta(t((NodoA, PesoA), LeftA, RightA), [], [],
          t((NodoA, PesoA), LeftA, RightA)).
confronta(t((NodoA, PesoA), LeftA, RightA),
          [t((NodoB, PesoB), LeftB, RightB) | RestA],
          [t((NodoB, PesoB), LeftB, RightB) | RestB], Max)  :- 
    PesoA > PesoB, !,
    confronta(t((NodoA, PesoA), LeftA, RightA), RestA, RestB, Max).
confronta(t((NodoA, PesoA), LeftA, RightA),
	  [t((NodoB, PesoB), LeftB, RightB) | RestA],
	  [t((NodoA, PesoA), LeftA, RightA) | RestB], Max) :- 
    PesoA =< PesoB, !,
    confronta(t((NodoB, PesoB), LeftB, RightB), RestA, RestB, Max).

tree_algorithm([NodoA, NodoB | Rest], HuffmanTree) :- 
    unisci(NodoA, NodoB, Uniti),
    genera_albero([Uniti | Rest], HuffmanTree), !.

tree_algorithm([t((Nodo, Peso), Left, Right) | []],
	       t((Nodo, Peso), Left, Right)) :- !.

unisci(t((NodoA, PesoA), LeftA, RightA), t((NodoB, PesoB), LeftB, RightB),
       t((Uniti, Peso), t((NodoA, PesoA), LeftA, RightA),
	 t((NodoB, PesoB), LeftB, RightB))) :- 
    atomic(NodoA),
    atomic(NodoB), !,
    append([NodoA], [NodoB], Uniti),
    Peso is PesoA + PesoB.

unisci(t((NodoA, PesoA), LeftA, RightA), t((NodoB, PesoB), LeftB, RightB),
       t((Uniti, Peso), t((NodoA, PesoA), LeftA, RightA),
	 t((NodoB, PesoB), LeftB, RightB))) :- 
    atomic(NodoA), !,
    append([NodoA], NodoB, Uniti),
    Peso is PesoA + PesoB.

unisci(t((NodoA, PesoA), LeftA, RightA), t((NodoB,PesoB), LeftB, RightB),
       t((Uniti, Peso), t((NodoA,PesoA), LeftA, RightA),
	 t((NodoB, PesoB), LeftB, RightB))) :- 
    atomic(NodoB), !,
    append(NodoA, [NodoB], Uniti),
    Peso is PesoA + PesoB.

unisci(t((NodoA, PesoA), LeftA, RightA), t((NodoB, PesoB), LeftB, RightB),
       t((Uniti, Peso), t((NodoA, PesoA), LeftA, RightA),
	 t((NodoB, PesoB), LeftB, RightB))) :- 
    append(NodoA, NodoB, Uniti),
    Peso is PesoA + PesoB.

% he_generate_symbol_bits_table permette di generare
% la codifica di ogni foglia dell'albero di Huffman
he_generate_symbol_bits_table(HuffmanTree, SymbolBitsTable) :- 
    scan(HuffmanTree, HuffmanTree, SymbolBitsTable), !.

scan(HuffmanTree, t(([Nodo | Rest], _), _, _), SymbolBitsTable) :- 
    find_path(Nodo, HuffmanTree, Bits),
    scan(HuffmanTree, t((Rest, _), _, _), NewSymbolBitsTable),
    append(NewSymbolBitsTable, [(Nodo, Bits)], SymbolBitsTable).
scan(_, t(([], _), _, _), _).

% he_print_huffman_tree stampa l'albero estendendolo orizzontalmente
he_print_huffman_tree(HuffmanTree):-
    print_tree(HuffmanTree, 0).

print_tree(t((Nodo, _), L, R), Level) :- 
    NewLevel is Level + 1,
    Spazi is Level * 10,
    print_tree(L, NewLevel),
    tab(Spazi), 
    write(Nodo), nl,
    print_tree(R, NewLevel).
print_tree(nil, _).
