% SICStus PROLOG: Declaracoes iniciais
:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

% SICStus PROLOG: definicoes iniciais
:- op( 800,fx,se ).
:- op( 800,fx,facto ).
:- op( 700,xfx,entao ).
:- op( 300,xfy,ou ).
:- op( 200,xfy,e ).
:- op( 900,xfx,com).
:- op( 900,xfx,porque).
:- op( 900,xfx,de).
:- op( 900,xfx,::).
:- op( 900,xfx,:::).

:- dynamic facto/2.
:- dynamic utente/5.
:- dynamic '::'/2.
:- dynamic ':::'/2.


%--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------				
%								  BASE DE CONHECIMENTO
%--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

%**************************************************************************************************
%                                             UTENTES
%**************************************************************************************************
% Extensao do predicado utente: #idUt, Nome, Idade, Genero, Morada -> {V,F}
utente(u1, rita_pinto, 41, feminino, braga).
utente(u2, carlos_moreira, 12, masculino, vilareal).
utente(u3, rui_sousa, 12, masculino, leiria).
utente(u4, maria_tavares, 84, feminino, braga).
utente(u5, ivone_lopes, 36, feminino, lisboa).
utente(u6, matilde_neves, 50, feminino, porto).
utente(u7, andre_fernandes, 64, masculino, leiria).

%**************************************************************************************************
%                                             FACTOS
%**************************************************************************************************
(facto('u1', 'stress_ligeiro'))::'tem sintomas'.
facto('u2', 'stress_cronico')::'tem sintomas'.
facto('u3', 'nauseas')::'tem sintomas'.
facto('u3', 'anorexia_ligeira')::'tem sintomas'.
facto('u3','insonia')::'tem sintomas'.
facto('u3','hipertensao')::'tem sintomas'.
facto('u3','edema')::'tem sintomas'.
facto('u3','oliguria')::'tem sintomas'.
facto('u4', 'nauseas')::'tem sintomas'.
facto('u4', 'vomitos')::'tem sintomas'.
facto('u4', 'fadiga')::'tem sintomas'.
facto('u4', 'anorexia_ligeira')::'tem sintomas'.
facto('u4','insonia')::'tem sintomas'.

%**************************************************************************************************
%                         REGRAS DE PRODUÇÃO - SE ... ENTAO
%**************************************************************************************************
%--------------------------------------------------------------------------------------------------
%  se 'SINTOMA' entao 'DIAGNOSTICO' :: ( 'GRAU', 'MEDICAMENTO') 
%--------------------------------------------------------------------------------------------------
se (ID, 'stress_ligeiro') entao (ID,'ansiedade') :: ('diagnostico certo' com 'ansiolitico').
se ((ID, 'ansiedade_freq' ou 'stress_cronico')) entao (ID, 'anorexia') :: 
	('diagnostico provavel' com 'psicoterapia').
se ((ID,'nauseas' ou 'vomitos' ou 'fadiga' ou 'anorexia_ligeira')) entao (ID,'nefrite') :: 
	('diagnostico muito provavel' com 'dieta alimentar').
se ((ID,'nefrite' e 'insonia' e 'hipertensao')) entao (ID,'nefrite glomerular') :: 
	('diagnostico muito provavel' com 'anti-inflamatorio').
se ((ID,'nefrite glomerular' e 'edema' e 'oliguria')) entao (ID,'sindrome nefritica') :: 
	('diagnostico provavel' com 'imunossupressores').



%**************************************************************************************************
%                                      PREDICADOS AUXILIARES
%**************************************************************************************************
%---------------------------------- NAO ------------------------------------
% Extensao do predicado nao: Questao -> {V,F}
nao(Q) :- Q, !, fail.
nao(Q).

%------------------------------ COMPRIMENTO -------------------------------- 
% Extensao do predicado comprimento: lista, comprimento-> {V,F}
comprimento([], 0).
comprimento([A|B], R) :- comprimento(B,X), R is X+1.


%------------------------------ CONCATENAR -------------------------------- 
% Extensao do predicado concatenar: L1,L2, NL -> {V,F}
concatenar([],X,X).
concatenar([X|L1],L2,[X|R]):- concatenar(L1,L2,R).

%------------------------------ SOLUCOES ---------------------------------- 
% Extensao do predicado solucoes: A,T,S -> {V,F}
solucoes(X,T,S):-findall(X,T,S).

%--------------------------------- PERTENCE ----------------------------------
%Extensao do predicado pertence: X,L -> {V,F}     
pertence(X,[X|L]).
pertence(X,[Y|L]):-pertence(X,L).


%------------------------------- SEM REPETIDOS -----------------------------
%Extensao do predicado sem_repetidos: L,S -> {V,F}
sem_repetidos([X],[X]).
sem_repetidos([X|L], S):- pertence(X,L), sem_repetidos(L, S).
sem_repetidos([X|L], [X|S]):- nao(pertence(X,L)), sem_repetidos(L, S).


%--------------------------------- MAXIMO ---------------------------------
% Extensao do predicado maximo: L,MAX -> {V,F}
maximo([X],X).
maximo([X|L1],X):-maximo(L1, MAX), (X > MAX).
maximo([X|L1],MAX):-maximo(L1, MAX), (X =< MAX).


%--------------------------------- TESTE ---------------------------------
% Extensão do predicado teste: L -> {V,F}
teste([]).
teste([R|LR]):-
	R,
	teste(LR).


%--------------------------------- INSERE SINTOMA------------------------------
% Extensao do predicado insere: Facto, Sintoma -> {V,F}
insere(ID, D):-
	assert((facto(ID, D)) :: 'tem sintomas').		
insere(ID, D):-
	retract((facto(ID, D)) :: 'tem sintomas'),!,fail.





%--------------------------------- INSERIR -------------------------------
% Extensao do predicado insere: Facto, Grau -> {V,F}
% Este possibilita a inserção de conhecimento.
inserir(F,G):-
	assert((facto F) :: G).		
inserir(F,G):-
	retract((facto F) :: G),!,fail.	

%--------------------------------- REMOVER -------------------------------
% Extensao do predicado remove: Facto, Grau -> {V,F}
% Este perdicado possibilita a remoção de conhecimento.
remover(F,G):-
	retract((facto F) :: G).	
remover(F,G):-
	assert((facto F) :: G ),!,fail.






%---------------------------
% Invariante que impossibilita a introdução de sintomas repetidos

+(facto(ID,D) :: C1) ::: (solucoes((ID,D), (facto(ID,D) :: C2), X), comprimento(X, H), H==1).






%**************************************************************************************************
%                     REGRAS DE PRODUÇÃO - BACKWARD CHAINING
%**************************************************************************************************
% Extensao do meta-predicado rp: ID, Diagnóstico, Grau, Explicacao, Tratamento -> {V,F}

rp(ID, D, G, G de D, []):-nonvar(D),
	(facto(ID, D))::G.
rp(ID, D, G, (D com G) porque Ex, [X|L]):-(se (ID, C) entao (ID, D))::(Gr com X),
	rp(ID, C, Gc, Ex, L), grau_multiplo(Gr, Gc, G).

rp(ID,(C1 e C2), G,(E1 e E2),Lista):-
nonvar(C1),nonvar(C2),
rp(ID, C1, G1, E1, L1),
rp(ID, C2, G2, E2, L2),
menor(G1,G2,G),
concatenar(L1,L2,Lista).

rp(ID,(C1 ou C2), G,(E1 e E2),Lista):-
nonvar(C1),nonvar(C2),
rp(ID,C1,G1,E1,L1),
rp(ID,C2,G2,E2,L2),
maior(G1,G2,G),
concatenar(L1,L2,Lista).

rp(ID,(C1 ou C2), G1, E1, L1):-
nonvar(C1),nonvar(C2),
rp(ID,C1,G1, E1, L1),
nao(rp(ID,C2, G2, E2, L2)).

rp(ID,(C1 ou C2), G2, E2, L2):-
nonvar(C1),nonvar(C2),
rp(ID, C2, G2, E2, L2),
nao(rp(ID, C1, G1, E1, L1)).

%**************************************************************************************************
%                     REGRAS DE PRODUÇÃO - FORWARD CHAINING
%**************************************************************************************************
% Extensao do meta-predicado rpf: ID, Diagnóstico, Grau, Explicacao, Tratamento -> {V,F},

derivar :- rpf(ID, X, G, E, T ), fail.
derivar.


rpf(ID, D, G, (D com G) porque (Ex com G), T1):-
	(se (ID, C) entao (ID, D)) :: (G1 com T1),
	composicao(ID, C, G2, Ex, T2),
	grau_multiplo(G1, G2, G),
	inserir((facto(ID, D)) :: G).
	
inserir((facto(ID, F)) :: G):-
	nao((facto(ID, F)) :: G),
	assert((facto(ID, F)) :: G).

inserir(facto(ID, F) :: G):-
	(facto(ID, F)) :: G.

composicao(ID, C, G, (C com G), []):-
	(facto(ID, C)) :: G.

composicao(ID, C, 'tem sintomas', ('tem sintomas' de C), []) :- 
    (facto (ID, C))::'tem sintomas'.

composicao(ID, (C1 e C2), G, (E1 e E2), [T1|T2]):-
	composicao(ID, C1, G1, E1, T1),
	composicao(ID, C2, G2, E2, T2),	
	menor(G1, G2, G).

composicao(ID, (C1 ou C2), G, (E1 ou E2), [T1|T2]):-
	composicao(ID, C1, G1, E1, T1),
	composicao(ID, C2, G2, E2, T2),	
	maior(G1, G2, G).

composicao(ID, (C1 ou C2), G1, E1, T1):-
	composicao(ID, C1, G1, E1, T1),
	nao(composicao(ID, C2, G2, E2, T2)).

composicao(ID, (C1 ou C2), G2, E2, T2):-
	composicao(ID, C2, G2, E2, T2),
	nao(composicao(ID, C1, G1, E1, T1)).


%**************************************************************************************************
%                                          FUNCIONALIDADES
%**************************************************************************************************
%--------------------------------------------------------------------------------------------------
%  consultar HISTORICO por utente (Sintomas, Diagnósticos e/ou Tratamentos) - BACKWARD CHAINING
%--------------------------------------------------------------------------------------------------
%Extensao do predicado cs_historico_utente: CS, IdU, S -> {V,F}
% CS pode ser 1 para consultar os sintomas
% CS pode ser 2 para consultar os diagnosticos
% CS pode ser 3 para consultar os tratamentos
% CS pode ser 4 para consultar os sintomas, diagnosticos e tratamentos

cs_historico_utente(CS, IdU, S):- CS=1, solucoes((IdU, N), utente(IdU, N, _, _, _), L1),
	solucoes(D, (facto(IdU,D)::'tem sintomas'), L2), concatenar(L1,L2,S).
cs_historico_utente(CS, IdU, S):- CS=2, solucoes((IdU, N), utente(IdU, N, _, _, _), L1), 
	solucoes(D, rp(IdU, D, G, E, T), L2), concatenar(L1,L2,S).
cs_historico_utente(CS, IdU, S):- CS=3, solucoes((IdU, N), utente(IdU, N, _, _, _), L1), 
	solucoes(X, rp(IdU, D, G, E, [X|L]), L2), concatenar(L1,L2,S).
cs_historico_utente(CS, IdU, S):- CS=4, solucoes((IdU, N), utente(IdU, N, _, _, _), L1),  
	solucoes(('sintomas' de D1), (facto(IdU,D1)::'tem sintomas', rp(IdU, D1, _, _, _)), L2),  
	solucoes(('diagnostico' de D2), (facto(IdU,D1)::'tem sintomas', rp(IdU, D2, _, _, _)), L3), sem_repetidos(L3,R3),	
	solucoes(('tratamento' de X), (facto(IdU,D1)::'tem sintomas', rp(IdU, D2, G, E, [X|L])), L4), sem_repetidos(L4,R4),
 	concatenar(L1,L2,L5), concatenar(L5,R3,L6), concatenar(L6,R4,S).


%--------------------------------------------------------------------------------------------------
%  consultar HISTORICO por utente (Sintomas, Diagnósticos e/ou Tratamentos) - FORWARD CHAINING
%--------------------------------------------------------------------------------------------------
%Extensao do predicado cs_historico_utente_f: CS, IdU, S -> {V,F}
% CS pode ser 1 para consultar os sintomas
% CS pode ser 2 para consultar os diagnosticos
% CS pode ser 3 para consultar os tratamentos
% CS pode ser 4 para consultar os sintomas, diagnosticos e tratamentos

cs_historico_utente_f(CS, IdU, S):- CS=1, solucoes((IdU, N), utente(IdU, N, _, _, _), L1),
	solucoes(D, (facto(IdU,D)::'tem sintomas'), L2), sem_repetidos(L2,L3), concatenar(L1,L3,S).
cs_historico_utente_f(CS, IdU, S):- CS=2, solucoes((IdU, N), utente(IdU, N, _, _, _), L1), 
	solucoes(D, rpf(IdU, D, G, E, T), L2), concatenar(L1,L2,S).
cs_historico_utente_f(CS, IdU, S):- CS=3, solucoes((IdU, N), utente(IdU, N, _, _, _), L1), 
	solucoes(T, rpf(IdU, D, G, E, T), L2), concatenar(L1,L2,S).
cs_historico_utente_f(CS, IdU, S):- CS=4, solucoes((IdU, N), utente(IdU, N, _, _, _), L1),  
	solucoes(('sintomas' de D1), (facto(IdU,D1)::'tem sintomas', rpf(IdU, D1, _, _, _)), L2),   
	solucoes(('diagnostico' de D2), (facto(IdU,D1)::'tem sintomas', rpf(IdU, D2, _, _, _)), L3), sem_repetidos(L3,R3),	
	solucoes(('tratamento' de T), (facto(IdU,D1)::'tem sintomas', rpf(IdU, D2, G, E, T)), L4), sem_repetidos(L4,R4),
 	concatenar(L1,L2,L5), concatenar(L5,R3,L6), concatenar(L6,R4,S).


%--------------------------------------------------------------------------------------------------
%  consultar utentes que tem um determinado diagnostico - BACKWARD CHAINING 
%--------------------------------------------------------------------------------------------------
%Extensao do predicado utente_doenca: D, S -> {V,F}

utente_doenca(D, S):-solucoes((IdU, N), (utente(IdU, N, _, _, _),rp(IdU, D, _, _, _)), S).

%--------------------------------------------------------------------------------------------------
%  consultar utentes que tem um determinado diagnostico - FORWARD CHAINING 
%--------------------------------------------------------------------------------------------------
%Extensao do predicado utente_doenca_f: D, S -> {V,F}

utente_doenca_f(D, S):-solucoes((IdU, N), (utente(IdU, N, _, _, _),rpf(IdU, D, _, _, _)), S).

%--------------------------------------------------------------------------------------------------
%  evolução do conhecimento
%--------------------------------------------------------------------------------------------------

% Extensao do predicado inserir_sintomas: ID, D -> {V,F}

inserir_sintomas(ID, D):- 
    solucoes(Invariante, +(facto(ID, D) :: 'tem sintomas') ::: Invariante, S),
    insere(ID, D),
    teste(S).

%--------------------------------- REMOVER SINTOMA------------------------------
% Extensao do predicado remove_sintomas: Facto,Sintoma -> {V,F}

remover_sintomas(ID,D):-
    (facto(ID,D)) :: D,
    solucoes(Invariante,-((facto(ID,D)) :: D) ::: Invariante,S),
    remover(ID,D),
    teste(S).	

%**************************************************************************************************
%           GRAU MULTIPLO - indica a probabilidade de 2 acontecimentos sequenciais 
%**************************************************************************************************
%--------------------------------------------------------------------------------------------------
% Um dos acontecimentos é: TEM SINTOMAS
%--------------------------------------------------------------------------------------------------
grau_multiplo('tem sintomas', 'tem sintomas', 'tem sintomas').
grau_multiplo('tem sintomas', 'diagnostico certo', 'diagnostico certo').
grau_multiplo('diagnostico certo', 'tem sintomas', 'diagnostico certo').
grau_multiplo('tem sintomas', 'diagnostico muito provavel', 'diagnostico muito provavel').
grau_multiplo('diagnostico muito provavel', 'tem sintomas', 'diagnostico muito provavel').
grau_multiplo('tem sintomas', 'diagnostico provavel', 'diagnostico provavel').
grau_multiplo('diagnostico provavel', 'tem sintomas', 'diagnostico provavel').
grau_multiplo('tem sintomas', 'diagnostico algo provavel', 'diagnostico algo provavel').
grau_multiplo('diagnostico algo provavel', 'tem sintomas', 'diagnostico algo provavel').
grau_multiplo('tem sintomas', 'diagnostico pouco provavel', 'diagnostico pouco provavel').
grau_multiplo('diagnostico pouco provavel', 'tem sintomas', 'diagnostico pouco provavel').
%--------------------------------------------------------------------------------------------------
% Um dos acontecimentos é: DIAGNÓSTICO CERTO 
%--------------------------------------------------------------------------------------------------
grau_multiplo('diagnostico certo', 'diagnostico certo', 'diagnostico certo').
grau_multiplo('diagnostico certo', 'diagnostico muito provavel', 'diagnostico muito provavel').
grau_multiplo('diagnostico muito provavel', 'diagnostico certo', 'diagnostico muito provavel').
grau_multiplo('diagnostico certo', 'diagnostico provavel', 'diagnostico provavel').
grau_multiplo('diagnostico provavel', 'diagnostico certo', 'diagnostico provavel').
grau_multiplo('diagnostico certo', 'diagnostico algo provavel', 'diagnostico algo provavel').
grau_multiplo('diagnostico algo provavel', 'diagnostico certo', 'diagnostico algo provavel').
grau_multiplo('diagnostico certo', 'diagnostico pouco provavel', 'diagnostico pouco provavel').
grau_multiplo('diagnostico pouco provavel', 'diagnostico certo', 'diagnostico pouco provavel').
%--------------------------------------------------------------------------------------------------
% Um dos acontecimentos é: DIAGNÓSTICO MUITO PROVAVEL 
%--------------------------------------------------------------------------------------------------
grau_multiplo('diagnostico muito provavel', 'diagnostico muito provavel', 'diagnostico provavel').
grau_multiplo('diagnostico muito provavel', 'diagnostico provavel', 'diagnostico provavel').
grau_multiplo('diagnostico provavel', 'diagnostico muito provavel', 'diagnostico provavel').
grau_multiplo('diagnostico muito provavel', 'diagnostico algo provavel', 'diagnostico provavel').
grau_multiplo('diagnostico algo provavel', 'diagnostico muito provavel', 'diagnostico provavel').
grau_multiplo('diagnostico muito provavel', 'diagnostico pouco provavel', 'diagnostico algo provavel').
grau_multiplo('diagnostico pouco provavel', 'diagnostico muito provavel', 'diagnostico algo provavel').
%--------------------------------------------------------------------------------------------------
% Um dos acontecimentos é: DIAGNÓSTICO PROVAVEL 
%--------------------------------------------------------------------------------------------------
grau_multiplo('diagnostico provavel', 'diagnostico provavel', 'diagnostico algo provavel').
grau_multiplo('diagnostico provavel', 'diagnostico algo provavel', 'diagnostico algo provavel').
grau_multiplo('diagnostico algo provavel', 'diagnostico provavel', 'diagnostico algo provavel').
grau_multiplo('diagnostico provavel', 'diagnostico pouco provavel', 'diagnostico algo provavel').
grau_multiplo('diagnostico pouco provavel', 'diagnostico provavel', 'diagnostico algo provavel').
%--------------------------------------------------------------------------------------------------
% Um dos acontecimentos é: DIAGNÓSTICO ALGO PROVAVEL 
%--------------------------------------------------------------------------------------------------
grau_multiplo('diagnostico algo provavel', 'diagnostico algo provavel', 'diagnostico pouco provavel').
grau_multiplo('diagnostico algo provavel', 'diagnostico pouco provavel', 'diagnostico pouco provavel').
grau_multiplo('diagnostico pouco provavel', 'diagnostico algo provavel', 'diagnostico pouco provavel').
%--------------------------------------------------------------------------------------------------
% Um dos acontecimentos é: DIAGNÓSTICO POUCO PROVAVEL 
%--------------------------------------------------------------------------------------------------
grau_multiplo('diagnostico pouco provavel', 'diagnostico pouco provavel', 'diagnostico pouco provavel').


%**************************************************************************************************
%              MENOR - indica a menor probabilidade de 2 acontecimentos 
%**************************************************************************************************
%--------------------------------------------------------------------------------------------------
% Um dos acontecimentos é: TEM SINTOMAS
%--------------------------------------------------------------------------------------------------
menor('tem sintomas', 'tem sintomas', 'tem sintomas').
menor('tem sintomas', 'diagnostico certo', 'diagnostico certo').
menor('diagnostico certo', 'tem sintomas', 'diagnostico certo').
menor('tem sintomas', 'diagnostico muito provavel', 'diagnostico muito provavel').
menor('diagnostico muito provavel', 'tem sintomas', 'diagnostico muito provavel').
menor('tem sintomas', 'diagnostico provavel', 'diagnostico provavel').
menor('diagnostico provavel', 'tem sintomas', 'diagnostico provavel').
menor('tem sintomas', 'diagnostico algo provavel', 'diagnostico algo provavel').
menor('diagnostico algo provavel', 'tem sintomas', 'diagnostico algo provavel').
menor('tem sintomas', 'diagnostico pouco provavel', 'diagnostico pouco provavel').
menor('diagnostico pouco provavel', 'tem sintomas', 'diagnostico pouco provavel').
%--------------------------------------------------------------------------------------------------
% Um dos acontecimentos é: DIAGNÓSTICO CERTO 
%--------------------------------------------------------------------------------------------------
menor('diagnostico certo', 'diagnostico certo', 'diagnostico certo').
menor('diagnostico certo', 'diagnostico muito provavel', 'diagnostico muito provavel').
menor('diagnostico muito provavel', 'diagnostico certo', 'diagnostico muito provavel').
menor('diagnostico certo', 'diagnostico provavel', 'diagnostico provavel').
menor('diagnostico provavel', 'diagnostico certo', 'diagnostico provavel').
menor('diagnostico certo', 'diagnostico algo provavel', 'diagnostico algo provavel').
menor('diagnostico algo provavel', 'diagnostico certo', 'diagnostico algo provavel').
menor('diagnostico certo', 'diagnostico pouco provavel', 'diagnostico pouco provavel').
menor('diagnostico pouco provavel', 'diagnostico certo', 'diagnostico pouco provavel').
%--------------------------------------------------------------------------------------------------
% Um dos acontecimentos é: DIAGNÓSTICO MUITO PROVAVEL 
%--------------------------------------------------------------------------------------------------
menor('diagnostico muito provavel', 'diagnostico muito provavel', 'diagnostico muito provavel').
menor('diagnostico muito provavel', 'diagnostico provavel', 'diagnostico provavel').
menor('diagnostico provavel', 'diagnostico muito provavel', 'diagnostico provavel').
menor('diagnostico muito provavel', 'diagnostico algo provavel', 'diagnostico algo provavel').
menor('diagnostico algo provavel', 'diagnostico muito provavel', 'diagnostico algo provavel').
menor('diagnostico muito provavel', 'diagnostico pouco provavel', 'diagnostico pouco provavel').
menor('diagnostico pouco provavel', 'diagnostico muito provavel', 'diagnostico pouco provavel').
%--------------------------------------------------------------------------------------------------
% Um dos acontecimentos é: DIAGNÓSTICO PROVAVEL 
%--------------------------------------------------------------------------------------------------
menor('diagnostico provavel', 'diagnostico provavel', 'diagnostico provavel').
menor('diagnostico provavel', 'diagnostico algo provavel', 'diagnostico algo provavel').
menor('diagnostico algo provavel', 'diagnostico provavel', 'diagnostico algo provavel').
menor('diagnostico provavel', 'diagnostico pouco provavel', 'diagnostico pouco provavel').
menor('diagnostico pouco provavel', 'diagnostico provavel', 'diagnostico pouco provavel').
%--------------------------------------------------------------------------------------------------
% Um dos acontecimentos é: DIAGNÓSTICO ALGO PROVAVEL 
%--------------------------------------------------------------------------------------------------
menor('diagnostico algo provavel', 'diagnostico algo provavel', 'diagnostico algo provavel').
menor('diagnostico algo provavel', 'diagnostico pouco provavel', 'diagnostico pouco provavel').
menor('diagnostico pouco provavel', 'diagnostico algo provavel', 'diagnostico pouco provavel').
%--------------------------------------------------------------------------------------------------
% Um dos acontecimentos é: DIAGNÓSTICO POUCO PROVAVEL 
%--------------------------------------------------------------------------------------------------
menor('diagnostico pouco provavel', 'diagnostico pouco provavel', 'diagnostico pouco provavel').


%**************************************************************************************************
%              MAIOR - indica a maior probabilidade de 2 acontecimentos 
%**************************************************************************************************
%--------------------------------------------------------------------------------------------------
% Um dos acontecimentos é: TEM SINTOMAS
%--------------------------------------------------------------------------------------------------
maior('tem sintomas', 'tem sintomas', 'tem sintomas').
maior('tem sintomas', 'diagnostico certo', 'tem sintomas').
maior('diagnostico certo', 'tem sintomas', 'diagnostico certo').
maior('tem sintomas', 'diagnostico muito provavel', 'tem sintomas').
maior('diagnostico muito provavel', 'tem sintomas', 'tem sintomas').
maior('tem sintomas', 'diagnostico provavel', 'tem sintomas').
maior('diagnostico provavel', 'tem sintomas', 'diagnostico certo').
maior('tem sintomas', 'diagnostico algo provavel', 'tem sintomas').
maior('diagnostico algo provavel', 'tem sintomas', 'tem sintomas').
maior('tem sintomas', 'diagnostico pouco provavel', 'tem sintomas').
maior('diagnostico pouco provavel', 'tem sintomas', 'tem sintomas').
%--------------------------------------------------------------------------------------------------
% Um dos acontecimentos é: DIAGNÓSTICO CERTO 
%--------------------------------------------------------------------------------------------------
menor('diagnostico certo', 'diagnostico certo', 'diagnostico certo').
menor('diagnostico certo', 'diagnostico muito provavel', 'diagnostico certo').
menor('diagnostico muito provavel', 'diagnostico certo', 'diagnostico certo').
menor('diagnostico certo', 'diagnostico provavel', 'diagnostico certo').
menor('diagnostico provavel', 'diagnostico certo', 'diagnostico certo').
menor('diagnostico certo', 'diagnostico algo provavel', 'diagnostico certo').
menor('diagnostico algo provavel', 'diagnostico certo', 'diagnostico certo').
menor('diagnostico certo', 'diagnostico pouco provavel', 'diagnostico certo').
menor('diagnostico pouco provavel', 'diagnostico certo', 'diagnostico certo').
%--------------------------------------------------------------------------------------------------
% Um dos acontecimentos é: DIAGNÓSTICO MUITO PROVAVEL 
%--------------------------------------------------------------------------------------------------
menor('diagnostico muito provavel', 'diagnostico muito provavel', 'diagnostico muito provavel').
menor('diagnostico muito provavel', 'diagnostico provavel', 'diagnostico muito provavel').
menor('diagnostico provavel', 'diagnostico muito provavel', 'diagnostico muito provavel').
menor('diagnostico muito provavel', 'diagnostico algo provavel', 'diagnostico muito provavel').
menor('diagnostico algo provavel', 'diagnostico muito provavel', 'diagnostico muito provavel').
menor('diagnostico muito provavel', 'diagnostico pouco provavel', 'diagnostico muito provavel').
menor('diagnostico pouco provavel', 'diagnostico muito provavel', 'diagnostico muito provavel').
%--------------------------------------------------------------------------------------------------
% Um dos acontecimentos é: DIAGNÓSTICO PROVAVEL 
%--------------------------------------------------------------------------------------------------
menor('diagnostico provavel', 'diagnostico provavel', 'diagnostico provavel').
menor('diagnostico provavel', 'diagnostico algo provavel', 'diagnostico provavel').
menor('diagnostico algo provavel', 'diagnostico provavel', 'diagnostico provavel').
menor('diagnostico provavel', 'diagnostico pouco provavel', 'diagnostico provavel').
menor('diagnostico pouco provavel', 'diagnostico provavel', 'diagnostico provavel').
%--------------------------------------------------------------------------------------------------
% Um dos acontecimentos é: DIAGNÓSTICO ALGO PROVAVEL 
%--------------------------------------------------------------------------------------------------
menor('diagnostico algo provavel', 'diagnostico algo provavel', 'diagnostico algo provavel').
menor('diagnostico algo provavel', 'diagnostico pouco provavel', 'diagnostico algo provavel').
menor('diagnostico pouco provavel', 'diagnostico algo provavel', 'diagnostico algo provavel').
%--------------------------------------------------------------------------------------------------
% Um dos acontecimentos é: DIAGNÓSTICO POUCO PROVAVEL 
%--------------------------------------------------------------------------------------------------
menor('diagnostico pouco provavel', 'diagnostico pouco provavel', 'diagnostico pouco provavel').




