%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).
:- set_prolog_flag(unknown,fail).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais

:- op( 900,xfy,'::' ).
:- dynamic utente/5.
:- dynamic prestador/5.
:- dynamic instituicao/1.
:- dynamic especialidade/1.
:- dynamic tipo_cuidado/1.
:- dynamic cuidado/5.
:- dynamic instituicao_especialidade/2.
:- dynamic instituicao_cuidado/2.  



%--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------				
%								  POVOAMENTO DA BASE DE CONHECIMENTO
%--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


%----------------------------------------- UTENTE ------------------------------------------------
% Extensao do predicado utente: #idUt, Nome, Idade, Genero, Morada -> {V,F,D}
utente(u1, rita_pinto, 41, feminino, braga).
utente(u2, carlos_moreira, 12, masculino, vilareal).
utente(u3, rui_sousa, 12, masculino, leiria).
utente(u4, maria_tavares, 84, feminino, braga).
utente(u5, ivone_lopes, 36, feminino, lisboa).
utente(u6, matilde_neves, 50, feminino, porto).
utente(u7, andre_fernandes, 64, masculino, leiria).
utente(u8, andre_marques,desconhecido, masculino, beja).
utente(u9,joana_vales, 23, feminino, desconhecido).
utente(u10, andreia_lima, 45, desconhecido, amares).
utente(u11,jose_cruz,interdito,msculino, braga).


%-------------------------------------- PRESTADOR ------------------------------------------------ 
% Extensao do predicado medico: #idPrest, Nome, Especialidade, Instituicao, Tipo -> {V,F,D}
prestador(p1, antonio_carvalho, cardiologia, hospital_braga, medico).
prestador(p2, manuel_sousa, otorrinolaringologia, hospital_braga, medico).
prestador(p3, alice_ribeiro, medicina_dentaria, hospital_luz, tecnico).
prestador(p4, henrique_alves, estomatologia, hospital_braga, medico).
prestador(p5, dinis_santos, cardiologia, hospital_sta_luzia, enfermeiro).
prestador(p6, sofia_almeida, medicina_geral, hospital_braga, tecnico).
prestador(p7, alberto_dias, neurologia, hospital_luz, medico).
prestador(p8, beatriz_barbosa, oncologia, hospital_privado_braga, tecnico).
prestador(p9, pedro_ribeiro, ortopedia, hospital_sta_luzia, enfermeiro).
prestador(p10, leonor_bastos, cardiologia, hospital_braga, tecnico).
prestador(p11, americo_abreu, otorrinolaringologia, hospital_braga, enfermeiro).
prestador(p12, joana_ribas, nutricao, desconhecido, medico).
prestador(p13, paulo_castro, desconhecido, hospital_privado_braga, tecnico).
prestador(p14, ricardo_couto, ortopedia, hospital_privado_braga, desconhecido).


%----------------------------------------- INSTITUICAO  -----------------------------------------
% Extensao do predicado instituicao: Designacao -> {V,F,D}
instituicao(hospital_braga).
instituicao(hospital_priv_braga).
instituicao(hospital_sta_luzia).
instituicao(hospital_luz).


%------------------------------------------ ESPECIALIDADE ----------------------------------------
% Extensao do predicado especialidade: Designacao -> {V,F,D}
especialidade(medicina_dentaria).
especialidade(medicina_geral).
especialidade(cardiologia).
especialidade(neurologia).
especialidade(ortopedia).
especialidade(estomatologia).
especialidade(oncologia).
especialidade(otorrinolaringologia).
especialidade(nutricao).


%----------------------------------------- TIPO CUIDADO  -----------------------------------------
% Extensao do predicado tipo_cuidado: Designacao -> {V,F,D}
tipo_cuidado(raio_x).
tipo_cuidado(analises_clinicas).
tipo_cuidado(tomografia).
tipo_cuidado(utrassonografia).
tipo_cuidado(rinectomia).
tipo_cuidado(fisioterapia).
tipo_cuidado(densitometria_ossea).


%--------------------------------- CUIDADO --------------------------------- 
%Extensao do predicado cuidado: Data, #idUt, #idP, DESC, Custo -> {V,F,D}
cuidado(200518, u1, p1, raio_x, 30).
cuidado(101018, u1, p5, raio_x, 20).
cuidado(010415, u2, p2, ultrassonografia, 10).
cuidado(300514, u3, p3, tomografia, 0).
cuidado(250813, u3, p4, analises_clinicas, 35).
cuidado(300114, u3, p2, ultrassonografia, 40).
cuidado(031211, u4, p5, raio_x, 20).
cuidado(130512, u5, p6, tomografia, 25).
cuidado(180614, u6, p7, fisioterapia, 30).
cuidado(150311, u7, p11, rinectomia, 15).
cuidado(310713, u7, p9, raio_x, 0).
cuidado(270914, u7, p10, analises_clinicas, 35).
cuidado(280914, u7, p12, raio_x, 40).
cuidado(121113, u1, p11, tomografia, 35).
cuidado(040418, u1, p11, rinectomia, 40).
cuidado(060817, u2, p10, analises_clinicas, 20).
cuidado(220715, u3, p10, analises_clinicas, interdito).
cuidado(160412, u3, p8, interdito, 30).
cuidado(010314, u4, p1, analises_clinicas, desconhecido).
cuidado(300515, u4, p2, ultrassonografia, desconhecido).
cuidado(140216, u5, p13, desconhecido, 35).
cuidado(180613, u6, p6, tomografia, interdito).


%--------------------------------- INSTITUICAO-ESPECIALIDADE -------------------------------- 
%Tem de se garantir que uma instituicao tem uma determinada especialidade para contratar um profissional 
% Al�m disso uma institui��o pode ter uma especialidade e n�o ter profissionais contratados
%-----------------------------------------------------------------------------
%Extensao do predicado instituicao especialidade: Inst, Esp -> {V,F,D}
instituicao_especialidade(hospital_braga, medicina_dentaria).
instituicao_especialidade(hospital_braga, medicina_geral).
instituicao_especialidade(hospital_braga, cardiologia).
instituicao_especialidade(hospital_braga, estomatologia).
instituicao_especialidade(hospital_braga, oncologia).
instituicao_especialidade(hospital_braga, otorrinolaringologia).
instituicao_especialidade(hospital_privado_braga, medicina_dentaria).
instituicao_especialidade(hospital_privado_braga, medicina_geral).
instituicao_especialidade(hospital_privado_braga, ortopedia).
instituicao_especialidade(hospital_privado_braga, oncologia).
instituicao_especialidade(hospital_luz, medicina_dentaria).
instituicao_especialidade(hospital_luz, medicina_geral).
instituicao_especialidade(hospital_luz, neurologia).
instituicao_especialidade(hospital_luz, nutricao).
instituicao_especialidade(hospital_sta_luzia, medicina_dentaria).
instituicao_especialidade(hospital_sta_luzia, medicina_geral).
instituicao_especialidade(hospital_sta_luzia, cardiologia).
instituicao_especialidade(hospital_sta_luzia, neurologia).
instituicao_especialidade(hospital_sta_luzia, ortopedia).
instituicao_especialidade(hospital_sta_luzia, oncologia).


%--------------------------------- INSTITUICAO-CUIDADO -------------------------------- 
%Tem de se garantir que um a instituicao tem poder prestar um determinado cuidado para contratar um profissional 
% Al�m disso uma institui��o pode ter poder realizar um cuidado e n�o ter profissionais contratados para prestarem esse cuidado
instituicao_cuidado(hospital_braga, raio_x).
instituicao_cuidado(hospital_braga, analises_clinicas).
instituicao_cuidado(hospital_braga, ultrassonografia).
instituicao_cuidado(hospital_braga, tomografia).
instituicao_cuidado(hospital_braga, rinectomia).
instituicao_cuidado(hospital_privado_braga, raio_x).
instituicao_cuidado(hospital_privado_braga, densitometria_ossea).
instituicao_cuidado(hospital_luz, raio_x).
instituicao_cuidado(hospital_luz, fisioterapia).
instituicao_cuidado(hospital_luz, tomografia).
instituicao_cuidado(hospital_sta_luzia, raio_x).



%--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%                                                                     PREDICADOS AUXILIARES 
%--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%--------------------------------EVOLUÇÃO-----------------------------------------
evolucao(T) :-
	solucoes(Invariante,+T::Invariante,R), inserir(T), testar(R).

%------------------------------SISTEMA INFERÊNCIA---------------------------------
si(Q,verdadeiro) :- Q.
si(Q,falso) :- -Q.
si(Q,desconhecido) :- nao(Q), nao(-Q).

%--------------------------------- COMPRIMENTO ---------------------------------
% Extensao do predicado comprimento: L,N -> {V,F}
comprimento([],0).
comprimento([X|L],N) :-
    comprimento(L,N1),
    N is N1+1.


%--------------------------------- MAXIMO ---------------------------------
% Extensao do predicado maximo: L,MAX -> {V,F}
maximo([X],X).
maximo([X|L1],X):-maximo(L1, MAX), (X > MAX).
maximo([X|L1],MAX):-maximo(L1, MAX), (X =< MAX).


%--------------------------------- SOLUCOES ---------------------------------
%Extensao do predicado solucoes: X,T,S -> {V,F}
solucoes(X,T,S):-findall(X,T,S).


%--------------------------------- N�O ---------------------------------------
%Extensao do predicado nao: Q -> {V,F}
nao(Q):-Q,!,fail.
nao(Q).


%--------------------------------- PERTENCE ----------------------------------
%Extensao do predicado pertence: X,L -> {V,F}     
pertence(X,[X|L]).
pertence(X,[Y|L]):-pertence(X,L).


%------------------------------- SEM REPETIDOS -----------------------------
%Extensao do predicado sem_repetidos: L,S -> {V,F}
sem_repetidos([X],[X]).
sem_repetidos([X|L], S):- pertence(X,L), sem_repetidos(L, S).
sem_repetidos([X|L], [X|S]):- nao(pertence(X,L)), sem_repetidos(L, S).


%------------------------------- APAGAR TUDO -----------------------------
% Extensao do predicado apagar_tudo: X,L,S -> {V,F}
% Apaga todas as ocorr�ncias de um elemento X numa lista L
apagar_tudo(X,[],[]).
apagar_tudo(X,[X|L],S):-apagar_tudo(X,L,S).
apagar_tudo(X,[Y|L],[Y|L1]):-X\=Y, apagar_tudo(X,L,L1).


%-------------------------- APAGAR TUDO LISTA  ---------------------------------
% Extensao do predicado apaga_lista: L1,L2,R -> {V,F}
% Apagar da lista L2 os elementos da lista L1 
apagar_tudo_lista([],L,L).
apagar_tudo_lista([X|Y],L,R):-
	apagar_tudo(X,L,S),
	apagar_tudo_lista(Y,S,R).


%-------------------------------- SOMA DE LISTA ------------------------------
%Extensao do predicado soma_lista: L, TOTAL -> {V,F}
soma_lista([], 0).
soma_lista([X|L], TOTAL) :-soma_lista(L, SOMA), TOTAL is X + SOMA.


%--------------------------------- INSERIR ----------------------------------
% Extensao do predicado inserir: T -> {V,F}
inserir(T):-assert(T).
inserir(T):-retract(T), !, fail.


%--------------------------------- REMOVER ----------------------------------
% Extensao do predicado remocao: T -> {V,F}
remocao(T):-retract(T).
remocao(T):-assert(T), !, fail.


%------------------------ TESTE DE PROVA (INVARIANTES) -----------------------
%Extensao do predicado testar: L -> {V,F}
testar([]).
testar([R|LR]):-
	R, testar(LR).




%--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%                                                               FUNCIONALIDADES B�SICAS 
%--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


%---------------- Registar utentes, prestadores e cuidados de sa�de --------------------
%Extensao do predicado registar: T -> {V,F}
registar(T) :-
    solucoes(I,+T::I,L),
    inserir(T),
    testar(L).


%---------------- Remover utentes, prestadores e cuidados de sa�de ---------------------
%Extensao do predicado remover: T -> {V,F}
remover(T) :-
    T, solucoes(I,-T::I,L),
    remocao(T),
    testar(L).

%-------------------- Identificar utentes por criterios de selecao -----------------------
%Extensao do predicado cselecao_utente: CS, X, S -> {V,F}
% CS pode ser id, nome, idade, genero ou morada e define o criterio de selecao a pesquisar
cselecao_utente(CS, IdU, S):-
	CS=id, solucoes((IdU, N, I, G, M), utente(IdU, N, I, G, M), S).
cselecao_utente(CS, N, S):-
	CS=nome, solucoes((IDU, N, I, G, M), utente(IDU, N, I, G, M), S).
cselecao_utente(CS, I, S):-
	CS=idade, solucoes((IDU, N, I, G, M), utente(IDU, N, I, G, M), S).
cselecao_utente(CS, G, S):-
	CS=genero, solucoes((IDU, N, I, G, M), utente(IDU, N, I, G, M), S).
cselecao_utente(CS, M, S):-
	CS=morada, solucoes((IDU, N, I, G, M), utente(IDU, N, I, G, M), S).


%------------- Identificar as instituicoes prestadoras de cuidados de saude --------------------
%Extensao do predicado inst_cuidados: S -> {V,F}
inst_cuidados(S):-
	solucoes((Inst), (cuidado(_, _, IdP, _, _), prestador(IdP, _, _, Inst, _)), L),sem_repetidos(L,S).


%------------- Identificar cuidados de saude por instituicao/morada/datas -----------------------
%Extensao do predicado filtro_cuidado: F, X, S -> {V,F}
filtro_cuidado(F, Inst, S):-
	F=instituicao, solucoes(Desc, (cuidado(_, _, IdP, Desc, _), prestador(IdP, _, _, Inst, _)), L), sem_repetidos(L,S).
filtro_cuidado(F, Mor, S):-
	F=morada, solucoes(Desc, (cuidado(_, IdU, _, Desc, _), utente(IdU, _, _, _, Mor)), L), sem_repetidos(L,S).
filtro_cuidado(F, Data, S):-
	F=data, solucoes(Desc, cuidado(Data, _, _, Desc, _), L), sem_repetidos(L,S).


%------------- Identificar os utentes de um prestador/especialidade/instituicao -------------------
%Extensao do predicado filtro_utente: F, X, S -> {V,F}
filtro_utente(F, P, S):-
	F=prestador, solucoes((IdU, N, I, G, M), (cuidado(_, IdU, P, _, _), utente(IdU, N, I, G, M)), L), sem_repetidos(L,S).
filtro_utente(F, E, S):-
	F=especialidade, solucoes((IdU, N, I, G, M), (cuidado(_, IdU, IdP, _, _), prestador(IdP, _, E, _, _), utente(IdU, N, I, G, M)), L), sem_repetidos(L,S).
filtro_utente(F, Inst, S):-
	F=instituicao, solucoes((IdU, N, I, G, M), (cuidado(_, IdU, IdP, _, _), prestador(IdP, _, _, Inst, _), utente(IdU, N, I, G, M)), L), sem_repetidos(L,S).


%-------------Determinar todas as instituicoes/prestadores a que um utente ja recorreu -----------------
%Extensao do predicado filtro_utente2: F, X, S -> {V,F}
filtro_utente2(F, IdU, S):-
	F=instituicao, solucoes(Inst, (cuidado(_, IdU, IdP, _, _), prestador(IdP, _, _, Inst, _)), L), sem_repetidos(L, S).
filtro_utente2(F, IdU, S):-
	F=prestador, solucoes(Nome, (cuidado(_, IdU, IdP, _, _), prestador(IdP, Nome, _, _, _)), L), sem_repetidos(L, S).


%------------- Calcular o custo total dos cuidados por utente/especialidade/prestador/datas -------------
%Extensao do predicado filtro_custo: F, X, S -> {V,F}
filtro_custo(F, IdU, S):-
	F=utente, solucoes(Custo, cuidado(_, IdU, _, _, Custo), C), soma_lista(C, S).
filtro_custo(F, E, S):-
	F=especialidade, solucoes(Custo, (cuidado(_, _, IdP, _, Custo), prestador(IdP, _, E, _, _)), C), soma_lista(C, S).
filtro_custo(F, IdP, S):-
	F=prestador_id, solucoes(Custo, cuidado(_, _, IdP, _, Custo), C), soma_lista(C, S).
filtro_custo(F, N, S):-
	F=prestador_nome, solucoes(Custo, (cuidado(_, _, IdP, _, Custo), prestador(IdP, N, _, _, _)), C), soma_lista(C, S).
filtro_custo(F, D, S):-
	F=data, solucoes(CUSTO, cuidado(D, _, _, _, Custo), C), soma_lista(C, S).



%--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%                                                        FUNCIONALIDADES ADICIONAIS 
%--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

%------------- Identificar os utentes que pagaram mais por um cuidado medico --------------------------
%Extensao do predicado utente_gasto_max: S -> {V,F}
utente_gasto_max(S):-solucoes(Custo,cuidado(_, _, _, _, Custo),L), maximo(L,Max), solucoes((IdU, Nome),(cuidado(_, IdU, _, _, Max), utente(IdU, Nome, _, _, _)),R), 
sem_repetidos(R, S).


%------------- Identificar o genero que mais cuidados de uma dada especialidade teve --------------------------
%Extens�o do predicado genero_especialidade: E, S -> {V,F}
genero_especialidade(E, S):-solucoes(_, (cuidado(_, IdU, IdP, _, _), prestador(IdP, _, E, _, _), utente(IdU, _, _, G, _), G=masculino), R1), comprimento(R1, S1),
	solucoes(_, (cuidado(_, IdU, IdP, _, _), prestador(IdP, _, E, _, _), utente(IdU, _, _, G, _), G=feminino), R2), comprimento(R2, S2), S1>S2, 
	S=(masculino, S1).
genero_especialidade(E, S):-solucoes(_, (cuidado(_, IdU, IdP, _, _), prestador(IdP, _, E, _, _), utente(IdU, _, _, G, _), G=masculino), R1), comprimento(R1, S1),
	solucoes(_, (cuidado(_, IdU, IdP, _, _), prestador(IdP, _, E, _, _), utente(IdU, _, _, G, _), G=feminino), R2), comprimento(R2, S2), S1<S2, 
	S=(feminino, S2).
genero_especialidade(E, S):-solucoes(_, (cuidado(_, IdU, IdP, _, _), prestador(IdP, _, E, _, _), utente(IdU, _, _, G, _), G=masculino), R1), comprimento(R1, S1),
	solucoes(_, (cuidado(_, IdU, IdP, _, _), prestador(IdP, _, E, _, _), utente(IdU, _, _, G, _), G=feminino), R2), comprimento(R2, S2), S1=S2, 
	S=(igual_prevalencia_de_genero, S2).


%------------- Identificar os prestadores que nao prestaram qualquer cuidado ----------------------------------------
%Extens�o do predicado prestador_zero: S -> {V,F}
prestador_zero(S):-
	solucoes(Nome, (prestador(IdP, Nome, _, _, _), cuidado(_, _, IdP, _, _)), L), sem_repetidos(L, L1), solucoes(Nome, prestador(IdP, Nome, _, _, _), L2), 
	apagar_tudo_lista(L1, L2, S).

%-----------------Obter a informacao completa de um utente/prestador a partir do nome -------
%Extens�o do predicado informacoes: F, N, S -> {V,F}
informacoes(F,N,S):-
	F=utente, solucoes((IdU, N, I, G, M), utente(IdU, N, I, G, M), S).
informacoes(F,N,S):-
	F=prestador, solucoes((Id, N, E, I, T), prestador(IdP, N, E, I, T), S).


%------------- Calcular o custo total de uma dada especialidade --------------------------
%Extens�o do predicado especialidade_custo: E, S -> {V,F}
especialidade_custo(E,R):-
	solucoes(C, (cuidado(_, _, IdP, _, C), prestador(IdP, _, E, _, _)), S), soma_lista(S,R).



%--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%                                                                      INVARIANTES 
%--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


%---------------------------------------- UTENTES ---------------------------------------------

%------------- Invariantes de insercao

%nao permite inserir utentes com ids que j� pertencam � base de conhecimento
+utente(IdU, _, _, _, _) :: (solucoes(IdU, utente(IdU, _, _, _, _),S), comprimento(S,N), N==1).

%nao permite inserir utentes com nomes que j� pertencam � base de conhecimento
+utente(_, Nome, _, _, _) :: (solucoes(Nome, utente(_, Nome, _, _, _),S), comprimento(S,N), N==1).

%nao permite inserir utentes com idades negativas ou superiores a 130 anos
+utente(_, _, IDD, _, _) :: (IDD>0, IDD<130).

%n�o permite inserir utentes com g�nero diferente de "feminino" ou "masculino"
+utente(_, _, _, G, _) :: (G=feminino; G=masculino).


%------------- Invariantes de remocao

%n�o permite remover utentes que j� tenham algum registo em cuidado m�dico
-utente(IdU, _, _, _, _) :: (solucoes(IdU, cuidado(_, IdU, _, _, _), S), S==[]).



%---------------------------------------- PRESTADORES ---------------------------------------

%------------- Invariantes de insercao

%n�o permite inserir prestador com ids que j� pertencam � base de conhecimento
+prestador(IdP, _, _, _, _) :: (solucoes(IdP, prestador(IdP, _, _, _, _), S), comprimento(S,N), N==1).

%n�o permite inserir prestador com nomes que j� pertencam � base de conhecimento
+prestador(_, Nome, _, _, _) :: (solucoes(Nome, prestador(_, Nome, _, _, _), S), comprimento(S,N), N==1).

%nao permite inserir prestadores cuja especialidade nao seja disponibilizada pela instituicao
+prestador(_, _, E, Inst, _)::instituicao_especialidade(Inst, E).

%nao permite inserir prestadores de tipo diferente de medico, enfermeiro ou tecnico
+prestador(_, _, _, _, Tipo) :: (Tipo=medico; Tipo=enfermeiro; Tipo=tecnico).


%------------- Invariantes de remocao

%n�o permite remover prestadores que j� tenham realizado cuidados medicos
-prestador(IdP, _, _, _, _) :: (solucoes(IdP, cuidado(_, _, IdP, _, _), S), S==[]). 




%---------------------------------------- ESPECIALIDADE -------------------------------------------

%------------- Invariantes de insercao

%nao permite inserir especialidades ja existentes
+especialidade(E) :: (solucoes(E, especialidade(E), S), comprimento(S,N), N==1).


%------------- Invariantes de remocao

%n�o permite remover especialidades que esteja associada a um prestador
-especialidade(E) :: (solucoes(E, prestador(IdP, _, E, _, _), S), S==[]). 


%---------------------------------------- INSTITUI��O ----------------------------------

%------------- Invariantes de insercao

%nao permite inserir institui��es ja existentes
+instituicao(Inst) :: (solucoes(Inst, instituicao(Inst), S), comprimento(S,N), N==1).


%------------- Invariantes de remocao

%n�o permite remover institui��o que esteja associada a um prestador
-instituicao(Inst) :: (solucoes(Inst, prestador(_, _, _, Inst, _), S), S==[]). 


%---------------------------------------- TIPO CUIDADO ----------------------------------

%------------- Invariantes de insercao

%nao permite inserir tipos de cuidado ja existentes
+tipo_cuidado(Desc) :: (solucoes(Desc, tipo_cuidado(Desc), S), comprimento(S,N), N==1).


%------------- Invariantes de remocao

%n�o permite remover tipos de cuidado que estejam associados a um cuidado
-tipo_cuidado(Desc) :: (solucoes(Desc, cuidado(_, _, _, Desc, _), S), S==[]). 


%---------------------------------------- CUIDADO -------------------------------------

%------------- Invariantes de insercao

%n�o permite inserir cuidados de prestadores nem de utentes que n�o perten�am � base de conhecimento
+cuidado(_, IdU, IdP, Desc, _) :: (prestador(IdP, _, _, Inst, _), utente(IdU, _, _, _, _), instituicao_cuidado(Inst, Desc)).

%nao permite inserir cuidados com, simultaneamente, os mesmos Data, IdU, IdP e Descricao
+cuidado(D, IdU, IdP, Desc, _) :: (solucoes((D, IdU, IdP, Desc), cuidado(D, IdU, IdP, Desc, _), S), comprimento(S,N), N==1).

%n�o permite inserir custos negativos
+cuidado(_, _, _, _, C) :: (C>=0).

%o custo dos cuidados prestados a maiores de 65 anos � zero (idosos isentos de pagamento de analises clinicas) 
+cuidado(_, IdU, _, analises_clinicas, C) :: (utente(IdU, _, I, _, _), ((C==0, I>65); I<65)).

%os cuidados de rinectomia so podem ser prestados pela especialidade de otorrinolaringologia
+cuidado(_, _, IdP, rinectomia, _) :: (cuidado(_, _, IdP, rinectomia, _), prestador(IdP, _, otorrinolaringologia, _, _)).

%os cuidados de densitometria ossea so podem ser prestados pela especialidade de ortopedia
+cuidado(_, _, IdP, densitometria_ossea, _) :: (cuidado(_, _, IdP, densitometria_ossea, _), prestador(IdP, _, ortopedia, _, _)).


%------------- Invariantes de remocao

%n�o existem invariantes de remocao associados. 


%---------------------------------------- INSTITUI��O-ESPECIALIDADE -------------------------------

%------------- Invariantes de insercao

%n�o permite inserir institui��es nem especialidades que n�o perten�am � base de conhecimento
+instituicao_especialidade(Inst, E) :: (instituicao(Inst), especialidade(E)).

%n�o permite inserir um par institui��o-especialidade repetido
+instituicao_especialidade(Inst, E) :: (solucoes((Inst,E), instituicao_especialidade(Inst, E), S), comprimento(S,N), N==1).


%------------- Invariantes de remo��o

%n�o permite remover registos que j� tenham cuidados medicos associados
-instituicao_especialidade(Inst, E) :: (solucoes((Inst, E), (cuidado(_, _, IdP, _, _), prestador(IdP, _, E, Inst, _)), S), S==[]).


%---------------------------------------- INSTITUI��O-CUIDADO -------------------------------

%------------- Invariantes de insercao

%n�o permite inserir institui��es nem tipos de cuidados que n�o perten�am � base de conhecimento
+instituicao_cuidado(Inst, Desc) :: (instituicao(Inst), tipo_cuidado(Desc)).

%n�o permite inserir um par institui��o-cuidado repetido
+instituicao_cuidado(Inst, Desc) :: (solucoes((Inst,Desc), instituicao_cuidado(Inst, Desc), S), comprimento(S,N), N==1).

%so aceita inserir um cuidado de rinectomia se a instituicao correspondente tiver a especialidade de otorrinolaringologia 
+instituicao_cuidado(Inst, rinectomia) :: (instituicao_especialidade(Inst, otorrinolaringologia)).


%so aceita inserir um cuidado de densitometria ossea se a instituicao correspondente tiver a especialidade de ortopedia 
+instituicao_cuidado(Inst, densitometria_ossea) :: (instituicao_especialidade(Inst, ortopedia)).


%------------- Invariantes de remo��o

%n�o permite remover registos que j� tenham cuidados medicos associados
-instituicao_cuidado(Inst, Desc) :: (solucoes((Inst, Desc), (cuidado(_, _, IdP, Desc, _), prestador(IdP, _, _, Inst, _)), S), S==[]).



