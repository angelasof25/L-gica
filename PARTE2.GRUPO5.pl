%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).
:- set_prolog_flag(unknown,fail).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais

:- op( 900,xfy,'::' ).
:- op(300,xfy,ou).
:- op(300,xfy,e).
:- dynamic '-'/1.
:- dynamic utente/5.
:- dynamic prestador/5.
:- dynamic instituicao/1.
:- dynamic especialidade/1.
:- dynamic tipo_cuidado/1.
:- dynamic cuidado/5.
:- dynamic instituicao_especialidade/2.
:- dynamic instituicao_cuidado/2.  
:- dynamic data/3.
:- dynamic excecao/1.


%--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------				
%								  CONHECIMENTO POSITIVO
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


%-------------------------------------- PRESTADOR ------------------------------------------------ 
% Extensao do predicado medico: #idPrest, Nome, Especialidade, Instituicao, Tipo -> {V,F,D}
prestador(p1, antonio_carvalho, otorrinolaringologia, hospital_braga, medico).
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
prestador(p12, joana_ribas, nutricao, hospital_luz, medico).
prestador(p13, paulo_castro, ortopedia, hospital_privado_braga, tecnico).
prestador(p14, ricardo_couto, ortopedia, hospital_privado_braga, medico).
                                                                                    

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
cuidado(data(20, 05, 18), u1, p1, raio_x, 30).
cuidado(data(10, 10, 18), u1, p5, raio_x, 20).
cuidado(data(01, 04, 15), u2, p2, ultrassonografia, 10).
cuidado(data(30, 05, 14), u3, p3, tomografia, 0).
cuidado(data(25, 08, 13), u3, p4, analises_clinicas, 35).
cuidado(data(30, 01, 14), u3, p2, ultrassonografia, 40).
cuidado(data(03, 12, 11), u4, p5, raio_x, 20).
cuidado(data(13, 05, 12), u5, p6, tomografia, 25).
cuidado(data(18, 06, 14), u6, p7, fisioterapia, 30).
cuidado(data(15, 03, 11), u7, p11, rinectomia, 15).
cuidado(data(31, 07, 13), u7, p9, raio_x, 0).
cuidado(data(27, 09, 14), u7, p10, analises_clinicas, 35).
cuidado(data(28, 09, 14), u7, p12, raio_x, 40).
cuidado(data(12, 11, 13), u1, p11, tomografia, 35).
cuidado(data(04, 04, 18), u1, p11, rinectomia, 40).
cuidado(data(06, 08, 17), u2, p10, analises_clinicas, 20).
cuidado(data(22, 07, 15), u3, p10, analises_clinicas, 25).
cuidado(data(16, 04, 12), u3, p8, raio_x, 30).
cuidado(data(01, 03, 14), u4, p1, analises_clinicas, 0).
cuidado(data(30, 05, 15), u4, p2, ultrassonografia, 10).
cuidado(data(14, 02, 16), u5, p13, densiometria_ossea, 35).
cuidado(data(18, 06, 13), u6, p6, tomografia, 40).


%--------------------------------- INSTITUICAO-ESPECIALIDADE --------------------------------                 
%Tem de se garantir que uma instituicao tem uma determinada especialidade para contratar um profissional 
% Além disso uma instituição pode ter uma especialidade e não ter profissionais contratados
%Extensao do predicado instituicao_especialidade: Inst, Esp -> {V,F,D}
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
% Além disso uma instituição pode ter poder realizar um cuidado e não ter profissionais contratados para prestarem esse cuidado
%Extensao do predicado instituicao_cuidados: Inst, Esp -> {V,F,D}
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


%--------------------------------- COMPRIMENTO ---------------------------------
% Extensao do predicado comprimento: L,N -> {V,F}
comprimento([],0).
comprimento([X|L],N) :-
    comprimento(L,N1),
    N is N1+1.

%--------------------------------- SOLUCOES ---------------------------------
%Extensao do predicado solucoes: X,T,S -> {V,F}
solucoes(X,T,S):-findall(X,T,S).


%--------------------------------- NÃO ---------------------------------------
%Extensao do predicado nao: Q -> {V,F}
nao(Q):-Q,!,fail.
nao(Q).


%--------------------------------- INSERIR ----------------------------------
% Extensao do predicado inserir: T -> {V,F}
inserir(T):-assert(T).
inserir(T):-retract(T), !, fail.


%--------------------------------- REMOCAO ----------------------------------
% Extensao do predicado remocao: T -> {V,F}
remocao(T):-retract(T).
remocao(T):-assert(T), !, fail.


%------------------------ TESTE DE PROVA (INVARIANTES) -----------------------
%Extensao do predicado testar: L -> {V,F}
testar([]).
testar([R|LR]):-
	R, testar(LR).

%----------------------- INSERIR NOVOS REGISTOS -----------------------------
%Extensao do predicado registar: T -> {V,F}
registar(T) :-
    solucoes(I,+T::I,L),
    inserir(T),
    testar(L).

%------------------------ REMOVER REGISTOS EXISTENTES ---------------------
%Extensao do predicado remover: T -> {V,F}
remover(T) :-
    T, solucoes(I,-T::I,L),
    remocao(T),
    testar(L).


%--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%                                                                     EVOLUÇÃO DO CONHECIMENTO 
%--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

%--------------------------EVOLUCAO DO CONHECIMENTO--------------------------
%Extensao do predicado evolucao_pos ---------------------------
%Permite a evolucao do conhecimento perfeito positivo ---------
% Extensao do predicado evolucao_pos: Rant, Rnovo -> {V,F}
evolucao_pos(Rant, Rnovo):-si(Rant, verdadeiro), remocao(Rant), registar(Rnovo).

%Extensao do predicado evolucao_imp ---------------------------
%Permite a evolucao do conhecimento imperfeito -------------------------
% Extensao do predicado evolucao_neg: Rant, Rnovo -> {V,F}
evolucao_imp(Rant, Rnovo):-si(Rant, verdadeiro), registar(Rnovo).



%--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%                                                               CONHECIMENTO NEGATIVO
%--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

%----------------------------------------- UTENTE ------------------------------------------------
	% so é falso se nao se provar que é verdadeiro e nao existir nenhuma excecao
-utente(IdU, N, I, G, M):-nao(utente(IdU, N, I, G, M)), nao(excecao(utente(IdU, N, I, G, M))).
+(-utente(u11, sonia_ribas, 56, feminino, guimaraes)).
	% nao permite inserir conhecimento positivo que esteja negado na base de conhecimento
+utente(IdU, N, I, G, M)::(solucoes((IdU, N, I, G, M),-utente(IdU, N, I, G, M), S),comprimento(S, L), L==0).

%-------------------------------------- PRESTADOR ------------------------------------------------ 
-prestador(IdP, N, E, I, T):-nao(prestador(IdP, N, E, I, T)), nao(excecao(prestador(IdP, N, E, I, T))).

%----------------------------------------- INSTITUICAO  -----------------------------------------
-instituicao(Desig):-nao(instituicao(Desig)), nao(excecao(instituicao(Desig))).

%------------------------------------------ ESPECIALIDADE ----------------------------------------
-especialidade(Desig):-nao(especialidade(Desig)), nao(excecao(especialidade(Desig))).

%----------------------------------------- TIPO CUIDADO  -----------------------------------------
-tipo_cuidado(Desig):-nao(tipo_cuidado(Desig)), nao(excecao(tipo_cuidado(Desig))).

%--------------------------------- CUIDADO --------------------------------- 
-cuidado(D, IdU, IdP, Desc, C):-nao(cuidado(D, IdU, IdP, Desc, C)), nao(excecao(cuidado(D, IdU, IdP, Desc, C))).

%--------------------------------- INSTITUICAO-ESPECIALIDADE -------------------------------- 
-instituicao_especialidade(Inst, Esp):-nao(instituicao_especialidade(Inst, Esp)), nao(excecao(instituicao_especialidade(Inst, Esp))).

%--------------------------------- INSTITUICAO-CUIDADO -------------------------------- 
-instituicao_cuidado(Inst, Cuid):-nao(instituicao_cuidado(Inst, Cuid)), nao(excecao(instituicao_cuidado(Inst, Cuid))).



%--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%                                                        CONHECIMENTO IMPERFEITO 
%--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

%----------------------------------------- UTENTE ------------------------------------------------

% TIPO I - conhecimento imperfeito incerto -------------------------------------------------------
	% cartas enviadas ao utente u8 vieram devolvidas porque mudou de morada
	% assim, foi criada uma excecao para permitir a ausencia de morada
utente(u8, fernando_alves, 24, masculino, incerto1).  
excecao(utente(IdU, N, I, G, M)):-utente(IdU, N, I, G, incerto1).
	
	% invariante que permite atualizar o registo incerto e substitui-lo pelo novo registo
+(utente(u8, fernando_alves, 24, masculino, M))::(utente(u8, fernando_alves, 24, masculino, incerto1),
	remocao(utente(u8, fernando_alves, 24, masculino, incerto1))).

% TIPO II - conhecimento imperfeito impreciso ----------------------------------------------------
	% apenas se sabe que o utente u9 e menor de idade. assim, foi criada
	% uma excecao que limita a idade de u9 a valores entre 0 e 18.
excecao(utente(u9, claudia_lopes, Menor, feminino, guimaraes)):-Menor>=0, Menor<18.

	% invariante que permite atualizar o registo impreciso e substitui-lo pelo novo registo
+(utente(u9, claudia_lopes, M, feminino, guimaraes))::(utente(u9, claudia_lopes, impreciso1, feminino, guimaraes),
	excecao(utente(u9, claudia_lopes, M, feminino, guimaraes)), remocao(utente(u9, claudia_lopes, impreciso1, feminino, guimaraes))).

%TIPO III - conhecimento interdito ----------------------------------------------------------------
	% o utente u10 pertence a uma embaixada estrangeira e nao se pode registar 
	% o seu nome. assim, eh criado um predicado interdito e uma regra que o
	% torna sempre falso impedindo posteriores alteracoes de conhecimento
utente(u10, embaixada1, 48, masculino, lisboa).
excecao(utente(IdU, N, I, G, M)):-utente(IdU, embaixada1, I, G, M).
interdito(embaixada1).

	% invariante que impede o preenchimento, no futuro, do nome em substituicao de embaixada
+utente(IdU, N, I, G, M)::(solucoes(N, (utente(u10, N, 48, masculino, lisboa), nao(interdito(N))),S), comprimento(S, R), R==0).


%-------------------------------------- PRESTADOR ------------------------------------------------ 

% TIPO I - conhecimento imperfeito incerto -------------------------------------------------------
	% sabe-se que o prestador p16 foi registado com o nome errado de john_smith desconhecendo-se
	% o seu nome correto
-prestador(p15, john_smith, medicina_dentaria, hospital_luz, enfermeiro). % conhecimento negativo

prestador(p15, incerto2, medicina_dentaria, hospital_luz, enfermeiro).
excecao(prestador(IdP, N, E, I, T)):-prestador(IdP, incerto2, E, I, T).

	% invariante que permite atualizar o registo incerto e substitui-lo pelo novo registo
+(prestador(p15, N, medicina_dentaria, hospital_luz, enfermeiro))::(prestador(p15, incerto2, medicina_dentaria, hospital_luz, enfermeiro),
	nao(-prestador(p15, N, medicina_dentaria, hospital_luz, enfermeiro)),
	remocao(prestador(p15, incerto2, medicina_dentaria, hospital_luz, enfermeiro))).


% TIPO II - conhecimento imperfeito impreciso ----------------------------------------------------
	% o prestador p16 tem habilitacoes que o permitem exercer o trabalho 
	% como enfermeiro ou como tecnico. criou-se assim a excecao que possibilita
	% definir o seu tipo como enfermeiro ou tecnico.
excecao(prestador(p16, rui_fonseca, ortopedia, hospital_sta_luzia, Tipo)):-Tipo=enfermeiro. 
excecao(prestador(p16, rui_fonseca, ortopedia, hospital_sta_luzia, Tipo)):-Tipo=tecnico.    

	% invariante que permite atualizar o registo impreciso e substitui-lo pelo novo registo
+(prestador(p16, rui_fonseca, ortopedia, hospital_sta_luzia, T))::(excecao(prestador(p16, rui_fonseca, ortopedia, hospital_sta_luzia, T))).       


%--------------------------------- CUIDADO --------------------------------- 

% TIPO I + TIPO II - conhecimento imperfeito incerto e impreciso                 
	% os prestadores p2 e p11 prestam cuidados de otorrinolaringologia e no dia
	% 30 de abril de 2018 nao colocaram qual dos prestadores prestou o
	% o cuidado ao utente u1 nem qual foi o custo aplicado.
cuidado(data(30, 04, 18), u1, impreciso1, rinectomia, incerto3).

excecao(cuidado(data(D, M, A), IdU, IdP, Desc, C)):-cuidado(data(D, M, A), IdU, impreciso1, Desc, incerto3), IdP==p2.
excecao(cuidado(data(D, M, A), IdU, IdP, Desc, C)):-cuidado(data(D, M, A), IdU, impreciso1, Desc, incerto3), IdP==p11.

	% invariante que permite atualizar o registo incerto e impreciso e substitui-lo pelo novo registo
+cuidado(data(30, 04, 18), u1, IdP, rinectomia, C)::(cuidado(data(30, 04, 18), u1, impreciso1, rinectomia, incerto3),
	excecao(cuidado(data(30, 04, 18), u1, IdP, rinectomia, C)), remocao(cuidado(data(30, 04, 18), u1, impreciso1, rinectomia, incerto3))).


%--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%                                                                      INVARIANTES AUXILIARES
%--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

%---------------------------------------- UTENTES ---------------------------------------------

%------------- Invariantes de insercao

%nao permite inserir utentes com ids que já pertencam à base de conhecimento
+utente(IdU, _, _, _, _) :: (solucoes(IdU, utente(IdU, _, _, _, _),S), comprimento(S,N), N==1).

%nao permite inserir utentes com nomes que já pertencam à base de conhecimento
+utente(_, Nome, _, _, _) :: (solucoes(Nome, utente(_, Nome, _, _, _),S), comprimento(S,N), N==1).

%nao permite inserir utentes com idades negativas ou superiores a 130 anos
+utente(_, _, IDD, _, _) :: (IDD>0, IDD<130).

%não permite inserir utentes com género diferente de "feminino" ou "masculino"
+utente(_, _, _, G, _) :: (G=feminino; G=masculino).


%------------- Invariantes de remocao

%não permite remover utentes que já tenham algum registo em cuidado médico
-utente(IdU, _, _, _, _) :: (solucoes(IdU, cuidado(_, IdU, _, _, _), S), S==[]). 


%---------------------------------------- PRESTADORES ---------------------------------------

%------------- Invariantes de insercao

%não permite inserir prestador com ids que já pertencam à base de conhecimento
+prestador(IdP, _, _, _, _) :: (solucoes(IdP, prestador(IdP, _, _, _, _), S), comprimento(S,N), N==1).

%não permite inserir prestador com nomes que já pertencam à base de conhecimento
+prestador(_, Nome, _, _, _) :: (solucoes(Nome, prestador(_, Nome, _, _, _), S), comprimento(S,N), N==1).

%nao permite inserir prestadores cuja especialidade nao seja disponibilizada pela instituicao
+prestador(_, _, E, Inst, _)::instituicao_especialidade(Inst, E).

%nao permite inserir prestadores de tipo diferente de medico, enfermeiro ou tecnico
+prestador(_, _, _, _, Tipo) :: (Tipo=medico; Tipo=enfermeiro; Tipo=tecnico).


%------------- Invariantes de remocao

%não permite remover prestadores que já tenham realizado cuidados medicos
-prestador(IdP, _, _, _, _) :: (solucoes(IdP, cuidado(_, _, IdP, _, _), S), S==[]). 


%---------------------------------------- ESPECIALIDADE -------------------------------------------

%------------- Invariantes de insercao

%nao permite inserir especialidades ja existentes
+especialidade(E) :: (solucoes(E, especialidade(E), S), comprimento(S,N), N==1).


%------------- Invariantes de remocao

%não permite remover especialidades que esteja associada a um prestador
-especialidade(E) :: (solucoes(E, prestador(IdP, _, E, _, _), S), S==[]). 


%---------------------------------------- INSTITUIÇÃO ----------------------------------

%------------- Invariantes de insercao

%nao permite inserir instituições ja existentes
+instituicao(Inst) :: (solucoes(Inst, instituicao(Inst), S), comprimento(S,N), N==1).


%------------- Invariantes de remocao

%não permite remover instituição que esteja associada a um prestador
-instituicao(Inst) :: (solucoes(Inst, prestador(_, _, _, Inst, _), S), S==[]). 


%---------------------------------------- TIPO CUIDADO ----------------------------------

%------------- Invariantes de insercao

%nao permite inserir tipos de cuidado ja existentes
+tipo_cuidado(Desc) :: (solucoes(Desc, tipo_cuidado(Desc), S), comprimento(S,N), N==1).


%------------- Invariantes de remocao

%não permite remover tipos de cuidado que estejam associados a um cuidado
-tipo_cuidado(Desc) :: (solucoes(Desc, cuidado(_, _, _, Desc, _), S), S==[]). 


%---------------------------------------- CUIDADO -------------------------------------

%------------- Invariantes de insercao

%não permite inserir cuidados de prestadores nem de utentes que não pertençam à base de conhecimento
+cuidado(_, IdU, IdP, Desc, _) :: (prestador(IdP, _, _, Inst, _), utente(IdU, _, _, _, _), instituicao_cuidado(Inst, Desc)).

%nao permite inserir cuidados com, simultaneamente, os mesmos Data, IdU, IdP e Descricao
+cuidado(D, IdU, IdP, Desc, _) :: (solucoes((D, IdU, IdP, Desc), cuidado(D, IdU, IdP, Desc, _), S), comprimento(S,N), N==1).

%não permite inserir custos negativos
+cuidado(_, _, _, _, C) :: (C>=0).

%o custo dos cuidados prestados a maiores de 65 anos é zero (idosos isentos de pagamento de analises clinicas) 
+cuidado(_, IdU, _, analises_clinicas, C) :: (utente(IdU, _, I, _, _), ((C==0, I>65); I<65)).

%os cuidados de rinectomia so podem ser prestados pela especialidade de otorrinolaringologia
+cuidado(_, _, IdP, rinectomia, _) :: (cuidado(_, _, IdP, rinectomia, _), prestador(IdP, _, otorrinolaringologia, _, _)).

%os cuidados de densitometria ossea so podem ser prestados pela especialidade de ortopedia
+cuidado(_, _, IdP, densitometria_ossea, _) :: (cuidado(_, _, IdP, densitometria_ossea, _), prestador(IdP, _, ortopedia, _, _)).

%não permite inserir cuidados em dias maiores que 28, 30 ou 31 (dependendo do mês), nem meses nem anos fora do intervalo 1-12 e 0-99, respetivamente.
+cuidado(data(D, M, _), _, _, _, _) :: (cuidado(data(D, M, _), _, _, _, _), ((M==1, D<32));
((M==3, D<32));((M==5, D<32));((M==7, D<32));((M==8, D<32));((M==10, D<32));((M==12, D<32));
((M==2, D<29));((M==4, D<31));((M==6, D<31));((M==9, D<31));((M==11, D<31))).
+cuidado(data(_, _, A), _, _, _, _) :: (cuidado(data(_, _, A), _, _, _, _), A>=0, A<100).

%------------- Invariantes de remocao

%não existem invariantes de remocao associados. 


%---------------------------------------- INSTITUIÇÃO-ESPECIALIDADE -------------------------------

%------------- Invariantes de insercao

%não permite inserir instituições nem especialidades que não pertençam à base de conhecimento
+instituicao_especialidade(Inst, E) :: (instituicao(Inst), especialidade(E)).

%não permite inserir um par instituição-especialidade repetido
+instituicao_especialidade(Inst, E) :: (solucoes((Inst,E), instituicao_especialidade(Inst, E), S), comprimento(S,N), N==1).


%------------- Invariantes de remoção

%não permite remover registos que já tenham cuidados medicos associados
-instituicao_especialidade(Inst, E) :: (solucoes((Inst, E), (cuidado(_, _, IdP, _, _), prestador(IdP, _, E, Inst, _)), S), S==[]).


%---------------------------------------- INSTITUIÇÃO-CUIDADO -------------------------------

%------------- Invariantes de insercao

%não permite inserir instituições nem tipos de cuidados que não pertençam à base de conhecimento
+instituicao_cuidado(Inst, Desc) :: (instituicao(Inst), tipo_cuidado(Desc)).

%não permite inserir um par instituição-cuidado repetido
+instituicao_cuidado(Inst, Desc) :: (solucoes((Inst,Desc), instituicao_cuidado(Inst, Desc), S), comprimento(S,N), N==1).

%so aceita inserir um cuidado de rinectomia se a instituicao correspondente tiver a especialidade de otorrinolaringologia 
+instituicao_cuidado(Inst, rinectomia) :: (instituicao_especialidade(Inst, otorrinolaringologia)).


%so aceita inserir um cuidado de densitometria ossea se a instituicao correspondente tiver a especialidade de ortopedia 
+instituicao_cuidado(Inst, densitometria_ossea) :: (instituicao_especialidade(Inst, ortopedia)).


%------------- Invariantes de remoção

%não permite remover registos que já tenham cuidados medicos associados
-instituicao_cuidado(Inst, Desc) :: (solucoes((Inst, Desc), (cuidado(_, _, IdP, Desc, _), prestador(IdP, _, _, Inst, _)), S), S==[]).



%--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%                                                                    SISTEMA DE INFERÊNCIA
%--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

%--------------------------------------------------------------
% Extensao do meta-predicado si: Questao, Resposta -> {V,F}    
                                                               
si(Questao,verdadeiro):-
	Questao.                               
si(Questao,falso):-
	-Questao.                                   
si(Questao,desconhecido):-
	nao(Questao), nao(-Questao).         

%---------------------------------------------------------------
% Extensao do meta-predicado si_2Q: Questao, Resposta -> {V,F}
% Sistema de inferência que trata a disjunção e conjunção de questoes

% -------------------- CONJUNCAO ------------------------------

si_2Q(Q1 e Q2,verdadeiro):- si(Q1,verdadeiro), si(Q2,verdadeiro).

si_2Q(Q1 e Q2,falso):-si(Q1,falso).

si_2Q(Q1 e Q2,falso):-si(Q2,falso).

si_2Q(Q1 e Q2,desconhecido):-
	(nao(si_2Q(Q1 e Q2,verdadeiro)), nao(si_2Q(Q1 e Q2,falso))).

% -------------------- DISJUNCAO ------------------------------

si_2Q(Q1 ou Q2,falso):-si(Q1,falso), si(Q2,falso).

si_2Q(Q1 ou Q2,verdadeiro):-si(Q1,verdadeiro).

si_2Q(Q1 ou Q2,verdadeiro):-si(Q2,verdadeiro).

si_2Q(Q1 ou Q2,desconhecido):-
	(nao(si_2Q(Q1 ou Q2,verdadeiro)), nao(si_2Q(Q1 ou Q2,falso))).


