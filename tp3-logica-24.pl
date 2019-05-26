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
:- op( 900,xfx,considerado).
:- op( 900,xfx,porque).
:- op( 900,xfx,::).
:- op( 900,xfx,:::).

:- dynamic facto/1.
:- dynamic '::'/2.
:- dynamic ':::'/2.

%----------------------------- ---- - - - -- - -------------------
% Extensao do predicado nao: Questao -> {V,F}
nao(Q) :- Q, !, fail.
nao(Q).
 
% Extensao do predicado comprimento: lista, comprimento-> {V,F}
comprimento([], 0).
comprimento([A|B], R) :- comprimento(B,X), R is X+1.
 
% Extensao do predicado concatenar: L1,L2, NL -> {V,F}
concatenar([],X,X).
concatenar([X|L1],L2,[X|R]):- concatenar(L1,L2,R).
 
% Extensao do predicado solucoes: A,T,S -> {V,F}
solucoes(X,T,S):-findall(X,T,S).

%---------------------------- ---- - --  - - - - - ---------------
% Extensao do predicado testar_grau: Grau -> {V,F}

testar_grau('improvavel').
testar_grau('pouco provavel').
testar_grau('provavel').
testar_grau('muito provavel').
testar_grau('certo').

%---------------- -- -- -  ----------------------------------------

% Regras de Produção-  
%  se 'SINTOMA' entao 'DIAGNOSTICO' :: ( 'GRAU', 'MEDICAMENTO')

se 'episodios stresse' entao 'ansiedade' :: ('certo','med').
se 'ansiedade' ou 'stresse cronico' entao 'anorexia' :: ('provavel','med1').
se 'nauseas' e 'vomitos' e 'fadiga' e 'anorexia' entao 'nefrite' :: ('muito provavel','med2').
se 'nefrite' e 'insonia' e 'hipertensao' entao 'nefrite glomerular' :: ('pouco provavel','med3').
se 'edema' e 'oliguria' e 'nefrite glomerular' entao 'sindrome nefritica' :: ('pouco provavel','med4').

se 'espirros' e 'rinite' entao 'constipação' :: ('muito provavel','anti-histaminico').
se 'espirros' e 'rinite' e 'dores cabeça' entao 'gripe' :: ('provavel','clorofenamina').
se 'febre' e 'dor orelha' entao 'otite' :: ('muito provavel','amoxicilina').
se 'febre' e 'dor garganta' entao 'amigdalite' :: (' provavel','cefalosporina').

%-----------  - - - - - - - - -- - -  ---------------------------
% INVARIANTES
+(facto(Termo) :: Certeza) ::: (solucoes(Termo, (facto Termo :: C), X),comprimento(X,H), H==1).



%--------------------- ------ - - -- - - -------------------------
%DICIONARIOS para as inferências dos graus de Certeza.
