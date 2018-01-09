%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%[ Funções de Lista ]%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% pertence(Elemento, Lista)
% Verifica se um elemento pertence a lista.
pertence(Elem,[Elem|_]). % Pertence a lista se ele for a cabeça
pertence(Elem,[_|Cauda]) :- pertence(Elem,Cauda). % Pertence a lista se ele pertence à cauda
pertence(Elem,[Cab|_]) :- pertence(Elem,Cab). % Pertence a lista se ele pertencer a cabeça (Lista dentro de lista)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% concatena(Lista1, Lista2, Resultado)
% Contatena duas listas (Juntar duas listas)
concatena([],L2,L2).
concatena([CabL1|CaudaL1],L2,[CabL1|Resultado]) :- concatena(CaudaL1,L2,Resultado).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% enesimo(N, Lista, Nesimo)
% Identifica o elemento que ocupa a posição N da lista
% Salva o elemento da posição N da lista na variavel Elem.
%enesimo(_,[],_) :- !, fail.
enesimo(1, [Elem|_], Elem).
enesimo(N,[_|Cauda], Elem) :- enesimo(M,Cauda,Elem), N is M+1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% replace(Lista, Index, NovoElemento, ListaResultado)
% Substitui um elemento na Lista pelo indice dele
replace([_|Cauda], 1, Elem, [Elem|Cauda]). % Se o ID for 1, então substitui a cabeça
replace([Cab|Cauda], Id, Elem, [Cab|Resul]):- Id > 0, NI is Id-1, replace(Cauda, NI, Elem, Resul), !. % Substitui o elemento na cauda
replace(L, _, _, L). % Não foi possivel substituir



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%[ Fatos gerais do programa ]%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

elevadorAgente(eleAGT). % Elevador com agente dentro.
objetivoAgente(objAGT). % Objetivo com agente em cima.
caminhaoAgente(trkAGT). % Caminhão com agente dentro.

elevador(ele). % Elevador (permite subir/descer os andares)
objetivo(obj). % Caixa objetivo (tem que levar ao caminhão)
caminho(cam). % Caminho que pode ser percorrido
caixa(box). % Caixa que não é objetivo (não pode passar por cima nem mover ela)
caminhao(trk). % Caminhão (entrega final)



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%[ Funções do Mapa ]%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
%Limite do cenário
limiteAltura(Y) :- Y > 0, Y =< 5, !.
limiteLargura(X) :- X > 0, X =< 12, !.
limiteCenario(X,Y) :- limiteLargura(X), limiteAltura(Y).

%Pode ir para posição X,Y
podeIr(X,Y,Mapa) :- not(temCaixa(X,Y,Mapa)).

%Pega a posição do agente no Mapa
posAgente(X,Y,Mapa) :- 
	enesimo(Y, Mapa, Andar), % Pego um andar qualquer
	(
		enesimo(X, Andar, agt) ; %Se for o agente em si
		enesimo(X, Andar, eleAGT) ; %Se for o agente dentro do elevador
		enesimo(X, Andar, trkAGT) % Agente dentro do caminhao
	), 
	limiteCenario(X,Y). % E está dentro do cenário



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%[ Verifica se Existe algo em X,Y ]%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Verifica se existe caixa na posicão X,Y
temCaixa(X,Y,Mapa) :- limiteCenario(X,Y), enesimo(Y, Mapa, Andar), enesimo(X, Andar, Elemento), !, caixa(Elemento).

%Verifica se existe objetivo na posição X,Y.
temObjetivo(X,Y,Mapa) :- limiteCenario(X,Y), enesimo(Y, Mapa, Andar), enesimo(X, Andar, Elemento), !, (objetivo(Elemento) ; objetivoAgente(Elemento)).

%Verifica se existe elevador na posição X,Y.
temElevador(X,Y,Mapa) :- limiteCenario(X,Y), enesimo(Y, Mapa, Andar), enesimo(X, Andar, Elemento), !, (elevador(Elemento) ; elevadorAgente(Elemento)).

%Verifica se existe caminhão em X,Y
temCaminhao(X,Y,Mapa) :- limiteCenario(X,Y), enesimo(Y, Mapa, Andar), enesimo(X, Andar, Elemento), !, (caminhao(Elemento) ; caminhaoAgente(Elemento)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%[ Movimenta o agente pelo cenário ]%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Muda os valores das posições [X1,Y1] e [X2,Y2] (onde o agente está e onde ele vai).
muda(X1,Y1,X2,Y2,Mapa,MapaDepois) :- limiteCenario(X1,Y1), limiteCenario(X2,Y2), muda(X1,Y1,Mapa,MapaMeio), muda(X2,Y2,MapaMeio,MapaDepois).

/****************************************************************************************************************
*	Como funciona :::: enesimo(Y, Mapa, Andar), enesimo(X, Andar, cam) ?					*
*	Primeiro pego o Yesimo termo do Mapa e salvo em Andar (array contendo os dados do andar Y).		*
*	Depois pego o Xesimo termo do Andar e tento unificar com (cam|agt|ele|eleAGT) (posição X do Andar Y).	*
*****************************************************************************************************************/
% Muda 'cam' para 'agt' (Caminho para Agente) - [Agente foi para um local sem nada]
muda(X,Y,Mapa,MapaDepois) :- enesimo(Y, Mapa, Andar), enesimo(X, Andar, cam), replace(Andar, X, agt, AndarNovo), replace(Mapa, Y, AndarNovo, MapaDepois), !.

% Muda 'agt' para 'cam' (Agente para Caminho) - [Agente saiu do local]
muda(X,Y,Mapa,MapaDepois) :- enesimo(Y, Mapa, Andar), enesimo(X, Andar, agt), replace(Andar, X, cam, AndarNovo), replace(Mapa, Y, AndarNovo, MapaDepois), !.

% Muda 'ele' para 'eleAGT' (Elevador para ElevadorAgente) - [Agente entrou no elevador]
muda(X,Y,Mapa,MapaDepois) :- enesimo(Y, Mapa, Andar), enesimo(X, Andar, ele), replace(Andar, X, eleAGT, AndarNovo), replace(Mapa, Y, AndarNovo, MapaDepois), !.

% Muda 'eleAGT' para 'ele' (ElevadorAgente para Elevador) - [Agente saiu do elevador]
muda(X,Y,Mapa,MapaDepois) :- enesimo(Y, Mapa, Andar), enesimo(X, Andar, eleAGT), replace(Andar, X, ele, AndarNovo), replace(Mapa, Y, AndarNovo, MapaDepois), !.

% Muda 'trk' para 'trkAGT' (Caminhao para CaminhaoAgente) - [Agente entrou no caminhao]
muda(X,Y,Mapa,MapaDepois) :- enesimo(Y, Mapa, Andar), enesimo(X, Andar, trk), replace(Andar, X, trkAGT, AndarNovo), replace(Mapa, Y, AndarNovo, MapaDepois), !.

% Muda 'trkAGT' para 'trk' (CaminhaoAgente para Caminhao) - [Agente saiu do caminhao]
muda(X,Y,Mapa,MapaDepois) :- enesimo(Y, Mapa, Andar), enesimo(X, Andar, trkAGT), replace(Andar, X, trk, AndarNovo), replace(Mapa, Y, AndarNovo, MapaDepois), !.


%Mover para a direita
mover([X,Y], [X2,Y], Mapa) :- X2 is X+1, podeIr(X2,Y,Mapa).

%Mover para a esquerda
mover([X,Y], [X2,Y], Mapa) :- X2 is X-1, podeIr(X2,Y,Mapa).

%Mover para cima (elevador)
mover([X,Y], [X,Y2], Mapa) :- Y2 is Y+1, podeIr(X,Y2,Mapa), temElevador(X,Y,Mapa).

%Mover para baixo (elevador)
mover([X,Y], [X,Y2], Mapa) :- Y2 is Y-1, podeIr(X,Y2,Mapa), temElevador(X,Y,Mapa).


% Pega um objetivo do cenário
pegaObjetivo([Xa,Ya], Antes, Depois) :- % Pega objetivo apenas se
	mover([Xa,Ya],[Xd,Yd], Antes), % Conseguir se mover para a posição
	temObjetivo(Xd,Yd, Antes), % Tem objetivo na posição
	enesimo(Yd, Antes, Andar), % Pego o andar atual
	enesimo(Xd, Andar, obj), % Pego a posição no andar que tem obj
	replace(Andar, Xd, objAGT, AndarNovo), % Substitui para obj pego
	replace(Antes, Yd, AndarNovo, Depois). % Salvo o novo andar



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%[ Estados Sucessores ]%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Tres estados possíveis:

% 1- Andar
s(Antes, Depois) :- 
	posAgente(Xa, Ya, Antes), %Pega a pos do agente, antes de andar
	mover([Xa,Ya], [Xd,Yd], Antes), % Move o agente de posição (E atualiza o Mapa)
	muda(Xa, Ya, Xd, Yd, Antes, Depois),
	posAgente(Xd, Yd, Depois). % Pega a nova posição do agente

% 2- Pegar objetivo
s(Antes, Depois) :- 
	not( pertence(objAGT, Antes) ), % So pega objetivo, se não existir objAGT (objeto pego atualmente pelo agente) no mapa
	posAgente(X, Y, Antes),
	pegaObjetivo([X,Y], Antes, Depois),
	!.

% 3- Chegar ao caminhão
s(Antes, Depois) :- 
	pertence(objAGT, Antes), % Tem obj que foi pego no mapa
	posAgente(Xa, Ya, Antes), % pega a posicao do agente e salva em [Xa,Ya]
	mover([Xa,Ya], [Xd,Yd], Antes), % Tenta mover o agente para a posicao [Xd,Yd]
	muda(Xa, Ya, Xd, Yd, Antes, MapaDepois), % Após mover o agente, atualiza a informação do mapa de [Xd,Yd]
	posAgente(Xd, Yd, MapaDepois), % Pega a nova posição do agente e salva em [Xd,Yd]
	temCaminhao(Xd, Yd, MapaDepois), % Verifica se a posição que o agente está é um caminhão
	!, % CORTE - Depois que chegar ao caminhão, apenas vai procurar no mapa o objAGT (caixa que ele ta carregando) e transformar em caminho
	enesimo(Yobj, MapaDepois, Andar), % Procura um andar 
	enesimo(Xobj, Andar, objAGT), % Procura um objAGT (objeto pego) no mapa
	replace(Andar, Xobj, cam, AndarNovo), % Atualiza o objAGT para cam no Andar
	replace(MapaDepois, Yobj, AndarNovo, Depois), % Atualiza o andar no mapa
	!. % Achou o primeiro objAGT: para de procurar, só existe 1
	


% Meta do agente: LIMPAR O MAPA DOS OBJETIVOS
meta( Estado ) :-
	not(pertence(obj, Estado )), % Não tem objetivo no mapa
	not(pertence(objAGT, Estado )). % Não tem caixa objetivo que foi pega (e está transportando) no mapa

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%[ Funções de Busca em Largura ]%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%metodo que faz a extensao do caminho até os nos filhos do estado
estende([Estado|Caminho],ListaSucessores) :- bagof(
	[Sucessor,Estado|Caminho],
	(
		s(Estado,Sucessor),
		not(pertence(Sucessor,[Estado|Caminho]))
	), ListaSucessores), !.
estende( _ ,[]). %se o estado não tiver sucessor, falha e não procura mais (corte)


%solucao por busca em largura (bl)
solucao_bl(Inicial,Solucao) :- bl([[Inicial]],Solucao).

%Se o primeiro estado de F for meta, então o retorna com o caminho
bl([[Estado|Caminho]|_],[Estado|Caminho]) :- meta(Estado).

%falha ao encontrar a meta, então estende o primeiro estado até seus sucessores e os coloca no final da lista de fronteira
bl([Primeiro|Outros], Solucao) :- estende(Primeiro,Sucessores),
	concatena(Outros,Sucessores,NovaFronteira),
	bl(NovaFronteira,Solucao).



%solucao por busca em profundidade (bp)
solucao_bp(Inicial,Solucao) :- bp([],Inicial,Solucao). 

%Se o primeiro estado da lista é meta, retorna a meta
bp(Caminho,Estado,[Estado|Caminho]) :- meta(Estado).

%se falha, coloca o no caminho e continua a busca
bp(Caminho,Estado,Solucao) :- s(Estado,Sucessor), 
	not(pertence(Sucessor,[Estado|Caminho])), 
	bp([Estado|Caminho],Sucessor,Solucao).

/*
Cenário 1
[
[trk, agt, cam, ele, obj, box, cam, ele, cam, cam],
[cam, cam, cam, ele, cam, box, cam, ele, cam, cam],
[cam, cam, box, ele, cam, cam, cam, ele, cam, cam],
[cam, obj, cam, ele, box, cam, cam, ele, cam, cam],
[cam, cam, cam, ele, cam, box, cam, ele, obj, cam]
]


Cenário 2
[
[trk, agt, cam, ele, cam, box, cam, ele, obj, obj],
[obj, obj, cam, ele, cam, cam, cam, ele, cam, cam],
[cam, cam, box, ele, cam, box, cam, ele, cam, cam],
[cam, cam, cam, ele, cam, cam, cam, ele, cam, cam],
[cam, obj, cam, ele, box, box, box, ele, cam, box]
]


Cenário 3
[
[trk, agt, cam, ele, cam, box, cam, ele, obj, cam],
[cam, cam, box, ele, cam, box, cam, ele, cam, cam],
[cam, cam, box, ele, cam, box, cam, ele, cam, cam],
[obj, obj, obj, ele, cam, cam, cam, ele, cam, cam],
[cam, cam, box, ele, box, box, obj, ele, cam, cam]
]


[
[trk, agt, cam, ele, obj, box, obj, ele, obj, cam],
[cam, cam, box, ele, cam, box, cam, ele, cam, cam],
[cam, cam, box, ele, cam, box, cam, ele, cam, cam],
[obj, obj, obj, ele, cam, cam, cam, ele, cam, cam],
[cam, cam, box, ele, box, box, obj, ele, cam, cam]
]


Cenário Desafiador 1
[
[trk, ele, box, ele, agt, ele, box, ele, cam, obj],
[cam, ele, box, ele, box, ele, box, ele, box, cam],
[cam, ele, box, ele, box, ele, box, ele, box, cam],
[cam, ele, box, ele, box, ele, box, ele, box, cam],
[cam, ele, cam, ele, box, ele, cam, ele, box, cam]
]

Cenário Desafiador 1 (5x12)
[
[trk, ele, box, ele, agt, ele, box, ele, cam, obj, cam, cam],
[cam, ele, box, ele, box, ele, box, ele, box, cam, cam, cam],
[cam, ele, box, ele, box, ele, box, ele, box, cam, cam, cam],
[cam, ele, box, ele, box, ele, box, ele, box, cam, cam, cam],
[cam, ele, cam, ele, box, ele, cam, ele, box, cam, cam, cam]
]
*/
