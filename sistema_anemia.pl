%Os comentarios sobre o codigo estao no relatorio
:- use_module(library(pce)).
:- use_module(library(pce_style_item)).
:- dynamic sim/1,nao/1,homem/1,mulher/1,maior/1,menor/1.

help :- 
    write("Digite consulta. para comecar o programa.").

consulta :-
        new(D, dialog("Sistema especialista para deteccao de anemia e seus subtipos")),
		send(D, size, size(570,480)),
		new(T, text("O sistema ira tentar diagnosticar o tipo de anemia do paciente de acordo com as respostas.")),
		send(D, display, T, point(50,80)),
		send(D, append(button(ok, message(D, destroy)))),
		send(D, open),
		sleep(4),
		pergunta_digitar_n(Nome, "Informe o nome do paciente"),
		hipotese(Resultado), nl,
		new(K, dialog(Nome)),
		new(Z, text(Resultado)),
		new(Y, text("Segundo a analise, o paciente aparenta ")),
		send(K, display, Z, point(250,8)),
		send(K, display, Y, point(10,10)),
		send(K, size, size(450,200)),
		send(K, append(button(ok, message(K, destroy)))),
		send(K, open),
		registro(Resultado).

	
 /* hipoteses para teste */
 
hipotese(anemia_hemolitica_congenita) :- anemia_hemolitica_congenita, !.
hipotese(anemia_hemolitica_adquirida) :- anemia_hemolitica_adquirida, !.
hipotese(anemia_por_deficiencia) :- anemia_por_deficiencia, !.
hipotese(nao_ser_anemico). /* paciente aparenta ser saudavel */

 /* regras de identificacao */

anemia_hemolitica_congenita :- anemia, rbc_baixo, historico_congenito, determinante_congenito.

anemia_hemolitica_adquirida :- anemia, ldh_alto.

anemia_por_deficiencia :- anemia.

anemia :-
    verificarsexo(Paciente),
    (verificar(fraqueza);
	verificar(tonturas);
	verificar(desmaios);
	verificar(taquicardia);
	verificar(palidez);
	verificar(palpitacoes);
	verificar(ictericia)),
	compararhemo(Paciente),
	(compararhemo(Paciente);
    compararmenor(36,hematocritos)).
	
rbc_baixo :-
    compararmenor(4,rbc),
	compararmenor(4,rbc).
	
historico_congenito :-
    (verificar(ictericia);
	verificar(calculos_biliares);
	verificar(esfenomegalia);
	verificar(hepatomegalia);
	verificar(malformacoes_osseas);
	verificar(retardo_mental)).
	

determinante_congenito :-
    (verificar(microcitose);
	verificar(eliptocitose);
	verificar(esferocitose);
	verificar(anisopoikilocitose));
	verificar(anemia_relacionada_a_comida).
	
ldh_alto :-
    compararmaior(333, lactate_dehydrogenase),
	compararmaior(333, lactate_dehydrogenase).
   
 /* perguntas */ 

perguntar(Questao) :- 
        pergunta_digitar(Resposta, "|y/n| O paciente apresenta ", Questao),
        ( (Resposta == yes ; Resposta == y) 
        -> assert(sim(Questao)) ; 
        assert(nao(Questao)), fail). 	 
		
perguntagenero(Pessoa) :-
        pergunta_digitar_n(Sexo, "Informe o sexo do paciente (m/f)"),
		( (Sexo == m)
		-> assert(homem(Pessoa)) ;
		    ( (Sexo == f)
			-> assert(mulher(Pessoa));
			write("Invalido"))).

perguntacomp(S,X) :-
        informar_niveis(Valor, "Informe os niveis de ", X),
		( (Valor > S)
		-> assert(maior(X)) ;
		assert(menor(X))).
		

/* verificacoes e comparacoes */ 

verificar(X) :- 
    (sim(X)
	-> true ;
	    nao(X) -> fail ;
		    perguntar(X)). 

verificarsexo(Pessoa) :-
	(homem(Pessoa)
	-> 	true ;
	    mulher(Pessoa)
		-> true ;
		    perguntagenero(Pessoa)).
			
compararmenor(S, X) :-
    (menor(X)
	-> true ;
	    maior(X) -> fail ;
		    perguntacomp(S, X)).

compararmaior(S, X) :-
    (maior(X)
	-> true ;
	    menor(X) -> fail ;
		    perguntacomp(S, X)).

compararhemo(Pessoa) :-
    ( (homem(Pessoa))
	-> compararmenor(135, hemoglobina);
	    ( (mulher(Pessoa))
		-> compararmenor(120, hemoglobina);
		    write("Invalido"))).
			
pergunta_digitar_n(Resp, Pergunta) :-
        new(D, dialog("Sistema anemia")),
		new(G, text(Pergunta)),
		send(D, display, G, point(50,80)),
		send(D, size, size(500, 200)),
        send(D, append(new(ItemNome, text_item(r)))),
		send(D, append(button(ok, message(D, return, ItemNome?selection)))),
        send(D, append(button(cancel, message(D, return, @nil)))),
        send(D, default_button(ok)),
        get(D, confirm, Rval),
        free(D),
        Rval \== @nil,
        Resp = Rval.	

pergunta_digitar(Resp, Pergunta, Termo) :-
        new(D, dialog("Sistema anemia")),
		new(G, text(Pergunta)),
		new(J, text(Termo)),
		new(U, text("?")),
		send(D, display, U, point(300,8)),
		send(D, display, J, point(170,8)),
		send(D, display, G, point(10,10)),
		send(D, size, size(500, 200)),
        send(D, append(new(ItemNome, text_item(r)))),
		send(D, append(button(ok, message(D, return, ItemNome?selection)))),
        send(D, append(button(cancel, message(D, return, @nil)))),
        send(D, default_button(ok)),
        get(D, confirm, Rval),
        free(D),
        Rval \== @nil,
        Resp = Rval.				
		
informar_niveis(Resp, Pergunta, Termo) :-
        new(D, dialog("Sistema anemia")),
		new(G, text(Pergunta)),
		new(J, text(Termo)),
		send(D, display, J, point(170,8)),
		send(D, display, G, point(10,10)),
		send(D, size, size(500, 200)),
        send(D, append(new(ItemNome, text_item(r)))),
		send(D, append(button(ok, message(D, return, ItemNome?selection)))),
        send(D, append(button(cancel, message(D, return, @nil)))),
        send(D, default_button(ok)),
        get(D, confirm, Rval),
        free(D),
        Rval \== @nil,
		atom_number(Rval, X),
		Resp = X.

registro(Resultado) :-
        open(resultado, write, X),
		write(X,Resultado),
		write("registrado"),
		close(X).		

/* desfazer os asserts */ 

undo :- retract(sim(_)),fail. 
undo :- retract(nao(_)),fail. 
undo :- retract(sexo(_)),fail. 
undo :- retract(valor(_)),fail. 
undo. 