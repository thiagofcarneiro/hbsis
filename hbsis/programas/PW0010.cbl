       identification         		division.
       program-id.            		PW0010.
       environment            		division.
       configuration          		section.
       
       input-output           		section.
       file-control.
	      
	   *> Cliente
	   copy "/hbsis/selects/arq-cliente.sl"	   
	   *> Vendedor
	   copy "/hbsis/selects/arq-vendedor.sl"
	   *> Importar Cliente
	   copy "/hbsis/selects/arq-imp-cliente.sl"	   
	   *> Importar Vendedor
	   copy "/hbsis/selects/arq-imp-vendedor.sl"
       			 
			 
      *=================================================================		  
	   data                   		division.
	   file                   		section.
	   
	   *> Cliente
	   copy "/hbsis/fds/arq-cliente.fd"
	   
	   *> Vendedor
	   copy "/hbsis/fds/arq-vendedor.fd"
	    
	   *> Importar Cliente
	   copy "/hbsis/fds/arq-imp-cliente.fd"
	   
	   *> Importar Vendedor
	   copy "/hbsis/fds/arq-imp-vendedor.fd"
 
      *================================================================= 	   
       working-storage        		section.
       77  wk-separar       	    	pic x(080) value all "".
       77  wk-hifennn       	    	pic x(080) value all "-".
       77  wk-fstatus       	    	pic x(002) value spaces.
       77  wk-confirm       	    	pic x(001) value spaces.
       77  wk-opcao          	    	pic 9(004) value zeros.       
       77  wk-delayss       	    	pic x(001) value spaces.
       77  wk-arquivo                   pic x(006) value spaces.
       
       01  wk-area.
		   05 work-limpar               PIC X(060) VALUE SPACES.
            
      *=================================================================		   
       screen 						section.           
       01 tela-limpa.
	       02 line 14 col 01 pic x(70) 	using work-limpar.
	       02 line 15 col 01 pic x(70) 	using work-limpar.	 
	       02 line 16 col 01 pic x(70) 	using work-limpar.	 
	       02 line 17 col 01 pic x(70) 	using work-limpar.	 
	       02 line 18 col 01 pic x(70) 	using work-limpar.	 
	       02 line 19 col 01 pic x(70) 	using work-limpar.	 	 
	       02 line 20 col 01 pic x(70) 	using work-limpar.
	       02 line 21 col 01 pic x(70) 	using work-limpar.
	       02 line 22 col 01 pic x(70) 	using work-limpar.

	   01 tela-01-menu.
          02 line 1 col 1 reverse-video value "MENU PRINCIPAL". 
          02 line 06 col 22 value "0101 - Cadastro de Clientes       ".
          02 line 07 col 22 value "0102 - Cadastro de Vendedores      ".         
	      02 line 08 col 22 value "0201 - Relatorio de Clientes       ".
	      02 line 09 col 22 value "0202 - Relatorio de Vendedores  	 ".	    
	      02 line 10 col 22 value "0301 - Distribuir Clientes X  Vend.".                  
 	      02 line 09 col 22 value "9999 - Sair  	 ".	    
		  02 col 33 highlight value "digite opcao:".
          02 col 49 value "_____".	 
 		 
       01 tela-02-cadastro-cliente.
          02 line 1 col 1 reverse-video value "MENU CADASTRO CLIENTES". 
          02 line 06 col 22 value "0101 - inclusao         ".
          02 line 07 col 22 value "0102 - alteracao                   ".         
	      02 line 08 col 22 value "0103 - exclusao".
		  02 line 08 col 22 value "0104 - importar".
		  02 col 33 highlight value "digite opcao:".
          02 col 49 value "_____".	
		  
       01 tela-02-cadastro-vendedor.
          02 line 1 col 1 reverse-video value "MENU CADASTRO VENDEDORES". 
          02 line 06 col 22 value "0101 - inclusao         ".
          02 line 07 col 22 value "0102 - alteracao                   ".         
	      02 line 08 col 22 value "0103 - exclusao".
		  02 line 08 col 22 value "0104 - importar".
		  02 col 33 highlight value "digite opcao:".
          02 col 49 value "_____".	
           
          
       01 tela-02-cad-vend-inc.
          02 line 1 col 1 reverse-video value "CADASTRO VENDEDOR". 
          02 line 05 col 22 value "codigo vendedor: ".
          02 col  32 value "_____".  
          02 line 06 col 22 value "cpf: ".
          02 col  24 value "_________________________".  
          02 line 07 col 22 value "nome vendedor:".  
          02 col  49 value "_________________________________".
          02 line 07 col 22 value "latitude:".  
          02 col  49 value "_________________________________".  
          02 line 07 col 22 value "longitude:".  
          02 col  49 value "_________________________________".      

       01 tela-02-cad-cli-inc.
          02 line 1 col 1 reverse-video value "CADASTRO CLIENTE". 
          02 line 05 col 22 value "codigo cliente: ".
          02 col  32 value "_____".  
          02 line 06 col 22 value "cnpj: ".
          02 col  24 value "_________________________".  
          02 line 07 col 22 value "razao social:".  
          02 col  49 value "_________________________________".  
          02 line 07 col 22 value "latitude:".  
          02 col  49 value "_________________________________".  
          02 line 07 col 22 value "longitude:".  
          02 col  49 value "_________________________________".  
  		  
       01 tela-03-relatorio-cliente.
          02 line 1 col 1 reverse-video value "MENU RELATORIO CLIENTE". 
          02 line 07 col 22 value "0101-Lista Clientes - Ordem Ascend.  ".         
	      02 line 08 col 22 value "0102-Lista Clientes - Ordem Descend.".
		  02 line 08 col 22 value "0103-Filtro por Codigo Cliente".
		  02 line 08 col 22 value "0104-Filtro por Razao Social do Cliente".	  
		  02 col 33 highlight value "digite opcao:".
          02 col 49 value "_____".	

       01 tela-03-relatorio-vendedor.
          02 line 1 col 1 reverse-video value "MENU RELATORIO VENDEDOR". 
          02 line 07 col 22 value "0201-Lista Vendedores - Ordem Ascendente ".         
	      02 line 08 col 22 value "0202-Lista Vendedores - Ordem Descendente".
		  02 line 08 col 22 value "0203-Filtro por Codigo Vendedor".
		  02 line 08 col 22 value "0204-Filtro por Nome Vendedor".		  
		  02 col 33 highlight value "digite opcao:".
          02 col 49 value "_____".	

      *=================================================================	
	   procedure 					division.
       0000-principal    			section.
	    
	        perform 1000-inicializa  
	        perform 2000-processa
				until   wk-opcao  equal 9999 or esc			 
	        perform 3000-finaliza
	    .
       0000-principal-exit.
			exit.
			
      *=================================================================	
	   1000-inicializa				section.
	   	    display  tela-limpa .
			
	   1000-exit.
			exit.
			
      *=================================================================	
       2000-processa   				section.
       
       	    initialize 			 	wk-opcao
 
	        display  tela-01-menu
	        accept   wk-opcao		     at 1447 with upper
										auto-skip highlight.
            if esc
                     exit perform;
            end-if
            
            evaluate wk-opcao
               when 0101
					perform 2100-cadastro-cliente
	           when 0102
       	            perform 2200-cadastro-vendedor
               when 0201
					perform 2300-relatorio-cliente
	           when 0202
       	            perform 2400-relatorio-vendedor                       
               when 0301
	  	            perform 2800-distrib-clientes  
			   when 9999
					exit perform
            end-evaluate.
	   .
       2000-exit.
      *=================================================================	
       2100-cadastro-cliente        section.
	   
	        initialize wk-opcao
      	   
	        display tela-02-cadastro-cliente

	        accept  wk-opcao	     at 1447 with upper
                                        auto-skip highlight.   
            if esc 
                   exit perform;
            end-if
            
            evaluate work-opcao-menu
                when 0101
	                   perform 2110-cad-inc-cli
 	            when 0102
					   perform 2120-cad-alt-cli
	            when 0103
     	               perform 2130-cad-exc-cli
                when 0104
     	               perform 2140-cad-importacao  	
				when 9999
					   exit perform
			end-evaluate
       	   .
       2100-exit.
      *=================================================================	
       2110-cad-inc-cli           section.
		   	 
			display tela-02-cad-cli-inc
         
			accept rs-cod-cli      at 1534
			accept rs-cnpj  	   at 1634
			
			move   rs-cnpj 		   to wk-2115-cnpj-cpf
			move   1			   to wk-2115-tipo-cad
			perform 2115-consulta-cnpj			*> Consiste CPF / CNPJ
			if ws-status-arq equal 99
				exit perform
			end-if

			accept rs-razao-social      at 1734
			accept rs-latitude-cli		at 1834
			accept rs-longitude-cli		at 1934
	
			open extend  arq-cliente
			if ws-resultado-acesso equal "00"
				write arq-cliente from rs-arq-cli
			end-if   

			close arq-cliente
			
       2110-exit.
      *=================================================================
       2115-consulta-cnpj           section.
	   
		   if wk-2115-tipo-cad equal 1			*> tipo cliente
			   if wk-2115-cnpj-cpf < 14
					move 99 		to ws-status-arq
					display "cnpj invalido"		
					exit section
			   end-if 
		   end-if 
		   
		   if wk-2115-tipo-cad equal 2			*> tipo vendedor
			   if wk-2115-cnpj-cpf < 11
					move 99 		to ws-status-arq
					display "CPF invalido"		
					exit section
			   end-if 
		   end-if 
		   
		   perform until ws-status-arq <> 00
			   read arqclientes next
			   if w-cod-cnpj equal w-reg-cpf-cnpj
					move 99				ws-status-arq
					display "ja existe cnpj ou cpf cadastrado!"
					exit perform
			   end-if
		   end-perform
	   
	   2115-exit.
      *=================================================================
       2120-cad-alt-cli           section.
	   
			initialize rs-arq-cli

			open i-o  arq-cliente
			if ws-resultado-acesso equal "00"
				display  "Entre Codigo Cliente para alterar:"
				accept rs-cod-cli
			    start arq-cliente key is = rs-cod-cli
			    if ws-resultado-acesso equal "00"
					read arq-cliente
				    display tela-02-cad-cli-inc
					accept rs-cod-cli      at 1534
					accept rs-cnpj  	   at 1634
				    move   rs-cnpj 		   to wk-2115-cnpj-cpf
					move   1			   to wk-2115-tipo-cad
					perform 2115-consulta-cnpj	*> Consiste CPF / CNPJ
					if ws-status-arq equal 99
						exit perform
					end-if

					accept rs-razao-social      at 1734
					accept rs-latitude-cli		at 1834
					accept rs-longitude-cli		at 1934
	
				    rewrite arq-cliente from rs-arq-cli
			    end-if
			end-if   
	      .
       2120-exit.
			exit.
      *=================================================================	
       2130-cad-exc-cli            section.
        
			initialize rs-arq-cli

			open i-o  arq-cliente
			if ws-resultado-acesso equal "00"
				display  "Entre Codigo Cliente para excluir:"
				accept rs-cod-cli
				
			    start arq-cliente key is = rs-cod-cli
			    if ws-resultado-acesso equal "00"
					read arq-cliente
				    display tela-02-cad-cli-inc
					
					display "Deseja Excluir registro (s / n)"
					accept wk-opcao
					if wk-opcao equal 'S' or 's'
				       delete arq-cliente from rs-arq-cli
					end-if
			    end-if
			end-if   		
		
	      .
       2130-exit.
			exit.
      *=================================================================	
	   2140-cad-importacao          section.
  		   
		   display tela-02-importacao

		   accept w-arq-caminho-importar
		   
		   if w-arq-caminho-importar is null
				display "Caminho vazio do arquivo importar"
				exit perform
		   end-if
		   
		   if wk-opcao equal 0104    *> Cliente
				open i-o arq-cliente
				open i-o arq-imp-cli
				if ws-resultado-acesso equal 00 then
					perform until ws-resultado-acesso not equal zeros
						  read arq-imp-cli next
						  move rs-imp-cod-cli			to rs-cod-cli 
						  move rs-imp-cnpj				to rs-cnpj
						  move rs-imp-razao-social 		to rs-razao-social
						  move rs-imp-lat-cli			to rs-latitude-cli
						  move rs-imp-long-cli			to rs-longitude-cli
						  write arq-cliente from rs-arq-cli
					end-perform
				end-if 
				close arq-cliente
				close arq-imp-cli
		   else	
				open i-o arq-vendedor
				open i-o arq-imp-vend
				if ws-resultado-acesso equal 00 then
					perform until ws-resultado-acesso not equal zeros
						  read arq-imp-vend next
						  move rs-imp-cod-vend			to rs-cod-vend
						  move rs-imp-cpf				to rs-cpf
						  move rs-imp-nome 				to rs-nome
						  move rs-imp-lat-vend			to rs-latitude-vend
						  move rs-imp-long-vend			to rs-latitude-vend
						  write arq-vendedor from rs-arq-vend
					  end-if
					end-perform
				end-if   
				close arq-vendedor
				close arq-imp-vend
		   end-if
       2140-exit.
			exit.
			
      *=================================================================
       2200-cadastro-vendedor        section.			
	        initialize wk-opcao
      	   
	        display tela-02-cadastro-vendedor

	        accept  wk-opcao	     at 1447 with upper
                                        auto-skip highlight.   
            if esc 
                   exit perform;
            end-if
            
            evaluate work-opcao-menu
                when 0101
	                   perform 2210-cad-incl-vend
 	            when 0102
					   perform 2220-cad-alt-vend
	            when 0103
     	               perform 2230-cad-exc-vend
                when 0104
     	               perform 2140-cad-importacao  	
				when 9999
					   exit perform
			end-evaluate			
	   2200-exit.
			exit.
			
      *=================================================================	
       2210-cad-incl-vend            section.
		   	 
			display tela-02-cad-vend-inc
         
			accept rs-cod-cli      at 1534
			accept rs-cnpj  	   at 1634
			
			move   rs-cnpj 		   to wk-2115-cnpj-cpf
			move   1			   to wk-2115-tipo-cad
			perform 2115-consulta-cnpj		*> Consiste CPF / CNPJ
			if ws-status-arq equal 99
				exit perform
			end-if

			accept rs-razao-social      at 1734
			accept rs-latitude-cli		at 1834
			accept rs-longitude-cli		at 1934
	
			open extend  arq-cliente
			if ws-resultado-acesso equal "00"
				write arq-cliente from rs-arq-cli
			end-if   

			close arq-cliente
			
       2210-exit.
			exit.
      *=================================================================	
       2220-cad-alt-vend           section.
	   
			initialize rs-arq-vend

			open i-o  arq-cliente
			if ws-resultado-acesso equal "00"
				display  "Entre Codigo Cliente para alterar:"
				accept rs-cod-cli
			    start arq-cliente key is = rs-cod-cli
			    if ws-resultado-acesso equal "00"
					read arq-cliente
				    display tela-02-cad-cli-inc
					accept rs-cod-cli      at 1534
					accept rs-cnpj  	   at 1634
				    move   rs-cnpj 		   to wk-2115-cnpj-cpf
					move   1			   to wk-2115-tipo-cad
					perform 2115-consulta-cnpj		*> Consiste CPF / CNPJ
					if ws-status-arq equal 99
						exit perform
					end-if

					accept rs-razao-social      at 1734
					accept rs-latitude-cli		at 1834
					accept rs-longitude-cli		at 1934
	
				    rewrite arq-cliente from rs-arq-cli
			    end-if
			end-if   
	      .
       2220-exit.
			exit.
      *===============================================================
       2230-cad-exc-vend            section.
        
			initialize rs-arq-cli

			open i-o  arq-cliente
			if ws-resultado-acesso equal "00"
				display  "Entre Codigo Cliente para excluir:"
				accept rs-cod-cli
				
			    start arq-cliente key is = rs-cod-cli
			    if ws-resultado-acesso equal "00"
					read arq-cliente
				    display tela-02-cad-cli-inc
					
					display "Deseja Excluir registro (s / n)"
					accept wk-opcao
					if wk-opcao equal 'S' or 's'
				       delete arq-cliente from rs-arq-cli
					end-if
			    end-if
			end-if   		
		
	      .
       2230-exit.
			exit.
      *=================================================================	
       2300-relatorio-cliente       section.
		    initialize 				ws-opcao
			
			display tela-03-relatorio-cliente
			accept ws-opcao
			
		    evaluate ws-opcao
			
			when 0101
			when 0102
					perform 2311-lista-cli-asc-desc
			when 0103
					perform 2313-filtro-por-cod-cli
			when 0104
					perform 2314-filtro-por-razao 					
			end-evaluate.

       2300-exit. 
			exit.


      *=================================================================	
       2311-lista-cli-asc-desc	         section.			
			initialize rs-cod-cli

			open input arq-cliente
			if ws-resultado-acesso equal zeros
			
				if ws-opcao equal 0101					*> Ascendente
					start arq-cliente key is >= rs-cod-cli
				else 
					initialize rs-cod-cli				*> Descendente
					move 9999		     to rs-cod-cli
					start arq-cliente key is <= rs-cod-cli
				end-if	
					
				display "Codigo;CNPJ;Razao Social;Latitude;Longitude"
				perform until ws-resultado-acesso not equal zeros
						read next arq-cliente 
						display rs-arq-cli
				end-perform
			
			end-if
	
	   2311-exit.
			exit.
			
      *=================================================================	
 	   2313-filtro-por-cod-cli	
			display tela-limpa

			open input arq-clientes
			if ws-resultado-acesso equal zeros
				display "Entre Codigo Cliente para filtra relatorio:"
				accept rs-cod-cli
			
				start arq-cliente key is = rs-cod-cli
				read  arq-cliente
				display rs-arq-cliente
			    close arq-clientes
			end-if.
			
	   2313-exit.
			exit.

      *=================================================================	
 	   2314-filtro-por-razao	
			display tela-limpa

			open input arq-clientes
			if ws-resultado-acesso equal zeros
				display "Entre Razao Social Cliente para filtra relatorio:"
				accept rs-razao-social
			
				start arq-cliente key is = rs-razao-social
				read  arq-cliente
				display rs-arq-cliente
			    close arq-clientes
			end-if.
			
	   2314-exit.
			exit.
			
      *=================================================================	
       2400-relatorio-vendedor       section.
		    initialize 				ws-opcao
			
			display tela-03-relatorio-vendedor
			accept ws-opcao
			
		    evaluate ws-opcao				
				when 0201
				when 0202
						perform 2422-lista-vend-asc-desc
				when 0203
						perform 2423-filtro-por-cod-vend
				when 0204
						perform 2424-filtro-por-nome
			end-evaluate.

       2400-exit. 
			exit.
	
      *=================================================================	
	   2422-lista-vend-asc-desc		section.
			initialize rs-arq-vend

			open input arq-vendedor
			if ws-resultado-acesso equal zeros
			
				if ws-opcao equal 0101						*> Ascendente
					start arq-cliente key is >= rs-cod-vend
				else 										*> Descendente
					move 9999		     to rs-cod-vend
					start arq-cliente key is <= rs-cod-vend
				end-if	
					
				display "Codigo;CPF;NOME;Latitude;Longitude"
				perform until ws-resultado-acesso not equal zeros
						read next arq-vendedor
						display rs-arq-vend
				end-perform
			
			end-if
	   
	   2422-exit.
			exit.
			
      *=================================================================	
	   2423-filtro-por-cod-vend		section.
			display tela-limpa

			open input arq-vendedor
			if ws-resultado-acesso equal zeros
				display "Entre Codigo Vendedor para filtra relatorio:"
				accept rs-cod-vend	
				start arq-vendedor key is = rs-cod-vend
				read  arq-vendedor
				display rs-arq-vendedor
			    close arq-vendedor
			end-if.
			
	   
	   2423-exit.
			exit.
			

      *=================================================================	
 	   2424-filtro-por-razao	
			display tela-limpa
			open input arq-vendedor
			if ws-resultado-acesso equal zeros
				display "Entre Nome Vendedor para filtra relatorio:"
				accept rs-nome
			
				start arq-vendedor key is = rs-nome
				read  arq-vendedor
				display rs-arq-vendedor
			    close arq-vendedor
			end-if.
			
	   2424-exit.
			exit.

      *=================================================================	
       2800-distrib-clientes        section.

		   move      work-nomearq       to  lk-caminho.
           move      zeros              to  lk-regfunc.
           call      "/hbsis/programas/pw00900" using lk-rot405.
           cancel    "pw00900".

       2800-exit.
			exit.
      *=================================================================	        
       3000-finaliza				section.
           exit      program
       	   stop      run
       	   .
       3000-exit.
           exit.