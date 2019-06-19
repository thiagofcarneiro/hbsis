      *---------------------------------------------------------------*
       identification         		division.
      *---------------------------------------------------------------*
       program-id.            		pw0090.
       security.
      *---------------------------------------------------------------* 
       environment            		division.
      *---------------------------------------------------------------* 
       configuration          		section.
      *---------------------------------------------------------------*
       special-names.
           				decimal-point is comma.
      *---------------------------------------------------------------*
       input-output           		section.
      *---------------------------------------------------------------*
       file-control.
      *---------------------------------------------------------------*
	    select arqxls     assign work-nomearq
	                      organization is line sequential
               		      file status  is work-fstatus.               		      
               		      
          
             
      *---------------------------------------------------------------*
       data                   		division.
      *---------------------------------------------------------------*
       file                   		section.
      *---------------------------------------------------------------*
       fd  arqxls.
       01  xls-registr.
    	   05  xls-nrcarga         	pic x(006).
           05  xls-filler1         	pic x(001).
	   05  xls-ordcarg         	pic x(002).
    	   05  xls-filler2         	pic x(001).
	   05  xls-sqcarga         	pic x(003).
    	   05  xls-filler3         	pic x(001).       
         
           
      *---------------------------------------------------------------*
       working-storage        		section.
      *---------------------------------------------------------------*   
      
       77  work-separar       	    	pic x(080) value all "".
       77  work-hifennn       	    	pic x(080) value all "-".
       77  work-fstatus       	    	pic x(002) value spaces.
       77  work-confirm       	    	pic x(001) value spaces.
       77  work-opcao          	    	pic 9(001) value zeros.       
       77  work-delayss       	    	pic x(001) value spaces.
       77  work-arquivo                 pic x(006) value spaces.
       
       01  work-area.
	   03 work-mestemp     	    	pic 9(002) value zeros.  
	   03 work-flag-primeira	pic x(003) value spaces.   
       	   03 work-limpar               pic x(060) value spaces.
       	   03 work-sldacum		pic --.--9,99.
       	   03 work-prioridade           pic 9(007) value zeros.
   	   03 work-cab		        pic x(060) value spaces.  

             
      *---------------------------------------------------------------*	   
       screen 				section.
      *---------------------------------------------------------------*       
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

           			            
       01 tela-01.
	  02 line 15 col 22 value "".
          02 line 16 col 22 value "                               ".
	  02 line 17 col 22 value "                               ".
          02 line 18 col 22 value "                               ".
          02 line 19 col 22 value "                               ".
          02 line 20 col 22 value "                               ".          
          02 line 21 col 22 value "                               ".                    
	  02 line 22 col 22 value "".         
         
      *---------------------------------------------------------------*
       procedure 			division .
      *---------------------------------------------------------------*
       0000-principal    		section.
	    
	    perform 1000-inicializa  
	    perform 2000-processa 
	      	    until   work-opcao  equal 9 or esc
	    perform 3000-finaliza
	    .
       0000-principal-exit.
       	   exit.
      *---------------------------------------------------------------*
      * inicializacao						      					  *
      *---------------------------------------------------------------*
       1000-inicializa			section.
           
  	   .
	   1000-inicializa-exit.
	   exit.
      *---------------------------------------------------------------*
      * processa  						      						  *
      *---------------------------------------------------------------*
       2000-processa   			section.
				
   
	   .
       2000-processa-exit.
           exit.
           
      *---------------------------------------------------------------*
      * finalizacao						      *
      *---------------------------------------------------------------*
       3000-finaliza			section.
           exit      program
       	   stop      run
       	   .
       3000-finaliza-exit.
           exit.
      *----------------------------fim--------------------------------*