       IDENTIFICATION         		DIVISION.
       PROGRAM-ID.            		PROVACOBOL.
       SECURITY.
       ENVIRONMENT            		DIVISION.
       CONFIGURATION          		SECTION.
       SOURCE-COMPUTER.      		AIX-34.
       OBJECT-COMPUTER.      		AIX-34.
       SPECIAL-NAMES.
           				DECIMAL-POINT IS COMMA
           				CONSOLE IS CRT,
           				CRT STATUS IS WORK-TECLASS.
       
       INPUT-OUTPUT           		SECTION.
       FILE-CONTROL.
       
       select arq-cliente assign to disk wid-arq-cliente
             organization       is indexed
             access mode        is dynamic
             record key         is ...
             lock mode          is manual
             file status        is ws-resultado-acesso.   
			 
      *============================================================================= 		  
	   DATA                   		DIVISION.
	   FILE                   		SECTION.
       FD CARGA-VENDEDOR.
       01  XLS-CARGA-VEND.
    	   05  XLS-COD-VENDEDOR         PIC 9(003).
    	   05  FILLER                   PIC X(001).
    	   05  XLS-CPF         	        PIC 9(011).
    	   05  FILLER                   PIC X(001).   
    	   05  XLS-NOME        	        PIC 9(040).
    	   05  FILLER                   PIC X(001).             	  
     	   05  XLS-LATITUDE         	PIC S9(003)V9(008).
    	   05  FILLER                   PIC X(001).
    	   05  XLS-LONGITUDE         	PIC S9(003)V9(008).
    	   05  FILLER                   PIC X(001).    	   
 
       FD CARGA-CLIENTE.
       01  XLS-CARGA-CLIENTE.
    	   05  XLS-COD-CLIENTES         PIC 9(003).
    	   05  FILLER                   PIC X(001).
    	   05  XLS-CNPJ        	        PIC 9(011).
    	   05  FILLER                   PIC X(001).   
    	   05  XLS-RAZAO-SOCIAL        	PIC 9(040).
    	   05  FILLER                   PIC X(001).             	  
     	   05  XLS-LATITUDE         	PIC S9(003)V9(008).
    	   05  FILLER                   PIC X(001).
    	   05  XLS-LONGITUDE         	PIC S9(003)V9(008).
    	   05  FILLER                   PIC X(001).   
           
      *============================================================================= 	   
       WORKING-STORAGE        		SECTION.
       77  WORK-SEPARAR       	    	PIC X(080) VALUE ALL "".
       77  WORK-HIFENNN       	    	PIC X(080) VALUE ALL "-".
       77  WORK-FSTATUS       	    	PIC X(002) VALUE SPACES.
       77  WORK-CONFIRM       	    	PIC X(001) VALUE SPACES.
       77  WORK-OPCAO          	    	PIC 9(004) VALUE ZEROS.       
       77  WORK-DELAYSS       	    	PIC X(001) VALUE SPACES.
       77  WORK-ARQUIVO                 PIC X(006) VALUE SPACES.
       
       01  WORK-AREA.
            
      *============================================================================= 		   
       SCREEN 						SECTION.           
       01 TELA-LIMPA.
	       02 LINE 14 COL 01 PIC X(70) 	USING WORK-LIMPAR.
	       02 LINE 15 COL 01 PIC X(70) 	USING WORK-LIMPAR.	 
	       02 LINE 16 COL 01 PIC X(70) 	USING WORK-LIMPAR.	 
	       02 LINE 17 COL 01 PIC X(70) 	USING WORK-LIMPAR.	 
	       02 LINE 18 COL 01 PIC X(70) 	USING WORK-LIMPAR.	 
	       02 LINE 19 COL 01 PIC X(70) 	USING WORK-LIMPAR.	 	 
	       02 LINE 20 COL 01 PIC X(70) 	USING WORK-LIMPAR.
	       02 LINE 21 COL 01 PIC X(70) 	USING WORK-LIMPAR.
	       02 LINE 22 COL 01 PIC X(70) 	USING WORK-LIMPAR.

	   01 TELA-01.
          02 LINE 1 COL 1 REVERSE-VIDEO VALUE "Prova Cobol". 
          02 LINE 05 COL 22 VALUE "Cadastros		   ".
          02 LINE 06 COL 22 VALUE "01.01 - Cadastro de Cliente         ".
          02 LINE 07 COL 22 VALUE "01.02 - Cadastro de Vendedor                   ".         
	      02 LINE 08 COL 22 VALUE "Relatorios".
	      02 LINE 09 COL 22 VALUE "02.01 - Relatório de Clientes           ".
	      02 LINE 10 COL 22 VALUE "02.02 - Relatório de Vendedores  	 ".	  
	      02 LINE 11 COL 22 VALUE "Executar          ".	  
	      02 LINE 12 COL 22 VALUE "03.01 - Executar Distrib.o de Clientes       ".                  
          02 COL 33 HIGHLIGHT VALUE "Digite Opcao:".
          02 COL 49 VALUE "_____".	 
 		 
       01 TELA-02.
          02 LINE 1 COL 1 REVERSE-VIDEO VALUE "Prova Cobol". 
          02 LINE 05 COL 22 VALUE "Cadastros		   ".
          02 LINE 06 COL 22 VALUE "01.01 - Inclusao         ".
          02 LINE 07 COL 22 VALUE "01.02 - Alteracao                   ".         
	      02 LINE 08 COL 22 VALUE "01.03 - Exclusao".
		  02 LINE 08 COL 22 VALUE "01.04 - Importar".
		  02 COL 33 HIGHLIGHT VALUE "Digite Opcao:".
          02 COL 49 VALUE "_____".	
		  
       01 TELA-03.
          02 LINE 1 COL 1 REVERSE-VIDEO VALUE "Prova Cobol". 
          02 LINE 05 COL 22 VALUE "Relatorios		   ".
          02 LINE 06 COL 22 VALUE "         			".
          02 LINE 07 COL 22 VALUE "01.01 - Clientes                   ".         
	      02 LINE 08 COL 22 VALUE "01.02 - Vendedores".
		  02 COL 33 HIGHLIGHT VALUE "Digite Opcao:".
          02 COL 49 VALUE "_____".			  
      *============================================================================= 	
	   PROCEDURE 					DIVISION.
       0000-PRINCIPAL    			SECTION.
	    
	         PERFORM 1000-INICIALIZA  
	         PERFORM 2000-PROCESSA 
	         PERFORM 3000-FINALIZA
	    .
       0000-PRINCIPAL-EXIT.
      *============================================================================= 	
	   1000-INICIALIZA				SECTION.
	   	    DISPLAY  TELA-LIMPA .
			
	   1000-EXIT.
      *============================================================================= 	
       2000-PROCESSA   				SECTION.
       
       	    INITIALIZE 			 	WORK-OPCAO
 
	        DISPLAY  TELA-01
	        DISPLAY  WORK-DATA	    AT 0171 WITH REVERSE-VIDEO  
	   
	        ACCEPT   WORK-OPCAO     AT 1447 WITH UPPER
                                    AUTO-SKIP HIGHLIGHT.
            IF ESC
                     exit perform;
            END-IF
            
            EVALUATE WORK-OPCAO
                WHEN 0101
	            WHEN 0102
       	              PERFORM 2000-CADASTROS
                WHEN 0201
	            WHEN 0202
       	              PERFORM 2200-RELATORIOS                       
                WHEN 0301
	  	              PERFORM 2800-DISTRIB-CLIENTES
       	        WHEN OTHER
       	       	    EXIT PERFORM          
            END-EVALUATE.
	   .
       2000-EXIT.
      *============================================================================= 	
       2000-CADASTROS             		SECTION.
	        INITIALIZE 					XLS-REGISTR
      	   
	        DISPLAY TELA-02
	        DISPLAY "CADASTROS"     	AT 1623
           
        	INITIALIZE 					WORK-OPCAO
       
	        DISPLAY  TELA-LIMPA
	        DISPLAY  WORK-DATA	        AT 0171 WITH REVERSE-VIDEO  
	        ACCEPT   WORK-OPCAO         AT 1447 WITH UPPER
                                        AUTO-SKIP HIGHLIGHT.   
            IF ESC
                   exit perform;
            END-IF
            
            EVALUATE WORK-OPCAO
                WHEN 01
	                    PERFORM 2110-CAD-INCLUSAO
 	            WHEN 02
						PERFORM 2120-CAD-ALTERACAO
	            WHEN 03
     	                PERFORM 2130-CAD-EXCLUSAO
                WHEN 04
     	                PERFORM 2140-CAD-IMPORTACAO                       
				WHEN OTHER
       	       	    EXIT PERFORM. 
			END-EVALUATE
       	   .
       2000-EXIT.
      *============================================================================= 	
       2110-CAD-INCLUSAO             SECTION.
		   CLOSE ARQXLS
		                      
           OPEN I-O  ARQXLS
           IF   WORK-FSTATUS EQUAL "00"
       	        MOVE "TESTE"        TO XLS-NRCARGA
                MOVE "XX"			TO XLS-SQCARGA
				WRITE XLS-REGISTR
			END-IF   
			
			
			DISPLAY "Codigo Cliente:"
			DISPLAY "Codigo Vendedor:"
			
			DISPLAY "CNPJ:"
			DISPLAY "CPF:"
			
			DISPLAY "Razao Social:"
			DISPLAY "Nome Vendedor:"
			
			DISPLAY "Latitude:"
			
			DISPLAY "Longitude:"
			
			
	   
			CLOSE ARQXLS.
       2110-EXIT.
      *============================================================================= 	
       2120-CAD-ALTERACAO           SECTION.
        
		   
		   OPEN I-O arqclientes
		   if status equal 39 then
		   
		   end if           
  
	      .
       2120-EXIT.
			EXIT.
      *============================================================================= 	
       2130-CAD-EXCLUSAO               		SECTION.
        .
  		   
		   OPEN I-O arqclientes
		   if status equal 39 then
		   
		   end if         
  
	      .
       2130-EXIT.
			EXIT.
      *============================================================================= 	
	   2140-CAD-IMPORTACAO              	SECTION.

       2140-EXIT.
			EXIT.
      *============================================================================= 	
       2200-RELATORIOS              		SECTION.

       2200-EXIT. 
			EXIT.
      *============================================================================= 		
       2800-DISTRIB-CLIENTES              	SECTION.

       2800-EXIT.
			EXIT.
      *============================================================================= 	        
       3000-FINALIZA			SECTION.
           EXIT      PROGRAM
       	   STOP      RUN
       	   .
       3000-EXIT.
           EXIT.
