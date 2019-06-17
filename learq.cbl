      *****************************************************************
      ********     DESENVOLVIDO POR OSTEN FERRAGENS LTDA         ******
      *****************************************************************
      *---------------------------------------------------------------*
       IDENTIFICATION         		DIVISION.
      *---------------------------------------------------------------*
       PROGRAM-ID.            		LEARQ.
       SECURITY.
      *****************************************************************
      *   SISTEMA     . QBG                                           *
      *   PROGRAMA    . LEARQ                                         *
      *   ANALISTA    . THIAGO                                        *
      *   PROGRAMADOR . THIAGO                                        *
      *   CODIFICACAO . 26abr12 13:07                                 *
      *   MANUTENCAO  .                                               *
      *   FUNCAO      . LE ARQUIVO   			              *
      *****************************************************************
      *---------------------------------------------------------------* 
       ENVIRONMENT            		DIVISION.
      *---------------------------------------------------------------* 
       CONFIGURATION          		SECTION.
      *---------------------------------------------------------------*
       SOURCE-COMPUTER.      		AIX-34.
       OBJECT-COMPUTER.      		AIX-34.
       SPECIAL-NAMES.
           				DECIMAL-POINT IS COMMA
           				CONSOLE IS CRT,
           				CRT STATUS IS WORK-TECLASS.
      *---------------------------------------------------------------*
       INPUT-OUTPUT           		SECTION.
      *---------------------------------------------------------------*
       FILE-CONTROL.
      *---------------------------------------------------------------*
	    SELECT ARQXLS     ASSIGN WORK-NOMEARQ
	                      ORGANIZATION IS LINE SEQUENTIAL
               		      FILE STATUS  IS WORK-FSTATUS.

	    SELECT ARQXLS2     ASSIGN WORK-NOMEARQ2
	                      ORGANIZATION IS LINE SEQUENTIAL
               		      FILE STATUS  IS WORK-FSTATUS.  
               		      
	    SELECT ARQXLS3     ASSIGN WORK-NOMEARQ3
	                      ORGANIZATION IS LINE SEQUENTIAL
               		      FILE STATUS  IS WORK-FSTATUS.   
	    SELECT ARQXLS4     ASSIGN WORK-NOMEARQ4
	                      ORGANIZATION IS LINE SEQUENTIAL
               		      FILE STATUS  IS WORK-FSTATUS.   
               		      
	    SELECT CARGA      ASSIGN WORK-NOMEARQ5
	                      ORGANIZATION IS LINE SEQUENTIAL
               		      FILE STATUS  IS WORK-FSTATUS.                		      
               		      
               		      
      *
            COPY   "/desenv/cobol/selects/sce/sce079.sl".
      *----Arquivo de LOG                  
            COPY   "/desenv/cobol/selects/sys/sys047.sl". 
      *----Arquivo de Pedidos      
            COPY   "/desenv/cobol/selects/sce/sce057.sl".    
      *----Arquivo de Mov. Volumes      
            COPY   "/desenv/cobol/selects/sce/sce135.sl".  
      *----Arquivo de locacao      
      *     COPY   "/desenv/cobol/selects/sce/sce091.sl".  
      *----Arquivo de Nota
            COPY   "/desenv/cobol/selects/sft/sft070.sl".
      *----Arquivo de Nota
            COPY   "/desenv/cobol/selects/sft/sft072.sl".
      *----Arquivo de Nota
            COPY   "/desenv/cobol/selects/sft/sft073.sl".            
      *----Arquivo de 
            COPY   "/desenv/cobol/selects/sft/sft006.sl".             
      *----Arquivo de Nota entrada
            COPY   "/desenv/cobol/selects/scp/scp002.sl".  
      *----Arquivo de Itens
            COPY   "/desenv/cobol/selects/sce/sce001.sl".   
      *----Arquivo de Reserva
            COPY   "/desenv/cobol/selects/com/com020.sl".              
      *----Arquivo de Saldo acumulado e Reserva
            COPY   "/desenv/cobol/selects/com/com021.sl".            
             
      *---------------------------------------------------------------*
       DATA                   		DIVISION.
      *---------------------------------------------------------------*
       FILE                   		SECTION.
      *---------------------------------------------------------------*
       FD  ARQXLS.
       01  XLS-REGISTR.
    	   05  XLS-NRCARGA         	PIC X(006).
           05  XLS-FILLER1         	PIC X(001).
	   05  XLS-ORDCARG         	PIC X(002).
    	   05  XLS-FILLER2         	PIC X(001).
	   05  XLS-SQCARGA         	PIC X(003).
    	   05  XLS-FILLER3         	PIC X(001).       
    	   
       FD  ARQXLS2.
       01  XLS-REGISTR2                 PIC X(200).
       
       FD  ARQXLS3.
       01  XLS-REGISTR3                 PIC X(080).     
       
       FD  ARQXLS4.
       01  XLS-REGISTR4                 PIC X(200).   
       
       FD  CARGA.
       01  XLS-CARGA.
    	   05  XLS-FILIAL          	PIC 9(002).
    	   05  FILLER                   PIC X(001).
    	   05  XLS-CODVEND         	PIC 9(005).
    	   05  FILLER                   PIC X(001).    	   
	   05  XLS-SLDACUM         	PIC ZZZ,ZZ.
         
   
          
           COPY   "/desenv/cobol/fds/sce/sce079.fd".
           COPY   "/desenv/cobol/fds/sys/sys047.fd".
           COPY   "/desenv/cobol/fds/sce/sce057.fd". 
        
           COPY   "/desenv/cobol/fds/sce/sce135.fd". 
           COPY   "/desenv/cobol/fds/sft/sft006.fd".            
           COPY   "/desenv/cobol/fds/sft/sft070.fd". 
           COPY   "/desenv/cobol/fds/sft/sft072.fd". 
           COPY   "/desenv/cobol/fds/sft/sft073.fd".            
           COPY   "/desenv/cobol/fds/scp/scp002.fd".    
           COPY   "/desenv/cobol/fds/sce/sce001.fd". 
	   COPY   "/desenv/cobol/fds/com/com021.fd".            
           COPY   "/desenv/cobol/fds/com/com020.fd".            
           
      *---------------------------------------------------------------*
       WORKING-STORAGE        		SECTION.
      *---------------------------------------------------------------*
 	   COPY   "/desenv/cobol/works/wk-rot405".      
           COPY   "/desenv/cobol/works/wk-datasis".
           COPY   "/desenv/cobol/works/wk-rot024".
           COPY   "/desenv/cobol/works/wk-teclas".
           COPY   "/desenv/cobol/works/wk-rot002".
           COPY   "/desenv/cobol/works/wk-rot005".
      
       77  WORK-SEPARAR       	    	PIC X(080) VALUE ALL "".
       77  WORK-HIFENNN       	    	PIC X(080) VALUE ALL "-".
       77  WORK-FSTATUS       	    	PIC X(002) VALUE SPACES.
       77  WORK-CONFIRM       	    	PIC X(001) VALUE SPACES.
       77  WORK-OPCAO          	    	PIC 9(001) VALUE ZEROS.       
       77  WORK-DELAYSS       	    	PIC X(001) VALUE SPACES.
       77  WORK-ARQUIVO                 PIC X(006) VALUE SPACES.
       
       01  WORK-AREA.
	   03 WORK-MESTEMP     	    	PIC 9(002) VALUE ZEROS.  
	   03 WORK-FLAG-PRIMEIRA	PIC X(003) VALUE SPACES.   
       	   03 WORK-LIMPAR               PIC X(060) VALUE SPACES.
       	   03 WORK-SLDACUM		PIC --.--9,99.
       	   03 WORK-PRIORIDADE           PIC 9(007) VALUE ZEROS.
   	   03 WORK-CAB		        PIC X(060) VALUE SPACES.  
	   03 WORK-SLDANT		PIC S9(006)V99.
	   03 WORK-DETLC.
	      05 WORK-DETLC01		PIC S9(006)V99.	   
	      05 WORK-DETLC02		PIC S9(006)V99.
	      05 WORK-DETLC03		PIC S9(006)V99.
	      05 WORK-DETLC04		PIC S9(006)V99.	   
       	   03 WORK-CODVEND-ANT          PIC 9(005) VALUE ZEROS.  
       	   03 WORK-CODVEND              PIC 9(005) VALUE ZEROS.         	   
           03 WORK-DATA. 
 	      05 WORK-DIA        	PIC 9(002) VALUE ZEROS. 
 	      05 WORK-SEP1              PIC X(001) VALUE "/".
 	      05 WORK-MES         	PIC 9(002) VALUE ZEROS. 	      
	      05 WORK-SEP1              PIC X(001) VALUE "/". 	
	      05 WORK-SEC		PIC 9(002) VALUE ZEROS.	   
 	      05 WORK-ANO        	PIC 9(002) VALUE ZEROS. 
	   03 WORK-NOMEARQ.
              05 WORK-CAMINHO         	PIC X(014) VALUE 
              "/trabalho/tca_".	
	      05 WORK-MM              	PIC 9(002) VALUE ZEROS.
 	      05 WORK-EXTENSAO        	PIC X(004) VALUE ".xls".
	   03 WORK-NOMEARQ2.
              05 FILLER                 PIC X(029) VALUE
              "/home/thiago/lista_sft006.csv".	
	   03 WORK-NOMEARQ3.
              05 FILLER                 PIC X(027) VALUE
              "/home/thiago/lista_nota.csv".
	   03 WORK-NOMEARQ4.
              05 FILLER                 PIC X(024) VALUE
**********    "/home/thiago/acom020.csv".              
              "/home/thiago/acumula.csv". 
	   03 WORK-NOMEARQ5.
              05 FILLER                 PIC X(026) VALUE
**********    "/home/thiago/acom020.csv".              
              "/home/thiago/saldo3007.csv".              
	   03 WORK-REMOVER.
              05 FILLER10         	PIC X(003) VALUE "rm ".
              05 WORK-REMOARQ          	PIC X(050) VALUE SPACES.
              05 FILLER11              	PIC X(012) VALUE
              " > /dev/null".
              05 FILLER                	PIC X(001) VALUE LOW-VALUES.
	   03 WORK-CONSULTA.
              05 WORK-PATH         	PIC X(018) VALUE 
              "/disco0/cobol/arq/".       
              05 WORK-ARQUIVO          	PIC X(010) VALUE SPACES.
                         
      *------grava log --------    
       01  WORK-SYS047.
           03 WORK-PERLOG   		PIC X(029) VALUE
              "/disco0/cobol/arq/log/learq.".
           03 WORK-MESLOG   		PIC 9(002).
	   03 WORK-DIALOG   		PIC 9(002).
	   
      *------grava log --------    
       01  WORK-REGXl3.
           03 WORK-REG3-CD   		PIC 9(001).
           03 FILLER			PIC X(001) VALUE ";".       
           03 WORK-REG3-FILIAL		PIC 9(002).
           03 FILLER			PIC X(001) VALUE ";".                  
           03 WORK-REG3-NOTA   		PIC 9(006).
           03 FILLER			PIC X(001) VALUE ";".
           03 WORK-REG3-COD   		PIC 9(010).
           03 FILLER			PIC X(001) VALUE ";".
           03 WORK-REG3-QTD   		PIC 9(005)V99.
           03 FILLER			PIC X(001) VALUE ";".           
           03 WORK-REG3-BICMSIT 	PIC ZZZ.ZZZ,ZZ.
           03 FILLER			PIC X(001) VALUE ";".           
           03 WORK-REG3-VLRSITI         PIC ZZZ.ZZZ,ZZ.
           
           
      *------Registro ACUMULADO --------    
       01  DET-ACUMULADO.           
           03 DET-CODVENDED		PIC 9(005).
           03 FILLER			PIC X(001) VALUE ";".              
	   03 DET-SLDACUM01		PIC Z.ZZZ.ZZ9,99-.
           03 FILLER			PIC X(001) VALUE ";".   	   
	   03 DET-SLDACUM02		PIC Z.ZZZ.ZZ9,99-.
           03 FILLER			PIC X(001) VALUE ";".   	   
	   03 DET-SLDACUM03		PIC Z.ZZZ.ZZ9,99-.
           03 FILLER			PIC X(001) VALUE ";".   	              
	   03 DET-SLDACUM04		PIC Z.ZZZ.ZZ9,99-.
           03 FILLER			PIC X(001) VALUE ";".   	              	   
	   03 DET-LANCAME01		PIC Z.ZZZ.ZZ9,99-.
           03 FILLER			PIC X(001) VALUE ";".   	   
	   03 DET-LANCAME02		PIC Z.ZZZ.ZZ9,99-.
           03 FILLER			PIC X(001) VALUE ";".   	   
	   03 DET-LANCAME03		PIC Z.ZZZ.ZZ9,99-.
           03 FILLER			PIC X(001) VALUE ";".   	              
	   03 DET-LANCAME04		PIC Z.ZZZ.ZZ9,99-.

      *------Registro RESERVA --------    
       01  DET-RESERVA.           
           03 DET-SEQUENC		PIC 9(005).
           03 FILLER			PIC X(001) VALUE ";".              
	   03 DET-NRPARCE		PIC 9(002).
           03 FILLER			PIC X(001) VALUE ";".   	   
	   03 DET-FILIALL		PIC 9(002).
           03 FILLER			PIC X(001) VALUE ";".   	   
	   03 DET-EQUIPES		PIC 9(005).
           03 FILLER			PIC X(001) VALUE ";".   	   
	   03 DET-CODEQUI		PIC 9(003).
           03 FILLER			PIC X(001) VALUE ";".   	   
	   03 DET-CODVEND		PIC 9(005).
           03 FILLER			PIC X(001) VALUE ";".   	   
	   03 DET-ANOEMES		PIC 9(006).
           03 FILLER			PIC X(001) VALUE ";".   	   
	   03 DET-SSAAVEN		PIC 9(004).
           03 FILLER			PIC X(001) VALUE ";".   	   
	   03 DET-MESSVEN		PIC 9(002).
           03 FILLER			PIC X(001) VALUE ";".   	   
	   03 DET-EMPRESA		PIC 9(001).
           03 FILLER			PIC X(001) VALUE ";".   	   
	   03 DET-FILIAL1		PIC 9(002).
           03 FILLER			PIC X(001) VALUE ";".   	   
	   03 DET-NRNOTAS		PIC 9(006).
           03 FILLER			PIC X(001) VALUE ";".   	   
	   03 DET-SERIENF		PIC X(002).
           03 FILLER			PIC X(001) VALUE ";".   	   
	   03 DET-MOTIVOO		PIC 9(002).
           03 FILLER			PIC X(001) VALUE ";".   	   
	   03 DET-DEBCRED		PIC X(001).
           03 FILLER			PIC X(001) VALUE ";".   	   
	   03 DET-QTDPARC		PIC 9(002).
           03 FILLER			PIC X(001) VALUE ";".   	   
           03 DET-VLRTOTA		PIC Z.ZZZ.ZZ9,99-.
           03 FILLER			PIC X(001) VALUE ";".   	   
	   03 DET-VLRPARC		PIC Z.ZZZ.ZZ9,99-.
           03 FILLER			PIC X(001) VALUE ";".   	   
	   03 DET-DTLANCA		PIC 9(008).
           03 FILLER			PIC X(001) VALUE ";".   	   
	   03 DET-QUITADO		PIC X(001).
           03 FILLER			PIC X(001) VALUE ";".   	   
	   03 DET-RESPONS		PIC X(001).
           03 FILLER			PIC X(001) VALUE ";".   	   
	   03 DET-TIPODOC		PIC X(001).
             
      *---------------------------------------------------------------*	   
       SCREEN 				SECTION.
      *---------------------------------------------------------------*       
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
          02 LINE 1 COL 1 REVERSE-VIDEO VALUE " DE ABERTURA E LEITURA DE ARQUIVOS    - DATA:         ". 
	  02 LINE 03 COL 22 VALUE "".
          02 LINE 04 COL 22 VALUE "                               ".
          02 LINE 05 COL 22 VALUE "  (1) Gera Arquivo		   ".
          02 LINE 06 COL 22 VALUE "  (2) Remove Arquivo           ".
          02 LINE 07 COL 22 VALUE "  (3) Teste                    ".         
	  02 LINE 08 COL 22 VALUE "  (4) Verifica Dia Util        ".
	  02 LINE 09 COL 22 VALUE "  (5) Altera COM021            ".
	  02 LINE 10 COL 22 VALUE "  (6) Consulta COM021  	   ".	  
	  02 LINE 11 COL 22 VALUE "  (7) Consulta COM020          ".	  
	  02 LINE 12 COL 22 VALUE "  (9) Sair <ESC>               ".                  
	  02 LINE 13 COL 22 VALUE "                               ".
          02 LINE 14 COL 22 VALUE "".
          02 COL 33 HIGHLIGHT VALUE "Digite Opcao:".
          02 COL 49 VALUE "".	 
         
       01 TELA-02.
	  02 LINE 15 COL 22 VALUE "".
          02 LINE 16 COL 22 VALUE "                               ".
	  02 LINE 17 COL 22 VALUE "                               ".
          02 LINE 18 COL 22 VALUE "                               ".
          02 LINE 19 COL 22 VALUE "                               ".
          02 LINE 20 COL 22 VALUE "                               ".          
          02 LINE 21 COL 22 VALUE "                               ".                    
	  02 LINE 22 COL 22 VALUE "".         
         
      *---------------------------------------------------------------*
       PROCEDURE 			DIVISION.
      *---------------------------------------------------------------*
       0000-PRINCIPAL    		SECTION.
	    
	    PERFORM 1000-INICIALIZA  
	    PERFORM 2000-PROCESSA 
	      	    UNTIL   WORK-OPCAO  EQUAL 9 OR ESC
	    PERFORM 3000-FINALIZA
	    .
       0000-PRINCIPAL-EXIT.
       	   EXIT.
      *---------------------------------------------------------------*
      * Inicializacao						      *
      *---------------------------------------------------------------*
       1000-INICIALIZA			SECTION.
           MOVE FUNCTION CURRENT-DATE   TO SIST-DATAHOR
	   MOVE SIST-MESSSSS  	        TO WORK-MES.
	   MOVE SIST-MESSSSS  	        TO WORK-MESLOG.
   	   MOVE SIST-DIAAAAA		TO WORK-DIA.
   	   MOVE	SIST-SECULOO		TO WORK-SEC.
	   MOVE SIST-ANOOOOO  		TO WORK-ANO.
	   MOVE WORK-MES      		TO WORK-MM.
  	   .
	1000-INICIALIZA-EXIT.
	   EXIT.
      *---------------------------------------------------------------*
      * Processa  						      *
      *---------------------------------------------------------------*
       2000-PROCESSA   			SECTION.
       
       	   INITIALIZE 			WORK-OPCAO
       
	   DISPLAY  TELA-LIMPA
	   DISPLAY  TELA-01
	   DISPLAY  WORK-DATA	        AT 0171 WITH REVERSE-VIDEO  
	   
	   ACCEPT   WORK-OPCAO          AT 1447 WITH UPPER
                                        AUTO-SKIP HIGHLIGHT.
           IF ESC
              GO TO 2000-PROCESSA-EXIT
           END-IF
                                        
	   EVALUATE WORK-OPCAO
               WHEN 01
	            PERFORM 2100-GERA-ARQUIVO
       	       WHEN 02
       	            PERFORM 2200-REMOVE-ARQUIVO
       	       WHEN 03
       	            PERFORM 2500-TESTE 
	       WHEN 04
       	            PERFORM 2600-VERIFICA-DIA-UTIL
	       WHEN 05
******************* PERFORM 2700-GERA-LOG
		    PERFORM 2999-CONSULTA-SFT072
********************		    PERFORM 2910-CONSULTA-ARQ-COM021	
	       WHEN 06
		    DISPLAY "CONSULTA ARQ RESERVA " at 2301
		    PERFORM 2900-CONSULTA-ARQ-RESERVA
	       WHEN 07
                    DISPLAY "Consulta ACUMU ANTERIOR COM021" at 2301 
******************* PERFORM 2910-CONSULTA-ARQ-COM021               
		    PERFORM 2877-CARGA-COM021
               WHEN 08
       	       	    GO   TO 2000-PROCESSA-EXIT.
	   END-EVALUTE
	   .
       2000-PROCESSA-EXIT.
           EXIT.
      *---------------------------------------------------------------*
      * Gera Arquivo XLS     					      *
      *---------------------------------------------------------------*
       2100-GERA-ARQUIVO		SECTION.
	   INITIALIZE XLS-REGISTR
      	   
	   DISPLAY TELA-02
	   DISPLAY "Gerar arquivo?"     AT 1623
	   
       	   PERFORM 2400-CONFIRMA
       
           IF LKRT024-RESPOST EQUAL "N" OR "n" OR "-"
              DISPLAY TELA-LIMPA
              GO TO  2100-GERA-ARQUIVO-EXIT.	   
	   
           OPEN INPUT   ARQXLS
           IF   WORK-FSTATUS EQUAL "00"
	        DISPLAY TELA-LIMPA
	        DISPLAY TELA-02
	        DISPLAY "ARQUIVO JA EXISTENTE" AT 1523
	        DISPLAY WORK-NOMEARQ           AT 1623
       	        CLOSE ARQXLS
	        PERFORM D00-DELAYSSS   	       
       	        GO TO  2100-GERA-ARQUIVO-EXIT
           END-IF
	   
	   CLOSE ARQXLS
		                      
           OPEN OUTPUT  ARQXLS
           IF   WORK-FSTATUS EQUAL "00"
       	        MOVE "TESTE"            TO XLS-NRCARGA
                MOVE "XX"		TO XLS-SQCARGA
	        WRITE XLS-REGISTR
	   END-IF   
	   
	   CLOSE ARQXLS      OPEN INPUT   ARQXLS
           IF   WORK-FSTATUS EQUAL "00"
	        DISPLAY TELA-LIMPA
	        DISPLAY TELA-02
	        DISPLAY "ARQUIVO JA EXISTENTE" AT 1523
	        DISPLAY WORK-NOMEARQ           AT 1623
       	        CLOSE ARQXLS
	        PERFORM D00-DELAYSSS   	       
       	        GO TO  2100-GERA-ARQUIVO-EXIT
           END-IF
	   
	   CLOSE ARQXLS
		                      
           OPEN OUTPUT  ARQXLS
           IF   WORK-FSTATUS EQUAL "00"
       	        MOVE "TESTE"            TO XLS-NRCARGA
                MOVE "XX"		TO XLS-SQCARGA
	        WRITE XLS-REGISTR
	   END-IF   
	   
	   CLOSE ARQXLS
	   
	   DISPLAY TELA-LIMPA
	   DISPLAY TELA-02
	   DISPLAY "Arq. Gerado em:"           AT 1523
	   DISPLAY WORK-NOMEARQ                AT 1623
	   DISPLAY "Enviar Arquivo Email?"     AT 1723
	   
	   PERFORM 2400-CONFIRMA
           IF LKRT024-RESPOST EQUAL "N" OR "n" OR "-"
              GO TO  2100-GERA-ARQUIVO-EXIT.	   
              
	   PERFORM 2300-ENVIA-EMAIL
	   .
       2100-GERA-ARQUIVO-EXIT.
           EXIT.           
      *---------------------------------------------------------------*
      * Remove Arquivo     					      *
      *---------------------------------------------------------------*
       2200-REMOVE-ARQUIVO		SECTION.
       	   PERFORM 2400-CONFIRMA
       
           IF LKRT024-RESPOST EQUAL "N" OR "n" OR "-"
              GO   TO 2200-REMOVE-ARQUIVO-EXIT.
       
           MOVE    WORK-NOMEARQ         TO WORK-REMOARQ.
	   CALL    "SYSTEM"             USING WORK-REMOVER.
	   
	   DISPLAY TELA-02
	   DISPLAY "Removendo Arquivo..."
	   				AT 1623
	   PERFORM D00-DELAYSSS
       	   .
       2200-REMOVE-ARQUIVO-EXIT.
           EXIT.  
      *---------------------------------------------------------------*
      * Envia Email com arquivo em anexo 			      *      
      *---------------------------------------------------------------*	
       2300-ENVIA-EMAIL       		SECTION.
           INITIALIZE LK-ROT405.
           
           MOVE      "LEARQ "           TO  LKRT405-PROGRAM.
           MOVE      "Teste de email"   TO  LKRT405-ASSUNTO.
           MOVE      "suporte@ovd.com.br" 
                                        TO  LKRT405-REMETEN.
	   MOVE      WORK-NOMEARQ       TO  LKRT405-CAMINHO.
           MOVE      ZEROS              TO  LKRT405-REGFUNC.
           MOVE      "S"                TO  LKRT405-REMOVER.
           CALL      "/disco0/cobol/obj/rot/rot405" USING LK-ROT405.
           CANCEL    "rot405".
	   .
       2300-ENVIA-EMAIL-EXIT.
           EXIT.            
      *---------------------------------------------------------------*
      * Rotina de Confirmacao  					      *
      *---------------------------------------------------------------*	
       2400-CONFIRMA			SECTION.
           MOVE        01               TO  LKRT024-MENSAGE.
           MOVE       "S"               TO  LKRT024-DEFAULT.
           CALL       "/disco0/cobol/obj/rot/rot024"
                       USING            LK-ROT024.
           CANCEL     "rot024"
           .
       2400-CONFIRMA-EXIT.
           EXIT.     
         
      *---------------------------------------------------------------*
      * Verifica se dia util 					      *
      *---------------------------------------------------------------*	
       2600-VERIFICA-DIA-UTIL		SECTION.
	   DISPLAY TELA-LIMPA
	   DISPLAY TELA-02			   
	   
	   MOVE    SIST-MESSSSS       	TO  LKRT005-MESINI.
	   MOVE    SIST-DIAAAAA       	TO  LKRT005-DIAINI.
	   MOVE    SIST-ANOOOOO       	TO  LKRT005-ANOINI.
	   MOVE    "U"                	TO  LKRT005-FUNCAO.
	   
	   CALL      "/disco0/cobol/obj/rot/rot005" USING LK-ROT005.
	   CANCEL    "rot005".  
	   
	   EVALUATE LKRT005-STATUS
	       WHEN 0
	            DISPLAY "Dia util!"		AT 1523
	   	    DISPLAY LKRT005-DESC	AT 1623
	       WHEN 1
   	   	    DISPLAY "Data Invalida"	AT 1523
	   	    DISPLAY LKRT005-DESC	AT 1623
	       WHEN 2
	   	    DISPLAY "Data Invalida"	AT 1523
	   	    DISPLAY LKRT005-DESC	AT 1623	   
	       WHEN 3
	   	    DISPLAY "Operacao Invalida!"
	   					AT 1523
	   	    DISPLAY LKRT005-DESC	AT 1623	   
	       WHEN 4
	   	    DISPLAY "Dia NAO util!"	AT 1523
	   	    DISPLAY LKRT005-DESC	AT 1623	   
	   END-EVALUATE
	   
	   PERFORM D00-DELAYSSS
           .
       2600-VERIFICA-DIA-UTIL-EXIT.
           EXIT.   
      *---------------------------------------------------------------*
      * Gerar LOG  	  					      *
      *---------------------------------------------------------------*	
       2700-GERA-LOG			SECTION.
       	   DISPLAY TELA-LIMPA
       	   DISPLAY TELA-02
       	   DISPLAY "Gerar LOG?"         AT 1623
       	   
       	   PERFORM 2400-CONFIRMA
       
           IF LKRT024-RESPOST EQUAL "N" OR "n" OR "-"
              GO   TO 2700-GERA-LOG-EXIT.       	   
       	   
       	   MOVE  "Teste de gravacao de loG"
       	   				TO YS047-DESCRIC
       	   
    	   OPEN   EXTEND  SYS047.
	   WRITE       			YS047-REGISTR
    	   INITIALIZE  			YS047-REGISTR
	   CLOSE       			SYS047.
	   
	   DISPLAY TELA-LIMPA
	   DISPLAY TELA-02
	   DISPLAY "Gerado LOG:"	AT 1623
	   DISPLAY WORK-SYS047          AT 1723
	   				
	   PERFORM D00-DELAYSSS	   
           .
       2700-GERA-LOG-EXIT.
           EXIT.  
      *---------------------------------------------------------------*
      * Consulta Arquivo   					      *
      *---------------------------------------------------------------*	
       2800-CONSULTA-ARQUIVO			SECTION.
           INITIALIZE                           LKRT024-RESPOST
       	   DISPLAY TELA-LIMPA
       	   DISPLAY TELA-02

	   
           OPEN  INPUT  SCE135
           IF WORK-FSTATUS  NOT EQUAL "00"
              DISPLAY "ERRO ABRIR SCE091"      AT 1723
           END-IF
          
           MOVE 525686                         TO CE135-NRPEDID
           MOVE	"E"			       TO CE135-SETORRR
           MOVE	zeros			       TO CE135-NRITEMM
           movE zeros			       TO CE135-SEQITEM
        
          INITIALIZE  CE135-REGISTR
        
          MOVE 525679                        TO  CE135-NRPEDID
          START  SCE135 KEY IS   >=   CE135-RECORDK INVALID KEY
          GO TO               2800-CONSULTA-ARQ-EXIT.
          
          K65-LOCACAO.
	      READ  SCE135 NEXT WITH IGNORE LOCK  AT  END
                GO TO               2800-CONSULTA-ARQ-EXIT.
                                    
          IF CE135-NRPEDID EQUAL 525686 OR 525680
	     OPEN EXTEND ARQXLS2
	     MOVE CE135-PRIORID         TO WORK-PRIORIDADE
             STRING 
		CE135-NRPEDID ";"
		CE135-SETORRR ";"
		CE135-NRITEMM ";"
		CE135-SEQITEM ";"
		CE135-CODPROD ";"
		WORK-PRIORIDADE	
                DELIMITED BY SIZE INTO XLS-REGISTR2
             END-STRING 
             
             WRITE XLS-REGISTR2
             CLOSE ARQXLS2
          END-IF
          
          GO TO K65-LOCACAO.
          
           PERFORM UNTIL LKRT024-RESPOST EQUAL 
                         "N" OR "n" OR "-" OR ESC
           
                   DISPLAY "CE135-NRPEDID:"  AT 1523
		   DISPLAY  CE135-NRPEDID    AT 1540
		   DISPLAY "CE135-SETORRR:"  AT 1623
		   DISPLAY  CE135-SETORRR    AT 1640
		   DISPLAY "CE135-NRITEMM:"  AT 1723
		   DISPLAY  CE135-NRITEMM    AT 1740	
		   DISPLAY "CE135-SEQITEM:"  AT 1823
		   DISPLAY  CE135-SEQITEM    AT 1840	
		   DISPLAY "CE135-CODPROD:"  AT 1923
		   DISPLAY  CE135-CODPROD    AT 1940
		   DISPLAY "CE135-CODPROD:"  AT 1923
		   DISPLAY  CE135-CODPROD    AT 1940		   

	           ACCEPT   CE135-NRPEDID    AT 1540 WITH UPPER
                                        AUTO-SKIP HIGHLIGHT
	           ACCEPT   CE135-SETORRR    AT 1640 WITH UPPER
                                        AUTO-SKIP HIGHLIGHT   
	           ACCEPT   CE135-NRITEMM    AT 1740 WITH UPPER
                                        AUTO-SKIP HIGHLIGHT 
	           ACCEPT   CE135-SEQITEM    AT 1840 WITH UPPER
                                        AUTO-SKIP HIGHLIGHT                                         
                   IF ESC
                      EXIT PERFORM
                   END-IF
                   
                   PERFORM 2400-CONFIRMA     
                    
                   START SCE135 KEY EQUAL CE135-RECORDK
                   READ  SCE135 NEXT WITH IGNORE LOCK
           END-PERFORM    
           
           CLOSE SCE135

           .
       2800-CONSULTA-ARQ-EXIT.
           EXIT.               
      *---------------------------------------------------------------*
      * Consulta Arquivo NOTA - Entrada                               *
      *---------------------------------------------------------------*	
       2850-CONSULTA-ARQ-NOTA			SECTION.
          INITIALIZE                            CP002-RECORDK
       	  DISPLAY TELA-LIMPA
          DISPLAY TELA-02
          
          OPEN EXTEND ARQXLS3

          OPEN  INPUT  SFT073
          IF WORK-FSTATUS  NOT EQUAL "00"
             DISPLAY "ERRO ABRIR SCP002"        AT 1723
          END-IF          
          
          MOVE 2               To FT073-EMPRESA
          MOVE ZEROS 	       TO FT073-FILIALL
          MOVE ZEROS           TO FT073-NRNOTAS
          

          START  SFT073 KEY IS   >=   FT073-RECORDK INVALID KEY
          GO TO               2850-CONSULTA-ARQ-NOTA-EXIT.
          
          K65-LESFT023.
	      READ  SFT073 NEXT WITH IGNORE LOCK  AT  END
	        CLOSE   SFT073 ARQXLS3 
                GO  TO  2850-CONSULTA-ARQ-NOTA-EXIT.
                INITIALIZE XLS-REGISTR3
                
		IF FT073-EMPRESA NOT EQUAL 2
                   CLOSE   SFT073 ARQXLS3 
	           GO  TO  2850-CONSULTA-ARQ-NOTA-EXIT		
		END-IF               
               
               
                IF FT073-AGRICUL   EQUAL "A" 
                   MOVE FT073-EMPRESA      TO WORK-REG3-CD
                   MOVE FT073-FILIALL	   TO WORK-REG3-FILIAL
                   MOVE FT073-NRNOTAS      TO WORK-REG3-NOTA
                   MOVE FT073-CODPROD	TO WORK-REG3-COD
                   MOVE FT073-QUANTID	TO WORK-REG3-QTD
                   MOVE FT073-BICMSIT	TO WORK-REG3-BICMSIT                
                   MOVE FT073-VICMSIT	TO WORK-REG3-VLRSITI                                  
                   MOVE WORK-REGXl3        TO XLS-REGISTR3

		  WRITE XLS-REGISTR3
                END-IF
          GO TO K65-LESFT023.
          
          CLOSE SFT073
           .
       2850-CONSULTA-ARQ-NOTA-EXIT.
           EXIT.     
           
      *---------------------------------------------------------------*
      * CAAARGA	 			                              *
      *---------------------------------------------------------------*	
       2877-CARGA-COM021			SECTION.
          INITIALIZE                            OM021-RECORDK
          
       	  DISPLAY TELA-LIMPA
          DISPLAY TELA-02
          
********  OPEN OUTPUT COM021
********* CLOSE COM021
          
          OPEN I-O COM021
          .
********  OPEN INPUT  CARGA
********  IF WORK-FSTATUS  NOT EQUAL "00"
********     DISPLAY "ERRO ABRIR CARGA"        AT 1723
********  END-IF .         
         
          K65-LECARGA.
************** READ CARGA NEXT WITH IGNORE LOCK  AT  END
               READ COM021 NEXT WITH IGNORE LOCK  AT  END
            		CLOSE   COM021 
************CLOSE   COM021 CARGA
	                GO  TO  2877-CARGA-COM021-EXIT.
************************MOVE	XLS-CODVEND	TO OM021-CODVEND
************************MOVE	XLS-FILIAL	TO OM021-FILIALL
************************MOVE	XLS-SLDACUM	TO OM021-SLDACUM01
************************MOVE	180,00 		TO OM021-SLDRESE01
************************MOVE	ZEROS		TO OM021-SLDACUM02
************************MOVE	ZEROS		TO OM021-SLDRESE02
************************MOVE	ZEROS		TO OM021-SLDACUM03
************************MOVE	ZEROS		TO OM021-SLDRESE03
************************MOVE	XLS-SLDACUM    	TO OM021-SLDACUM04
************************MOVE	ZEROS 		TO OM021-SLDRESE04
************************WRITE OM021-REGISTR 
		MOVE OM021-SLDACUM01            TO OM021-SLDACUM04
		REWRITE OM021-REGISTR
                
          GO TO K65-LECARGA.
          .
       2877-CARGA-COM021-EXIT.
           EXIT.  
           
      *---------------------------------------------------------------*
      * Gera arq itens			                              *
      *---------------------------------------------------------------*	
       2888-GERA-ARQ-RESERVA			SECTION.
          INITIALIZE                            OM020-RECORD2
          
          MOVE "SIM"                            TO WORK-FLAG-PRIMEIRA 
       	  DISPLAY TELA-LIMPA
          DISPLAY TELA-02
          
          OPEN OUTPUT ARQXLS4
          
          OPEN INPUT  COM020
          IF WORK-FSTATUS  NOT EQUAL "00"
             DISPLAY "ERRO ABRIR COM020"        AT 1723
          END-IF          
          
          START  COM020 KEY IS   >=   OM020-RECORD2 INVALID KEY
          GO TO               2888-GERA-ARQ-RESERVA-EXIT.
          
          K65-LECOM020.
	      READ  COM020 NEXT WITH IGNORE LOCK  AT  END
	        CLOSE   COM020 ARQXLS4 
                GO  TO  2888-GERA-ARQ-RESERVA-EXIT.
                
                IF WORK-FLAG-PRIMEIRA EQUAL "SIM"
	           MOVE OM020-CODVEND      TO WORK-CODVEND-ANT
	           MOVE "NAO"              TO WORK-FLAG-PRIMEIRA 
	        END-IF   
	        
		IF OM020-CODVEND NOT EQUAL  WORK-CODVEND-ANT
                   MOVE  WORK-CODVEND-ANT       TO DET-CODVENDED
		   WRITE XLS-REGISTR4 	   FROM DET-ACUMULADO
		   MOVE  OM020-CODVEND      TO WORK-CODVEND-ANT
		   INITIALIZE DET-ACUMULADO
		END-IF	        
	        
          GO TO K65-LECOM020.
          
          CLOSE COM020                 
           .
       2888-GERA-ARQ-RESERVA-EXIT.
           EXIT.  
      *---------------------------------------------------------------*
      * Consulta arq reserva por cod vend                             *
      *---------------------------------------------------------------*	
       2900-CONSULTA-ARQ-RESERVA		SECTION.
          INITIALIZE                            OM020-RECORD2
       	  DISPLAY TELA-LIMPA
          DISPLAY TELA-02
          
          OPEN I-O  COM020
          IF WORK-FSTATUS  NOT EQUAL "00"
             DISPLAY "ERRO ABRIR COM020"        AT 1723
          END-IF          
          
  	  ACCEPT WORK-CODVEND   	   	AT 1540 WITH UPPER
                                    	        AUTO-SKIP HIGHLIGHT  
                                    	        
          MOVE WORK-CODVEND 	                TO OM020-CODVEND
                                    	        
          IF ESC
             MOVE ZEROS                 TO WORK-TIPTECL
             MOVE 01                    TO WORK-CDTECL1          
             GO TO 2900-CONSULTA-ARQ-RESERVA-EXIT	
          END-IF
                                    	        
          START  COM020 KEY IS   >=   OM020-RECORD2 INVALID KEY
          GO TO               2900-CONSULTA-ARQ-RESERVA-EXIT.
          
          K65-LECOM020.
	      READ  COM020 NEXT WITH IGNORE LOCK  AT  END
	        CLOSE   COM020
                GO  TO  2900-CONSULTA-ARQ-RESERVA-EXIT.
                
               	   IF OM020-CODVEND NOT EQUAL WORK-CODVEND
               	      CLOSE COM020
               	      GO TO 2900-CONSULTA-ARQ-RESERVA
               	   END-IF                
                
                   DISPLAY "OM020-CODVEND:"  AT 1523
		   DISPLAY  OM020-CODVEND    AT 1540
		   DISPLAY "OM020-DEBCRED:"  AT 1623
		   DISPLAY  OM020-DEBCRED    AT 1640
		   DISPLAY "OM020-VLRTOTA:"  AT 1723
		   DISPLAY  OM020-VLRTOTA    AT 1740		   
		   DISPLAY "OM020-VLRPARC:"  AT 1823
		   DISPLAY  OM020-VLRPARC    AT 1840	
		   DISPLAY "OM020-DTLANCA:"  AT 1923
		   DISPLAY  OM020-DTLANCA    AT 1940
		   
		   DISPLAY "OM020-SLDACUM:"  AT 2023
                   DISPLAY  OM020-SLDACUM    AT 2040
		   
		   DISPLAY "OM020-SLDTRIM:"  AT 2123
	           DISPLAY  OM020-SLDTRIM    AT 2140
	           
	           ACCEPT   OM020-SLDACUM    AT 2040
		   ACCEPT   OM020-SLDTRIM    AT 2140		   
		   
		   REWRITE OM020-REGISTR            
		   MOVE OM020-SLDTRIM	     TO WORK-SLDANT
		   DISPLAY "Saldo Anterior: " AT 1550
		   DISPLAY WORK-SLDANT        AT 1566
          GO TO K65-LECOM020.
          
          CLOSE COM020
           .
       2900-CONSULTA-ARQ-RESERVA-EXIT.
           EXIT.    
           
      *---------------------------------------------------------------*
      * INCLUI arq COM021		                              *
      *---------------------------------------------------------------*	
       2905-INCLUI-ARQ-COM021		SECTION.
          INITIALIZE                            OM021-RECORDK
          
          OPEN I-O  COM021
          IF WORK-FSTATUS  NOT EQUAL "00"
             DISPLAY "ERRO ABRIR COM021"        AT 1723
          END-IF.
          
          2905-LECOM021.
          
       	  DISPLAY TELA-LIMPA
          DISPLAY TELA-02          
          
  	  ACCEPT WORK-CODVEND   	   	AT 1540 WITH UPPER
                                    	        AUTO-SKIP HIGHLIGHT  
                                    	        
          MOVE WORK-CODVEND 	                TO OM021-CODVEND
                                   	        
          IF ESC
             CLOSE COM021
          
             MOVE ZEROS                 TO WORK-TIPTECL
             MOVE 01                    TO WORK-CDTECL1
             GO TO 2905-INCLUI-ARQ-COM021-EXIT	
          END-IF
                                    	        
	      READ  COM021 WITH IGNORE LOCK 
	      
	      IF WORK-FSTATUS EQUAL 23
                
                   DISPLAY "OM021-CODVEND:"  AT 1523
		   DISPLAY  OM021-CODVEND    AT 1540
		   DISPLAY "OM021-SLDACUM01:"  AT 1723
		   DISPLAY  OM021-SLDACUM01    AT 1740		   
		   DISPLAY "OM021-SLDRESE01:"  AT 1823
		   DISPLAY  OM021-SLDRESE01  AT 1840
		   
		   DISPLAY "OM021-FILIALL:"  AT 1923
		   DISPLAY  OM021-FILIALL    AT 1940
		   
		   ACCEPT OM021-SLDACUM01    AT 1740
		   ACCEPT OM021-SLDRESE01    AT 1840
		   
		   WRITE OM021-REGISTR
		   
	      END-IF 	   
		   
        	   STOP " ENTER"
		   
          GO TO 2905-LECOM021.
          
          CLOSE COM021
           .
       2905-INCLUI-ARQ-COM021-EXIT.
           EXIT.                        
           
           
      *---------------------------------------------------------------*
      * Consulta arq COM021		                              *
      *---------------------------------------------------------------*	
       2910-CONSULTA-ARQ-COM021		SECTION.
          INITIALIZE                            OM021-RECORDK
          
          OPEN I-O  COM021
          IF WORK-FSTATUS  NOT EQUAL "00"
             DISPLAY "ERRO ABRIR COM020"        AT 1723
          END-IF.
          
          2910-LECOM021.
          
       	  DISPLAY TELA-LIMPA
          DISPLAY TELA-02          
          
  	  ACCEPT WORK-CODVEND   	   	AT 1540 WITH UPPER
                                    	        AUTO-SKIP HIGHLIGHT  
                                    	        
          MOVE WORK-CODVEND 	                TO OM021-CODVEND
                                    	        
          IF ESC
             CLOSE COM021
             MOVE ZEROS                 TO WORK-TIPTECL
             MOVE 01                    TO WORK-CDTECL1
             GO TO 2910-CONSULTA-ARQ-COM021-EXIT	
          END-IF
                                    	        
	      READ  COM021 WITH IGNORE LOCK 
	      
	      IF WORK-FSTATUS EQUAL ZEROS
            	   IF OM021-CODVEND NOT EQUAL WORK-CODVEND
               	      CLOSE COM021
               	      GO TO 2910-CONSULTA-ARQ-COM021-EXIT
               	   END-IF                
                
                   DISPLAY "OM021-CODVEND:"  AT 1523
		   DISPLAY  OM021-CODVEND    AT 1540
		   DISPLAY "OM021-SLDACUM01:"  AT 1723
		   DISPLAY  OM021-SLDACUM01    AT 1740		   
		   DISPLAY "OM021-SLDRESE01:"  AT 1823
		   DISPLAY  OM021-SLDRESE01  AT 1840
		   
		   DISPLAY "OM021-FILIALL:"  AT 1923
		   DISPLAY  OM021-FILIALL    AT 1940
		   
		   MOVE OM021-SLDACUM01	     to WORK-SLDACUM 	  
		   ACCEPT WORK-SLDACUM       AT 1740	
		   IF F9
		      DELETE COM021
		   END-IF
		   
	      END-IF 	   
		   
        	   STOP " ENTER"
		   
          GO TO 2910-LECOM021.
          
          CLOSE COM021
           .
       2910-CONSULTA-ARQ-COM021-EXIT.
           EXIT.             
           
      *---------------------------------------------------------------*
      * Consulta arq COM021		                              *
      *---------------------------------------------------------------*	
       2915-CONSULTA-ARQ-COM021			SECTION.
          INITIALIZE                            OM021-RECORDK
          
       	  DISPLAY TELA-LIMPA
          DISPLAY TELA-02
          
          OPEN I-O  COM021
          IF WORK-FSTATUS  NOT EQUAL "00"
             DISPLAY "ERRO ABRIR COM021"        AT 1723
          END-IF          
          
  	  ACCEPT WORK-CODVEND   	   	AT 1540 WITH UPPER
                                    	        AUTO-SKIP HIGHLIGHT  
          MOVE WORK-CODVEND 	                TO OM021-CODVEND

          IF ESC
             CLOSE COM021
             MOVE ZEROS                 	TO WORK-TIPTECL
             MOVE 01                    	TO WORK-CDTECL1             
             GO TO 2915-CONSULTA-ARQ-COM021-EXIT	
          END-IF.
	     .
          K65-LECOM021.
	      READ  COM021 NEXT WITH IGNORE LOCK  AT  END
	        CLOSE   COM021
                GO  TO  2915-CONSULTA-ARQ-COM021-EXIT.
                
                   DISPLAY "OM021-CODVEND:"     AT 1523
		   DISPLAY  OM021-CODVEND       AT 1540
		   
		   DISPLAY "OM021-SLDACUM01:"   AT 1723
		   DISPLAY  OM021-SLDACUM01     AT 1740	
		   
		   DISPLAY "OM021-SLDRESE01:"   AT 1823
		   DISPLAY  OM021-SLDRESE01     AT 1840
		   
		   DISPLAY "OM021-SLDACUM04:"   AT 1923
		   DISPLAY  OM021-SLDACUM04     AT 1940	
		   
		   DISPLAY "OM021-SLDRESE04:"   AT 2023
		   DISPLAY  OM021-SLDRESE04     AT 2040		   
		   
		   ACCEPT OM021-SLDACUM04       AT 1940
		   ACCEPT OM021-SLDRESE04       AT 2040		   
		   REWRITE OM021-REGISTR
		   
		   STOP "    <ENTER>"

          GO TO K65-LECOM021.
          
          CLOSE COM021
           .
       2915-CONSULTA-ARQ-COM021-EXIT.
           EXIT.  

      *---------------------------------------------------------------*
      * Gera arq de itens   					      *
      *---------------------------------------------------------------*	
       2950-GERA-ARQ-ITENS			SECTION.
          INITIALIZE                            CE001-RECORDK
          
       	  DISPLAY TELA-LIMPA
          DISPLAY TELA-02
          
          OPEN OUTPUT ARQXLS2
          
          OPEN INPUT  SCE001
          IF WORK-FSTATUS  NOT EQUAL "00"
             DISPLAY "ERRO ABRIR SCE001"        AT 1723
          END-IF          
                                     	        
          START  SCE001 KEY IS   >=   CE001-RECORDK INVALID KEY
          GO TO               2950-GERA-ARQ-ITENS-EXIT.
          
          K65-LESCE001.
	      READ  SCE001 NEXT WITH IGNORE LOCK  AT  END
	        CLOSE   SCE001 ARQXLS2
                GO  TO  2950-GERA-ARQ-ITENS-EXIT.
                
               	   IF CE001-NAO-COM EQUAL 1
               	      MOVE CE001-CODPROD        TO XLS-REGISTR2
               	      WRITE XLS-REGISTR2
               	   END-IF                
              
          GO TO K65-LESCE001.
          
           .
       2950-GERA-ARQ-ITENS-EXIT.
           EXIT.     
      *---------------------------------------------------------------*
      * Gera arq SFT006   					      *
      *---------------------------------------------------------------*	
       2977-GERA-ARQ-SFT006			SECTION.
          INITIALIZE                            FT006-RECORDK
          
       	  DISPLAY TELA-LIMPA
          DISPLAY TELA-02
          
          OPEN OUTPUT ARQXLS2
          
          OPEN INPUT  SFT006
          IF WORK-FSTATUS  NOT EQUAL "00"
             DISPLAY "ERRO ABRIR SFT006"        AT 1723
          END-IF          
                                     	        
          START  SFT006 KEY IS   >=   FT006-RECORDK INVALID KEY
          GO TO               2977-GERA-ARQ-SFT006-EXIT.
          
          K65-LESFT006.
	      READ  SFT006 NEXT WITH IGNORE LOCK  AT  END
	        CLOSE   SFT006 ARQXLS2
                GO  TO  2977-GERA-ARQ-SFT006-EXIT.
                
               	   IF (FT006-INSCEST EQUAL "ISENTO" OR SPACES)               	      
		       MOVE FT006-CODVEND        TO WORK-CODVEND

             	       STRING 
				FT006-EMPRESA ";"
				FT006-FILIALL ";"
				WORK-CODVEND  ";"
				FT006-NOMECLI ";"
				FT006-CGCECPF ";"
				FT006-INSCEST ";"
				FT006-RAMOATI 
             	       DELIMITED BY SIZE INTO XLS-REGISTR2
               	       WRITE XLS-REGISTR2
		       DISPLAY XLS-REGISTR2   AT 2020               	       
               	   END-IF                
              
          GO TO K65-LESFT006.
          
           .
       2977-GERA-ARQ-SFT006-EXIT.
           EXIT.              
           
      *---------------------------------------------------------------*
      * CONSULTA SFT072
      *---------------------------------------------------------------*	
       2999-CONSULTA-SFT072			SECTION.
          INITIALIZE                            FT072-RECORDK
          
       	  DISPLAY TELA-LIMPA
          DISPLAY TELA-02
          
          OPEN INPUT  SFT072
          IF WORK-FSTATUS  NOT EQUAL "00"
             DISPLAY "ERRO ABRIR SFT072"        AT 1723
          END-IF          
                                     	        
          START  SFT072 KEY IS   >=   FT072-RECORDK INVALID KEY
          GO TO               2999-CONSULTA-SFT072-EXIT.
          
          K65-LESFT072.
	      READ  SFT072 NEXT WITH IGNORE LOCK  AT  END
	        CLOSE   SFT072
	        GO TO   2999-CONSULTA-SFT072-EXIT. 
	        
	        IF (FT072-CGCECPF EQUAL 50060 OR 
	           50001 OR 50002 OR 50003 OR
	           50004 OR 50005 OR 50006) AND 
	           FT072-SERIENF NOT EQUAL "DO"
	           DISPLAY "FT072-CGCECPF: "	AT 1730
		   DISPLAY FT072-CGCECPF	AT 1745
		
		   DISPLAY "FT072-EMPRESA: "	AT 1830	
	           DISPLAY FT072-EMPRESA	AT 1845
	        
	           DISPLAY "FT072-FILIALL: "	AT 1930
		   DISPLAY FT072-FILIALL	AT 1945

	           DISPLAY "FT072-NRNOTAS: "	AT 2030
		   DISPLAY FT072-NRNOTAS	AT 2045

	           DISPLAY "FT072-SERIENF: "	AT 2130
		   DISPLAY FT072-SERIENF	AT 2145		   
		   
		   STOP " <ENTER> "
		END-IF   
             
          GO TO K65-LESFT072.
          
           .
       2999-CONSULTA-SFT072-EXIT.
           EXIT.             
          
      *---------------------------------------------------------------*
      * Rotina de Delay  (TEMPO)				      *
      *---------------------------------------------------------------*	      
           COPY     "/desenv/cobol/rotinas/rt-delayss".           
      *---------------------------------------------------------------*
      * Finalizacao						      *
      *---------------------------------------------------------------*
       3000-FINALIZA			SECTION.
           EXIT      PROGRAM
       	   STOP      RUN
       	   .
       3000-FINALIZA-EXIT.
           EXIT.
      *----------------------------FIM--------------------------------*