       IDENTIFICATION DIVISION.
       PROGRAM-ID. CARTEIRA-CLIENTES.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
        
       DATA DIVISION.    
       WORKING-STORAGE SECTION.
       01 WS-OPCAO                     PIC X           VALUE "S".
       SCREEN SECTION.
       01  MENU-PRINCIPAL.
           05  MENU-PRINCIPAL-CABECALHO.
               10  VALUE "GERENCIAMENTO DE CARTELA DE CLIENTES - AMBEV" 
                                       BLANK SCREEN    LINE 1 COL 35.
           05  MENU-PRINCIPAL-OPCOES.
               10  VALUE "01.00 - CADASTROS"           LINE 5 COL 10.
               10  VALUE "01.01 - CADASTRO DE CLIENTE" LINE 6 COL 10.
               10  VALUE "01.02 - CADASTRO DE VENDEDOR"  
                                                       LINE 7 COL 10.
               10  VALUE "02.00 - RELATORIOS"          LINE 9 COL 10.
               10  VALUE "02.01 - RELATORIO DE CLIENTES" 
                                                       LINE 10 COL 10.
               10  VALUE "02.02 - RELATORIO DE VENDEDORES"
                                                       LINE 11 COL 10.
               10  VALUE "03.00 - EXECUTAR"            LINE 13 COL 10.
               10  VALUE "03.01 - EXECUTAR DISTRIBUICAO DE CLIENTES"   
                                                       LINE 14 COL 10.
               10  VALUE "S - SAIR"                    LINE 16 COL 10.
           05 ESCOLHA-MENU-PRINCIPAL.
               10  VALUE "DIGITE A OPCAO DESEJADA"     LINE 20 COL 10.
               10  RESPONSE-INPUT                      LINE 20 COL 35
                                       PIC X           TO WS-OPCAO.
                                       
       PROCEDURE DIVISION.
      ******************************************************************
      *                                                                *
      ******************************************************************
       0000-PRINCIPAL.
           PERFORM 1000-MOSTAR-MENU-PRINCIPAL  UNTIL WS-OPCAO = "S".
       0000-PRINCIPAL-FIM.
      ******************************************************************
      *                                                                *
      ******************************************************************
       1000-MOSTAR-MENU-PRINCIPAL.
           DISPLAY MENU-PRINCIPAL.
           ACCEPT MENU-PRINCIPAL.
       1000-MOSTRAR-MENU-PRINCIPAL-FIM.    
           
       END PROGRAM.
