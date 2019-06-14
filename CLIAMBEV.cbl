      ******************************************************************
       IDENTIFICATION                  DIVISION.
      ******************************************************************
      *
       PROGRAM-ID. CARTEIRA-CLIENTES.
      *
      ******************************************************************
       ENVIRONMENT                     DIVISION.
      ******************************************************************
      *
      *----------------------------------------------------------------*
       CONFIGURATION                    SECTION.
      *----------------------------------------------------------------*
      *
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*
      *
       FILE-CONTROL.
           SELECT ARQ-CLIENTES         ASSIGN TO  "WID-ARQ-CLIENTES.DAT"
               ORGANIZATION            IS INDEXED
               ACCESS MODE             IS DYNAMIC
               RECORD KEY              IS CLI-CNPJ
               LOCK MODE               IS MANUAL
               FILE STATUS             IS WS-FILE-STATUS.
      *  
      ******************************************************************
       DATA                            DIVISION.
      ******************************************************************
      *
      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*
      *
       FD ARQ-CLIENTES.
       01 REG-CLIENTE.
           05 CLI-CODIGO               PIC 9(07).
           05 CLI-CNPJ                 PIC 9(14).
           05 CLI-RAZAO-SOCIAL         PIC X(40).
           05 CLI-LATITUDE             PIC S9(03)V9(08).
           05 CLI-LONGITUDE            PIC S9(03)V9(08).
      *     
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
       77 WS-OPCAO                     PIC X(5).
       77 WS-OPCAO-CLI                 PIC X.
       77 WS-OPCAO-INC-CLI             PIC X.
       01 WS-STATUS                    PIC X(30). 
       77 WS-FILE-STATUS               PIC X(02).
           88 FS-OK                    VALUE "00" THRU "09".
           88 FS-NAO-ENCONTRADO        VALUE "23".
           88 FS-ERRO-LAYOUT           VALUE "39".
           88 FS-CANCELA               VALUE "99".
      *     
      *----------------------------------------------------------------*
       SCREEN                          SECTION.
      *----------------------------------------------------------------*
      *
       01  SCR-MENU-PRINCIPAL.
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
               10  OPCAO-PRINC                         LINE 20 COL 35
                                       PIC X(5)        TO WS-OPCAO.
      *                                 
       01 SCR-MENU-CLIENTES.
           05  MENU-CLIENTES-CABECALHO.
               10  VALUE "CADASTRO DE CLIENTES - AMBEV"
                                       BLANK SCREEN    LINE 1 COL 35.
           05  MENU-CLIENTES-OPCOES.
               10  VALUE "1 - INCLUSAO"                LINE 5 COL 10.
               10  VALUE "2 - ALTERACAO"               LINE 6 COL 10.
               10  VALUE "3 - EXCLUSAO"                LINE 7 COL 10.
               10  VALUE "4 - IMPORTACAO"              LINE 8 COL 10.
               10  VALUE "M - VOLTAR PARA O MENU PRINCIPAL"           
                                                       LINE 16 COL 10. 
           05 ESCOLHA-MENU-CLIENTES.
               10  VALUE "DIGITE A OPCAO DESEJADA"     LINE 20 COL 10.
               10  OPCAO-CLI                           LINE 20 COL 35
                                       PIC X           TO WS-OPCAO-CLI.
      *                                 
       01 OPCAO-STATUS                 HIGHLIGHT.
           05 STATUS-OUTPUT                            LINE 25 COL 10
                                       PIC X(30)       FROM WS-STATUS.
      * 
       01  SCR-INCLUIR-CLIENTE.
           05  INCLUIR-CLIENTE-CABECALHO.
               10  VALUE "CADASTRO DE CLIENTE - AMBEV"           
                                       BLANK SCREEN    LINE 1 COL 50.
           05  INCLUIR-CLIENTE-DADOS.
               10  VALUE "CODIGO: "                    LINE 5 COL 10.
               10  SCR-COD-CLI                         LINE 5 COL 35
                                       PIC 9(7)  USING CLI-CODIGO.      
               10  VALUE "CNPJ:"                       LINE 6 COL 10.
               10  SCR-CNPJ-CLI                        LINE 6 COL 35
                                       PIC 9(14) USING CLI-CNPJ.
               10  VALUE "RAZAO SOCIAL:"                LINE 7 COL 10.
               10  SCR-RAZAO-SOCIAL-CLI                LINE 7 COL 35
                                       PIC X(40) USING CLI-RAZAO-SOCIAL.
               10  VALUE "LATITUDE:"                   LINE 8 COL 10.
               10  SCR-LATITUDE-CLI                    LINE 8 COL 35
                                       PIC S9(3)V9(8)
                                                 USING CLI-LATITUDE.
               10  VALUE "LONGITUDE"                   LINE 9 COL 10.
               10  SCR-LONGITUDE-CLI                   LINE 9 COL 35    
                                       PIC S9(3)V9(8)  
                                                 USING CLI-LONGITUDE.
               10  VALUE "S - SALVAR"                  LINE 16 COL 10.
               10  VALUE "V - VOLTAR"                  LINE 17 COL 10. 
           05 ESCOLHA-INCLUIR-CLIENTE.
               10  VALUE "DIGITE A OPCAO DESEJADA"     LINE 20 COL 10.
               10  OPCAO-INC-CLI                       LINE 20 COL 35
                                       PIC X       TO WS-OPCAO-INC-CLI.
      *
       01  SCR-ALTERAR-CLIENTE.
           05 ALTERAR-CLIENTE-CABECALHO.
               10  VALUE "CADASTRO DE CLIENTE - AMBEV"           
                                       BLANK SCREEN    LINE 1 COL 50.
           05  CNPJ-BUSCA-CLIENTE.
               10  VALUE "DIGITE O CNPJ DO CLIENTE"    LINE 5 COL 10.
               10  SCR-CNPJ-ALT-CLI                    LINE 5 COL 35
                                       PIC 9(14)       TO CLI-CNPJ.
      *      
      ******************************************************************
       PROCEDURE                       DIVISION.
      ******************************************************************
      *                                                                *
      *----------------------------------------------------------------*
       PERFORM 00000-MENU-PRINCIPAL.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       00000-MENU-PRINCIPAL            SECTION.
      *----------------------------------------------------------------*
           PERFORM UNTIL 1 <> 1
               DISPLAY SCR-MENU-PRINCIPAL
               DISPLAY OPCAO-STATUS
               ACCEPT OPCAO-PRINC
               EVALUATE WS-OPCAO
                   WHEN "01.01"
                       PERFORM 10000-MENU-CADASTRO-CLIENTE
                   WHEN "s"
                   WHEN "S"
                       GOBACK
                   WHEN OTHER
                       MOVE "OPCAO INVALIDA"   
                                       TO WS-STATUS
               END-EVALUATE
           END-PERFORM
           .
       00000-FIM. EXIT.    
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       10000-MENU-CADASTRO-CLIENTE     SECTION.
      *----------------------------------------------------------------*
           PERFORM UNTIL 1 <> 1
               DISPLAY SCR-MENU-CLIENTES
               DISPLAY OPCAO-STATUS
               ACCEPT OPCAO-CLI
               EVALUATE WS-OPCAO-CLI
                   WHEN "1"
                       PERFORM 11000-INCLUIR-CLIENTE 
                   WHEN "2"
                       PERFORM 12000-ALTERAR-CLIENTE
                   WHEN "3"
                       PERFORM 13000-EXCLUIR-CLIENTE
                   WHEN "m"
                   WHEN "M"
                       PERFORM 00000-MENU-PRINCIPAL
                   WHEN OTHER
                       MOVE "OPCAO INVALIDA"   
                                       TO WS-STATUS
               END-EVALUATE
           END-PERFORM
           .
       10000-FIM. EXIT.    
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       11000-INCLUIR-CLIENTE           SECTION.
      *----------------------------------------------------------------*
           INITIALIZE REG-CLIENTE      REPLACING NUMERIC BY ZEROS
                                       ALPHANUMERIC BY SPACES
           DISPLAY SCR-INCLUIR-CLIENTE
           DISPLAY OPCAO-STATUS
           ACCEPT SCR-COD-CLI
           ACCEPT SCR-CNPJ-CLI
           ACCEPT SCR-RAZAO-SOCIAL-CLI
           ACCEPT SCR-LATITUDE-CLI
           ACCEPT SCR-LONGITUDE-CLI
           ACCEPT OPCAO-INC-CLI
           EVALUATE WS-OPCAO-INC-CLI
               WHEN "S"
                   PERFORM 11100-ABRIR-ARQUIVO-CLIENTES
                   PERFORM 11200-LER-ARQUIVO-CLIENTES
                   IF FS-NAO-ENCONTRADO
                       PERFORM 11300-GRAVAR-ARQUIVO-CLIENTES
                   ELSE
                       MOVE "CNPJ JA CADASTRADO"       
                                       TO WS-STATUS
                   END-IF
                   PERFORM 11400-FECHAR-ARQUIVO-CLIENTES
                   PERFORM 10000-MENU-CADASTRO-CLIENTE
               WHEN "V"
                   PERFORM 10000-MENU-CADASTRO-CLIENTE
               WHEN OTHER
                   MOVE "OPCAO INVALIDA"   
                                       TO WS-STATUS
           END-EVALUATE
           .
       11000-FIM. EXIT.    
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       11100-ABRIR-ARQUIVO-CLIENTES     SECTION.
      *----------------------------------------------------------------*
           OPEN I-O ARQ-CLIENTES
           IF NOT FS-OK
               PERFORM 99991-ERRO-ARQUIVO-CLIENTES
           END-IF
           .
       11100-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       11200-LER-ARQUIVO-CLIENTES      SECTION.
      *----------------------------------------------------------------*
           READ ARQ-CLIENTES           INTO REG-CLIENTE
           IF NOT FS-OK AND NOT FS-NAO-ENCONTRADO
               PERFORM 99991-ERRO-ARQUIVO-CLIENTES
           END-IF
           .
       11200-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       11300-GRAVAR-ARQUIVO-CLIENTES   SECTION.
      *----------------------------------------------------------------*
           WRITE REG-CLIENTE
           IF NOT FS-OK
               PERFORM 99991-ERRO-ARQUIVO-CLIENTES
           ELSE
               MOVE "CLIENTE CADASTRADO COM SUCESSO"
                                       TO WS-STATUS
           END-IF
           .
       11300-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       11400-FECHAR-ARQUIVO-CLIENTES   SECTION.                         
      *----------------------------------------------------------------*
           CLOSE ARQ-CLIENTES
           IF NOT FS-OK
               PERFORM 99991-ERRO-ARQUIVO-CLIENTES
           END-IF
           .
       11400-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       12000-ALTERAR-CLIENTE            SECTION.
      *----------------------------------------------------------------*
           DISPLAY OPCAO-STATUS
           PERFORM 11100-ABRIR-ARQUIVO-CLIENTES
           DISPLAY SCR-ALTERAR-CLIENTE
           ACCEPT SCR-CNPJ-ALT-CLI
           PERFORM 11200-LER-ARQUIVO-CLIENTES
           IF FS-NAO-ENCONTRADO
               MOVE "CLIENTE NAO ENCONTRADO"
                                       TO WS-STATUS
           ELSE
               DISPLAY SCR-INCLUIR-CLIENTE
               ACCEPT SCR-COD-CLI
               ACCEPT SCR-RAZAO-SOCIAL-CLI
               ACCEPT SCR-LATITUDE-CLI
               ACCEPT SCR-LONGITUDE-CLI
               ACCEPT OPCAO-INC-CLI
               EVALUATE WS-OPCAO-INC-CLI
                   WHEN "S"
                       PERFORM 12100-ALTERAR-ARQUIVO-CLIENTES
                       PERFORM 11400-FECHAR-ARQUIVO-CLIENTES
                       PERFORM 10000-MENU-CADASTRO-CLIENTE
                   WHEN "V"
                       PERFORM 10000-MENU-CADASTRO-CLIENTE
                   WHEN OTHER
                       MOVE "OPCAO INVALIDA"   
                                           TO WS-STATUS
               END-EVALUATE
           END-IF

           
           .
       12000-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       12100-ALTERAR-ARQUIVO-CLIENTES  SECTION.                        
      *----------------------------------------------------------------*
           REWRITE REG-CLIENTE
           IF NOT FS-OK
               PERFORM 99991-ERRO-ARQUIVO-CLIENTES
           ELSE
               MOVE "CLIENTE ALTERADO COM SUCESSO"
                                       TO WS-STATUS
           END-IF
           .
       12100-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       13000-EXCLUIR-CLIENTE            SECTION.
      *----------------------------------------------------------------*
           PERFORM 11100-ABRIR-ARQUIVO-CLIENTES
           DISPLAY SCR-ALTERAR-CLIENTE
           DISPLAY OPCAO-STATUS
           ACCEPT SCR-CNPJ-ALT-CLI
           PERFORM 11200-LER-ARQUIVO-CLIENTES
           IF FS-NAO-ENCONTRADO
               MOVE "CLIENTE NAO ENCONTRADO"
                                       TO WS-STATUS
           ELSE
               PERFORM 13100-EXCLUIR-ARQUIVO-CLIENTES
               PERFORM 11400-FECHAR-ARQUIVO-CLIENTES
               PERFORM 10000-MENU-CADASTRO-CLIENTE
           END-IF
           .
       13000-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       13100-EXCLUIR-ARQUIVO-CLIENTES  SECTION.                         
      *----------------------------------------------------------------*
           DELETE ARQ-CLIENTES
           IF NOT FS-OK
               PERFORM 99991-ERRO-ARQUIVO-CLIENTES
           ELSE
               MOVE "CLIENTE REMOVIDO COM SUCESSO"
                                       TO WS-STATUS
           END-IF
           .
       13100-FIM. EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       99991-ERRO-ARQUIVO-CLIENTES     SECTION.                
      *----------------------------------------------------------------*
           IF NOT FS-OK
               IF FS-ERRO-LAYOUT    
                   MOVE "ERRO NO LAYOUT DO ARQUIVO"    TO WS-STATUS
               ELSE IF FS-CANCELA
                   MOVE "ERRO NO ACESSO AO ARQUIVO"    TO WS-STATUS
               END-IF
           END-IF
           PERFORM 11400-FECHAR-ARQUIVO-CLIENTES
           PERFORM 10000-MENU-CADASTRO-CLIENTE
           .
       99991-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       END PROGRAM.
      *----------------------------------------------------------------*