      ******************************************************************
      * PROGRAMA.: CADACLIE                                            *
      * AUTOR....: SAULO MARIO DE MOURA                                *
      * DATA.....: 14/06/2019                                          *
      * OBJETIVO.: CADASTRO DE CLIENTES                                *
      ******************************************************************
       IDENTIFICATION                  DIVISION.
      ******************************************************************
      *
       PROGRAM-ID. CADACLIE.
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
      *
           SELECT ARQ-CLIENTES         ASSIGN TO  "WID-ARQ-CLI.DAT"
               ORGANIZATION            IS INDEXED
               ACCESS MODE             IS DYNAMIC
               RECORD KEY              IS CNPJ
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
       COPY REGICADA.
      *
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
      *
       01 WS-STATUS                    PIC X(40). 
       77 WS-OPCAO-CLI                 PIC X.
       77 WS-OPCAO-INC-CLI             PIC X.
       77 WS-FILE-STATUS               PIC X(02).                      
           88 FS-CLI-OK                VALUE "00" THRU "09".
           88 FS-CLI-FIM               VALUE "10".
           88 FS-CLI-NAO-ENCONTRADO    VALUE "23".
           88 FS-CLI-ERRO-LAYOUT       VALUE "39".
           88 FS-CLI-CANCELA           VALUE "99".
      *     
      *----------------------------------------------------------------*
       SCREEN                          SECTION.
      *----------------------------------------------------------------*
      *
      * --> MENU DE CADASTRO DE CLIENTES
      *                         
       COPY CMENUSCR.
      * 
      * --> MENU DE INCLUSAO DE CLIENTES
      *
       COPY INCLISCR.
      *
      * --> MENU ALTERACAO DE CLIENTE
      *
       COPY ALCLISCR.
      *
      * --> LABEL PARA MOSTRAR MENSAGENS
      *          
       01 OPCAO-STATUS                 HIGHLIGHT.
           05 STATUS-OUTPUT                            LINE 25 COL 10
                                       PIC X(40)       FROM WS-STATUS.
      *
      * --> LIMPAR TELA
      *
       01 CLEAR-SCREEN.
           05  CLEAR                   BLANK SCREEN.
      *
      ******************************************************************
       PROCEDURE                       DIVISION.
      ******************************************************************
      *                                                                *
      *----------------------------------------------------------------*
       PERFORM 0000-MENU-CADASTRO-CLIENTE.
      *----------------------------------------------------------------*
       0000-MENU-CADASTRO-CLIENTE     SECTION.                         
      *----------------------------------------------------------------*
      *
           PERFORM UNTIL 1 <> 1
               DISPLAY SCR-MENU-CLIENTES
               DISPLAY OPCAO-STATUS
               ACCEPT OPCAO-CLI
               MOVE SPACES TO WS-STATUS
               EVALUATE WS-OPCAO-CLI
                   WHEN "1"
                       PERFORM 1000-INCLUIR-CLIENTE 
                   WHEN "2"
                       PERFORM 2000-ALTERAR-CLIENTE
                   WHEN "3"
                       PERFORM 3000-EXCLUIR-CLIENTE
                   WHEN "m"
                   WHEN "M"
                       GOBACK
                   WHEN OTHER
                       MOVE "OPCAO INVALIDA"   
                                       TO WS-STATUS
               END-EVALUATE
           END-PERFORM
           .
       0000-FIM. EXIT.    
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       1000-INCLUIR-CLIENTE            SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE REGISTRO         REPLACING NUMERIC BY ZEROS
                                       ALPHANUMERIC BY SPACES
           PERFORM UNTIL 1 <> 1
               DISPLAY SCR-INCLUIR-CLIENTE
               ACCEPT SCR-COD-CLI
               ACCEPT SCR-CNPJ-CLI
               ACCEPT SCR-RAZAO-SOCIAL-CLI
               ACCEPT SCR-LATITUDE-CLI
               ACCEPT SCR-LONGITUDE-CLI
               ACCEPT OPCAO-INC-CLI
               MOVE SPACES TO WS-STATUS
               EVALUATE WS-OPCAO-INC-CLI
                   WHEN "S"
                       PERFORM 1100-ABRIR-ARQUIVO-CLIENTES
                       PERFORM 1200-PESQUISAR-CLIENTE
                       IF FS-CLI-NAO-ENCONTRADO
                           PERFORM 1300-GRAVAR-ARQUIVO-CLIENTES
                       ELSE
                           MOVE "CNPJ JA CADASTRADO"       
                                           TO WS-STATUS
                       END-IF
                       PERFORM 1400-FECHAR-ARQUIVO-CLIENTES
                       PERFORM 0000-MENU-CADASTRO-CLIENTE
                   WHEN "V"
                       PERFORM 0000-MENU-CADASTRO-CLIENTE
                   WHEN OTHER
                       MOVE "OPCAO INVALIDA"   
                                           TO WS-STATUS
               END-EVALUATE
            END-PERFORM
           .
       1000-FIM. EXIT.    
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       1100-ABRIR-ARQUIVO-CLIENTES     SECTION.
      *----------------------------------------------------------------*
      *
           OPEN I-O ARQ-CLIENTES
           IF NOT FS-CLI-OK
               PERFORM 9999-ERRO-ARQUIVO-CLIENTES
           END-IF
           .
      *     
       1100-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       1200-PESQUISAR-CLIENTE      SECTION.
      *----------------------------------------------------------------*
      *
           READ ARQ-CLIENTES           INTO REGISTRO
           IF  NOT FS-CLI-OK AND NOT FS-CLI-NAO-ENCONTRADO 
               AND NOT FS-CLI-FIM
               PERFORM 9999-ERRO-ARQUIVO-CLIENTES
           END-IF
           .
       1200-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       1300-GRAVAR-ARQUIVO-CLIENTES   SECTION.
      *----------------------------------------------------------------*
      *
           WRITE REGISTRO
           IF NOT FS-CLI-OK
               PERFORM 9999-ERRO-ARQUIVO-CLIENTES
           ELSE
               MOVE "CLIENTE CADASTRADO COM SUCESSO"
                                       TO WS-STATUS
           END-IF
           .
       1300-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       1400-FECHAR-ARQUIVO-CLIENTES   SECTION.                         
      *----------------------------------------------------------------*
      *
           CLOSE ARQ-CLIENTES
           IF NOT FS-CLI-OK
               PERFORM 9999-ERRO-ARQUIVO-CLIENTES
           END-IF
           .
       1400-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       2000-ALTERAR-CLIENTE            SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 1100-ABRIR-ARQUIVO-CLIENTES
           DISPLAY SCR-ALTERAR-CLIENTE
           ACCEPT SCR-CNPJ-ALT-CLI
           MOVE SPACES TO WS-STATUS
           PERFORM 1200-PESQUISAR-CLIENTE
           IF FS-CLI-NAO-ENCONTRADO
               MOVE "CLIENTE NAO ENCONTRADO"
                                       TO WS-STATUS
               PERFORM 1400-FECHAR-ARQUIVO-CLIENTES
               PERFORM 0000-MENU-CADASTRO-CLIENTE
           ELSE
               DISPLAY SCR-INCLUIR-CLIENTE
               MOVE SPACES             TO WS-STATUS
               PERFORM UNTIL 1 <> 1
                   DISPLAY OPCAO-STATUS
                   ACCEPT SCR-COD-CLI
                   ACCEPT SCR-RAZAO-SOCIAL-CLI
                   ACCEPT SCR-LATITUDE-CLI
                   ACCEPT SCR-LONGITUDE-CLI
                   ACCEPT OPCAO-INC-CLI
                   EVALUATE WS-OPCAO-INC-CLI
                       WHEN "s"
                       WHEN "S"
                           PERFORM 2100-ALTERAR-ARQUIVO-CLIENTES
                           PERFORM 1400-FECHAR-ARQUIVO-CLIENTES
                           PERFORM 0000-MENU-CADASTRO-CLIENTE
                       WHEN "v"
                       WHEN "V"
                           PERFORM 1400-FECHAR-ARQUIVO-CLIENTES
                           PERFORM 0000-MENU-CADASTRO-CLIENTE
                       WHEN OTHER
                           MOVE "OPCAO INVALIDA"   
                                               TO WS-STATUS
                   END-EVALUATE
               END-PERFORM
           END-IF
           .
       2000-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       2100-ALTERAR-ARQUIVO-CLIENTES  SECTION.                        
      *----------------------------------------------------------------*
      *
           REWRITE REGISTRO
           IF NOT FS-CLI-OK
               PERFORM 9999-ERRO-ARQUIVO-CLIENTES
           ELSE
               MOVE "CLIENTE ALTERADO COM SUCESSO"
                                       TO WS-STATUS
           END-IF
           .
       2100-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       3000-EXCLUIR-CLIENTE            SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 1100-ABRIR-ARQUIVO-CLIENTES
           DISPLAY SCR-ALTERAR-CLIENTE
           ACCEPT SCR-CNPJ-ALT-CLI
           MOVE SPACES                 TO WS-STATUS
           PERFORM 1200-PESQUISAR-CLIENTE
           IF FS-CLI-NAO-ENCONTRADO
               MOVE "CLIENTE NAO ENCONTRADO"
                                       TO WS-STATUS
           ELSE
               PERFORM 3100-EXCLUIR-ARQUIVO-CLIENTES
           END-IF
      *     
           PERFORM 1400-FECHAR-ARQUIVO-CLIENTES
           PERFORM 0000-MENU-CADASTRO-CLIENTE
           .
       3000-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       3100-EXCLUIR-ARQUIVO-CLIENTES   SECTION.                         
      *----------------------------------------------------------------*
      *
           DELETE ARQ-CLIENTES
           IF NOT FS-CLI-OK
               PERFORM 9999-ERRO-ARQUIVO-CLIENTES
           ELSE
               MOVE "CLIENTE REMOVIDO COM SUCESSO"
                                       TO WS-STATUS
           END-IF
           .
       3100-FIM. EXIT.  
      *----------------------------------------------------------------*
      *       
      *----------------------------------------------------------------*
       9999-ERRO-ARQUIVO-CLIENTES      SECTION.                
      *----------------------------------------------------------------*
      *
           IF FS-CLI-ERRO-LAYOUT    
               MOVE "ERRO NO LAYOUT DO ARQUIVO"    TO WS-STATUS
           ELSE IF FS-CLI-CANCELA
               MOVE "ERRO NO ACESSO AO ARQUIVO"    TO WS-STATUS
           END-IF
      *     
           PERFORM 1400-FECHAR-ARQUIVO-CLIENTES
           GOBACK
           .
       9999-FIM. EXIT.
      *----------------------------------------------------------------*