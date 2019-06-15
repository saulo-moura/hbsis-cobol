      ******************************************************************
      * PROGRAMA.: RELACLIE                                            *
      * AUTOR....: SAULO MARIO DE MOURA                                *
      * DATA.....: 14/06/2019                                          *
      * OBJETIVO.: RELATORIO DE CLIENTES CADASTRADOS                   *
      ******************************************************************
       IDENTIFICATION                  DIVISION.
      ******************************************************************
      *
       PROGRAM-ID. RELACLIE.
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
       77 WS-OPCAO                     PIC X(5).
       77 WS-FILE-STATUS               PIC X(02).
           88 FS-CLI-OK                VALUE "00" THRU "09".
           88 FS-CLI-FIM               VALUE "10".
           88 FS-CLI-NAO-ENCONTRADO    VALUE "23".
           88 FS-CLI-NAO-EXISTE        VALUE "35".
           88 FS-CLI-ERRO-LAYOUT       VALUE "39".
           88 FS-CLI-CANCELA           VALUE "99".
      *     
       01  REL-CONFIG.
           05 LN                       PIC 9(2)        VALUE 5.
           05 IND                      PIC 9(2)        VALUE 0.
       01  REL-INFO-CLIENTE            OCCURS 100 TIMES.
           10 REL-CLI-CODIGO           PIC 9(7).
           10 REL-CLI-CNPJ             PIC 9(14).
           10 REL-CLI-RAZAO-SOCIAL     PIC X(40).
           10 REL-CLI-LATITUDE         PIC S9(03)V9(08).
           10 REL-CLI-LONGITUDE        PIC S9(03)V9(08).
      *     
      *----------------------------------------------------------------*
       SCREEN                          SECTION.
      *----------------------------------------------------------------*
      *
      * --> RELATORIO DE CLIENTES
      *
       COPY RELATCLI.
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
       PERFORM 0000-MOSTRAR-RELATORIO-CLIENTE.
      *----------------------------------------------------------------*
      *                                                         
      *----------------------------------------------------------------*
       0000-MOSTRAR-RELATORIO-CLIENTE  SECTION.                      
      *----------------------------------------------------------------*
      *
           INITIALIZE REGISTRO         REPLACING NUMERIC BY ZEROS
                                       ALPHANUMERIC BY SPACES
           MOVE 1                      TO IND
           MOVE 5                      TO LN
           DISPLAY CLEAR-SCREEN
           PERFORM 1000-ABRIR-ARQUIVO-CLIENTES
           IF FS-CLI-NAO-EXISTE
               DISPLAY SEM-DADOS
           ELSE
               PERFORM 2000-LER-ARQUIVO-CLIENTES
               IF FS-CLI-FIM
                   DISPLAY SEM-DADOS
               ELSE
                   PERFORM 3000-FORMATAR-RELATORIO-CLIENTE
                                       UNTIL FS-CLI-FIM
                   PERFORM 4000-FECHAR-ARQUIVO-CLIENTES
               END-IF
           END-IF    
           ACCEPT WS-OPCAO
           GOBACK
           .
       0000-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       1000-ABRIR-ARQUIVO-CLIENTES     SECTION.
      *----------------------------------------------------------------*
      *
           OPEN INPUT ARQ-CLIENTES
           IF NOT FS-CLI-OK AND NOT FS-CLI-NAO-EXISTE
               PERFORM 9999-ERRO-ARQUIVO-CLIENTES
           END-IF
           .
      *     
       1000-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       2000-LER-ARQUIVO-CLIENTES      SECTION.                       
      *----------------------------------------------------------------*
      *
           READ ARQ-CLIENTES           NEXT       
           IF NOT FS-CLI-OK AND NOT FS-CLI-FIM
               PERFORM 9999-ERRO-ARQUIVO-CLIENTES
           END-IF
           .
       2000-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       3000-FORMATAR-RELATORIO-CLIENTE SECTION.                        
      *----------------------------------------------------------------*
      *
           MOVE REGISTRO               TO REL-INFO-CLIENTE (IND)
           DISPLAY REL-CLIENTES
           ADD 1                       TO IND
           ADD 1                       TO LN
           PERFORM 2000-LER-ARQUIVO-CLIENTES                           
           .
       3000-FIM. EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       4000-FECHAR-ARQUIVO-CLIENTES   SECTION.                         
      *----------------------------------------------------------------*
      *
           CLOSE ARQ-CLIENTES
           IF NOT FS-CLI-OK
               PERFORM 9999-ERRO-ARQUIVO-CLIENTES
           END-IF
           .
       4000-FIM. EXIT.
      *----------------------------------------------------------------*
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
           PERFORM 4000-FECHAR-ARQUIVO-CLIENTES
           GOBACK
           .
       9999-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       END PROGRAM.
      *----------------------------------------------------------------*