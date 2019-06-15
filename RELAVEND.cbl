      ******************************************************************
      * PROGRAMA.: RELAVEND                                            *
      * AUTOR....: SAULO MARIO DE MOURA                                *
      * DATA.....: 14/06/2019                                          *
      * OBJETIVO.: RELATORIO DE VENDEDORES CADASTRADOS                 *
      ******************************************************************
       IDENTIFICATION                  DIVISION.
      ******************************************************************
      *
       PROGRAM-ID. RELAVEND.
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
           SELECT ARQ-VENDEDORES       ASSIGN TO  "WID-ARQ-VEN.DAT"     
               ORGANIZATION            IS INDEXED
               ACCESS MODE             IS DYNAMIC
               RECORD KEY              IS CPF
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
       FD ARQ-VENDEDORES.
       COPY REGICADA.
      *
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
      *
       01 WS-STATUS                    PIC X(40). 
       77 WS-OPCAO                     PIC X(5).
       77 WS-FILE-STATUS               PIC X(02).
           88 FS-VEN-OK                VALUE "00" THRU "09".
           88 FS-VEN-FIM               VALUE "10".
           88 FS-VEN-NAO-ENCONTRADO    VALUE "23".
           88 FS-VEN-NAO-EXISTE        VALUE "35".
           88 FS-VEN-ERRO-LAYOUT       VALUE "39".
           88 FS-VEN-CANCELA           VALUE "99".
      *     
       01  REL-CONFIG.
           05 LN                       PIC 9(2)        VALUE 5.
           05 IND                      PIC 9(2)        VALUE 0.
       01  REL-INFO-VENDEDOR           OCCURS 100 TIMES.
           10 FILLER                   PIC X(4).
           10 REL-VEN-CODIGO           PIC 9(3).
           10 REL-VEN-CPF              PIC 9(11).
           10 FILLER                   PIC X(3).
           10 REL-VEN-NOME             PIC X(40).
           10 REL-VEN-LATITUDE         PIC S9(03)V9(08).
           10 REL-VEN-LONGITUDE        PIC S9(03)V9(08).
      *     
      *----------------------------------------------------------------*
       SCREEN                          SECTION.
      *----------------------------------------------------------------*
      *
      * --> RELATORIO DE VENDEDORES
      *
       COPY RELATVEN.
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
       PERFORM 0000-MOSTRAR-RELATORIO-VENDEDOR.
      *----------------------------------------------------------------*
      *                                                                 
      *----------------------------------------------------------------*
       0000-MOSTRAR-RELATORIO-VENDEDOR SECTION.                      
      *----------------------------------------------------------------*
      *
           INITIALIZE REGISTRO         REPLACING NUMERIC BY ZEROS
                                       ALPHANUMERIC BY SPACES
           MOVE 1                      TO IND
           MOVE 5                      TO LN
           DISPLAY CLEAR-SCREEN
           PERFORM 1000-ABRIR-ARQUIVO-VENDEDORES
           IF FS-VEN-NAO-EXISTE
               DISPLAY SEM-DADOS
           ELSE
               PERFORM 2000-LER-ARQUIVO-VENDEDORES
               IF FS-VEN-FIM
                   DISPLAY SEM-DADOS
               ELSE
                   PERFORM 3000-FORMATAR-RELATORIO-VENDEDOR
                                       UNTIL FS-VEN-FIM
                   PERFORM 4000-FECHAR-ARQUIVO-VENDEDORES
               END-IF
           END-IF
           ACCEPT WS-OPCAO
           GOBACK
           .
       0000-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       1000-ABRIR-ARQUIVO-VENDEDORES  SECTION.
      *----------------------------------------------------------------*
      *
           OPEN INPUT ARQ-VENDEDORES
           IF NOT FS-VEN-OK AND NOT FS-VEN-NAO-EXISTE
               PERFORM 9999-ERRO-ARQUIVO-VENDEDORES
           END-IF
           .
       1000-FIM. EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2000-LER-ARQUIVO-VENDEDORES    SECTION.                       
      *----------------------------------------------------------------*
      *
           READ ARQ-VENDEDORES           NEXT       
           IF NOT FS-VEN-OK AND NOT FS-VEN-FIM
               PERFORM 9999-ERRO-ARQUIVO-VENDEDORES
           END-IF
           .
       2000-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       3000-FORMATAR-RELATORIO-VENDEDOR SECTION.                       
      *----------------------------------------------------------------*
      *
           MOVE REGISTRO               TO REL-INFO-VENDEDOR(IND)
           DISPLAY REL-VENDEDORES
           ADD 1                       TO IND
           ADD 1                       TO LN
           PERFORM 2000-LER-ARQUIVO-VENDEDORES                         
           .
       3000-FIM. EXIT.                                                 
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       4000-FECHAR-ARQUIVO-VENDEDORES  SECTION.                       
      *----------------------------------------------------------------*
      *
           CLOSE ARQ-VENDEDORES
           IF NOT FS-VEN-OK
               PERFORM 9999-ERRO-ARQUIVO-VENDEDORES                    
           END-IF
           .
       4000-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       9999-ERRO-ARQUIVO-VENDEDORES    SECTION.                
      *----------------------------------------------------------------*
      *
           IF FS-VEN-ERRO-LAYOUT    
               MOVE "ERRO NO LAYOUT DO ARQUIVO"    TO WS-STATUS
           ELSE IF FS-VEN-CANCELA
               MOVE "ERRO NO ACESSO AO ARQUIVO"    TO WS-STATUS
           END-IF
      *     
           PERFORM 4000-FECHAR-ARQUIVO-VENDEDORES
           GOBACK
           .
       9999-FIM. EXIT.
      *----------------------------------------------------------------*