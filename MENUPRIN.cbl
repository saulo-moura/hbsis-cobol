      ******************************************************************
      * PROGRAMA.: MENUPRIN                                            *
      * AUTOR....: SAULO MARIO DE MOURA                                *
      * DATA.....: 14/06/2019                                          *
      * OBJETIVO.: MENU INICIAL PARA GERENCIAMENTO DE CARTEIRA DE      *
      *            CLIENTES                                            *
      ******************************************************************
       IDENTIFICATION                  DIVISION.
      ******************************************************************
      *
       PROGRAM-ID. MENUPRIN.
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
      ******************************************************************
       DATA                            DIVISION.
      ******************************************************************
      *
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
      *
       01 WS-STATUS                    PIC X(40). 
       77 WS-OPCAO                     PIC X(5).
      *     
      *----------------------------------------------------------------*
       SCREEN                          SECTION.
      *----------------------------------------------------------------*
      *
      * --> MENU PRINCIPAL
      *
       COPY PMENUSCR.
      *   
      * --> LABEL PARA MOSTRAR MENSAGENS
      *          
       01 OPCAO-STATUS                 HIGHLIGHT.
           05 STATUS-OUTPUT                            LINE 25 COL 10
                                       PIC X(40)       FROM WS-STATUS.
      *
      ******************************************************************
       PROCEDURE                       DIVISION.
      ******************************************************************
      *                                                                *
      *----------------------------------------------------------------*
       PERFORM 0000-MENU-PRINCIPAL.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       0000-MENU-PRINCIPAL            SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM UNTIL 1 <> 1
               DISPLAY SCR-MENU-PRINCIPAL
               DISPLAY OPCAO-STATUS
               ACCEPT OPCAO-PRINC
               MOVE SPACES                 TO WS-STATUS
               EVALUATE WS-OPCAO
                   WHEN "01.01"
                       CALL "CADACLIE"
                   WHEN "01.02"
                       CALL "CADAVEND"
                   WHEN "02.01"
                       CALL "RELACLIE"
                   WHEN "02.02"
                       CALL "RELAVEND"
                   WHEN "s"
                   WHEN "S"
                       GOBACK
                   WHEN OTHER
                       MOVE "OPCAO INVALIDA"   
                                       TO WS-STATUS
               END-EVALUATE
           END-PERFORM
           .
       0000-FIM. EXIT.    
      *----------------------------------------------------------------*