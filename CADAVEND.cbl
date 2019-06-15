      ******************************************************************
      * PROGRAMA.: CADAVEND                                            *
      * AUTOR....: SAULO MARIO DE MOURA                                *
      * DATA.....: 14/06/2019                                          *
      * OBJETIVO.: CADASTRO DE VENDEDORES                              *
      ******************************************************************
       IDENTIFICATION                  DIVISION.
      ******************************************************************
      *
       PROGRAM-ID. CADAVEND.
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
       77 WS-OPCAO-VEN                 PIC X.
       77 WS-OPCAO-INC-VEN             PIC X.
       77 WS-FILE-STATUS               PIC X(02).                       
           88 FS-VEN-OK                VALUE "00" THRU "09".
           88 FS-VEN-FIM               VALUE "10".
           88 FS-VEN-NAO-ENCONTRADO    VALUE "23".
           88 FS-VEN-ERRO-LAYOUT       VALUE "39".
           88 FS-VEN-CANCELA           VALUE "99".
      *     
      *----------------------------------------------------------------*
       SCREEN                          SECTION.
      *----------------------------------------------------------------*
      *
      * --> MENU DE CADASTRO DE VENDEDORES
      *                         
       COPY VMENUSCR.
      * 
      * --> MENU DE INCLUSAO DE VENDEDORES
      *
       COPY INVENSCR.
      *
      * --> MENU DE ALTERACAO DE VENDEDORES
      *
       COPY ALVENSCR.
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
       PERFORM 0000-MENU-CADASTRO-VENDEDOR.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       0000-MENU-CADASTRO-VENDEDOR    SECTION.                        
      *----------------------------------------------------------------*
      *
           PERFORM UNTIL 1 <> 1
               DISPLAY SCR-MENU-VENDEDORES
               DISPLAY OPCAO-STATUS
               ACCEPT OPCAO-VEN
               MOVE SPACES             TO WS-STATUS
               EVALUATE WS-OPCAO-VEN
                   WHEN "1"
                       MOVE SPACES     TO WS-STATUS
                       PERFORM 1000-INCLUIR-VENDEDOR 
                   WHEN "2"
                       MOVE SPACES     TO WS-STATUS
                       PERFORM 2000-ALTERAR-VENDEDOR
                   WHEN "3"
                       MOVE SPACES     TO WS-STATUS
                       PERFORM 3000-EXCLUIR-VENDEDOR
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
       1000-INCLUIR-VENDEDOR           SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE REGISTRO         REPLACING NUMERIC BY ZEROS
                                       ALPHANUMERIC BY SPACES
           PERFORM UNTIL 1 <> 1
               DISPLAY SCR-INCLUIR-VENDEDOR
               ACCEPT SCR-COD-VEN
               ACCEPT SCR-CPF-VEN
               ACCEPT SCR-NOME-VEN
               ACCEPT SCR-LATITUDE-VEN
               ACCEPT SCR-LONGITUDE-VEN
               ACCEPT OPCAO-INC-VEN
               MOVE SPACES             TO WS-STATUS
               EVALUATE WS-OPCAO-INC-VEN
                   WHEN "S"
                       PERFORM 1100-ABRIR-ARQUIVO-VENDEDORES
                       PERFORM 1200-PESQUISAR-VENDEDOR
                       IF FS-VEN-NAO-ENCONTRADO
                           PERFORM 1300-GRAVAR-ARQUIVO-VENDEDORES
                       ELSE
                           MOVE "CPF JA CADASTRADO"       
                                           TO WS-STATUS
                       END-IF
                       PERFORM 1400-FECHAR-ARQUIVO-VENDEDORES
                       PERFORM 0000-MENU-CADASTRO-VENDEDOR
                   WHEN "V"
                       PERFORM 0000-MENU-CADASTRO-VENDEDOR
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
       1100-ABRIR-ARQUIVO-VENDEDORES   SECTION.
      *----------------------------------------------------------------*
      *
           OPEN I-O ARQ-VENDEDORES
           IF NOT FS-VEN-OK
               PERFORM 9999-ERRO-ARQUIVO-VENDEDORES
           END-IF
           .
       1100-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       1200-PESQUISAR-VENDEDOR         SECTION.
      *----------------------------------------------------------------*
      *
           READ ARQ-VENDEDORES         INTO REGISTRO       
           IF NOT FS-VEN-OK AND NOT FS-VEN-NAO-ENCONTRADO
             AND NOT FS-VEN-FIM
               PERFORM 9999-ERRO-ARQUIVO-VENDEDORES
           END-IF
           .
       1200-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       1300-GRAVAR-ARQUIVO-VENDEDORES SECTION.
      *----------------------------------------------------------------*
      *
           WRITE REGISTRO
           IF NOT FS-VEN-OK
               PERFORM 9999-ERRO-ARQUIVO-VENDEDORES                    
           ELSE
               MOVE "VENDEDOR CADASTRADO COM SUCESSO"
                                       TO WS-STATUS
           END-IF
           .
       1300-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       1400-FECHAR-ARQUIVO-VENDEDORES SECTION.                       
      *----------------------------------------------------------------*
      *
           CLOSE ARQ-VENDEDORES
           IF NOT FS-VEN-OK
               PERFORM 9999-ERRO-ARQUIVO-VENDEDORES                    
           END-IF
           .
       1400-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       2000-ALTERAR-VENDEDOR          SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 1100-ABRIR-ARQUIVO-VENDEDORES
           DISPLAY SCR-ALTERAR-VENDEDOR
           ACCEPT SCR-CPF-ALT-VEN
           PERFORM 1200-PESQUISAR-VENDEDOR
           IF FS-VEN-NAO-ENCONTRADO
               MOVE "VENDEDOR NAO ENCONTRADO"
                                       TO WS-STATUS
               PERFORM 1400-FECHAR-ARQUIVO-VENDEDORES
               PERFORM 0000-MENU-CADASTRO-VENDEDOR
           ELSE
               DISPLAY SCR-INCLUIR-VENDEDOR
               MOVE SPACES             TO WS-STATUS
               PERFORM UNTIL 1 <> 1
                   DISPLAY OPCAO-STATUS
                   ACCEPT SCR-COD-VEN
                   ACCEPT SCR-NOME-VEN
                   ACCEPT SCR-LATITUDE-VEN
                   ACCEPT SCR-LONGITUDE-VEN
                   ACCEPT OPCAO-INC-VEN
                   EVALUATE WS-OPCAO-INC-VEN
                       WHEN "s"
                       WHEN "S"
                           PERFORM 2100-ALTERAR-ARQUIVO-VENDEDORES
                           PERFORM 1400-FECHAR-ARQUIVO-VENDEDORES      
                           PERFORM 0000-MENU-CADASTRO-VENDEDOR
                       WHEN "v"
                       WHEN "V"
                           PERFORM 1400-FECHAR-ARQUIVO-VENDEDORES      
                           PERFORM 0000-MENU-CADASTRO-VENDEDOR
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
       2100-ALTERAR-ARQUIVO-VENDEDORES SECTION.                     
      *----------------------------------------------------------------*
      *
           REWRITE REGISTRO
           IF NOT FS-VEN-OK
               PERFORM 9999-ERRO-ARQUIVO-VENDEDORES
           ELSE
               MOVE "VENDEDOR ALTERADO COM SUCESSO"
                                       TO WS-STATUS
           END-IF
           .
       2100-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       3000-EXCLUIR-VENDEDOR           SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 1100-ABRIR-ARQUIVO-VENDEDORES
           DISPLAY SCR-ALTERAR-VENDEDOR
           ACCEPT SCR-CPF-ALT-VEN
           PERFORM 1200-PESQUISAR-VENDEDOR
           IF FS-VEN-NAO-ENCONTRADO
               MOVE "VENDEDOR NAO ENCONTRADO"
                                       TO WS-STATUS
           ELSE
               PERFORM 3100-EXCLUIR-ARQUIVO-VENDEDORES
           END-IF
      *     
           PERFORM 1400-FECHAR-ARQUIVO-VENDEDORES
           PERFORM 0000-MENU-CADASTRO-VENDEDOR
           .
       3000-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       3100-EXCLUIR-ARQUIVO-VENDEDORES SECTION.                       
      *----------------------------------------------------------------*
      *
           DELETE ARQ-VENDEDORES
           IF NOT FS-VEN-OK
               PERFORM 9999-ERRO-ARQUIVO-VENDEDORES
           ELSE
               MOVE "VENDEDOR REMOVIDO COM SUCESSO"
                                       TO WS-STATUS
           END-IF
           .
       3100-FIM. EXIT.
      *----------------------------------------------------------------*
      *         
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
           PERFORM 1400-FECHAR-ARQUIVO-VENDEDORES
           GOBACK
           .
       9999-FIM. EXIT.
      *----------------------------------------------------------------*