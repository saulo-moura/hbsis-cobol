      ******************************************************************
       IDENTIFICATION                  DIVISION.
      ******************************************************************
      *
       PROGRAM-ID. CADAMBEV.
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
      * --> ARQUIVO DE CLIENTES
      *      
           SELECT ARQ-CLIENTES         ASSIGN TO  "WID-ARQ-CLI.DAT"
               ORGANIZATION            IS INDEXED
               ACCESS MODE             IS DYNAMIC
               RECORD KEY              IS CLI-CNPJ
               LOCK MODE               IS MANUAL
               FILE STATUS             IS WS-FS-CLIENTE.
      *         
      * --> ARQUIVO DE VENDEDORES
      *   
           SELECT ARQ-VENDEDORES       ASSIGN TO  "WID-ARQ-VEN.DAT"     
               ORGANIZATION            IS INDEXED
               ACCESS MODE             IS DYNAMIC
               RECORD KEY              IS VEN-CPF
               LOCK MODE               IS MANUAL
               FILE STATUS             IS WS-FS-VENDEDOR.
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
       FD ARQ-VENDEDORES.
       01 REG-VENDEDOR.
           05 VEN-CODIGO               PIC 9(03).
           05 VEN-CPF                  PIC 9(11).
           05 VEN-NOME                 PIC X(40).
           05 VEN-LATITUDE             PIC S9(03)V9(08).
           05 VEN-LONGITUDE            PIC S9(03)V9(08).
      *
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
      *
       01 WS-STATUS                    PIC X(40). 
       77 WS-OPCAO                     PIC X(5).
       77 WS-OPCAO-CLI                 PIC X.
       77 WS-OPCAO-INC-CLI             PIC X.
       77 WS-OPCAO-VEN                 PIC X.
       77 WS-OPCAO-INC-VEN             PIC X.
       77 WS-FS-CLIENTE                PIC X(02).
           88 FS-CLI-OK                VALUE "00" THRU "09".
           88 FS-CLI-FIM               VALUE "10".
           88 FS-CLI-NAO-ENCONTRADO    VALUE "23".
           88 FS-CLI-ERRO-LAYOUT       VALUE "39".
           88 FS-CLI-CANCELA           VALUE "99".
       77 WS-FS-VENDEDOR               PIC X(02).
           88 FS-VEN-OK                VALUE "00" THRU "09".
           88 FS-VEN-FIM               VALUE "10".
           88 FS-VEN-NAO-ENCONTRADO    VALUE "23".
           88 FS-VEN-ERRO-LAYOUT       VALUE "39".
           88 FS-VEN-CANCELA           VALUE "99".
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
       01  REL-INFO-VENDEDOR           OCCURS 100 TIMES.
           10 REL-VEN-CODIGO           PIC 9(3).
           10 REL-VEN-CPF              PIC 9(11).
           10 REL-VEN-NOME             PIC X(40).
           10 REL-VEN-LATITUDE         PIC S9(03)V9(08).
           10 REL-VEN-LONGITUDE        PIC S9(03)V9(08).
      *     
      *----------------------------------------------------------------*
       SCREEN                          SECTION.
      *----------------------------------------------------------------*
      *
      * --> MENU PRINCIPAL
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
      * --> MENU DE CADASTRO DE CLIENTES
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
      * --> MENU DE INCLUSAO DE CLIENTES
      *
       01  SCR-INCLUIR-CLIENTE.
           05  INCLUIR-CLIENTE-CABECALHO.
               10  VALUE "CADASTRO DE CLIENTE - AMBEV"           
                                       BLANK SCREEN    LINE 1 COL 50.
           05  INCLUIR-CLIENTE-DADOS.
               10  VALUE "CODIGO: "                    LINE 5 COL 10.
               10  SCR-COD-CLI                         LINE 5 COL 35
                                       PIC ZZZZZZZ  
                                       USING CLI-CODIGO.
               10  VALUE "CNPJ:"                       LINE 6 COL 10.
               10  SCR-CNPJ-CLI                        LINE 6 COL 35
                                       PIC 99.999.999/9999B99
                                       USING CLI-CNPJ.
               10  VALUE "RAZAO SOCIAL:"                LINE 7 COL 10.
               10  SCR-RAZAO-SOCIAL-CLI                LINE 7 COL 35
                                       PIC X(40) USING CLI-RAZAO-SOCIAL.
               10  VALUE "LATITUDE:"                   LINE 8 COL 10.
               10  SCR-LATITUDE-CLI                    LINE 8 COL 35
                                       PIC 999,99999999
                                       USING CLI-LATITUDE.
               10  VALUE "LONGITUDE"                   LINE 9 COL 10.
               10  SCR-LONGITUDE-CLI                   LINE 9 COL 35 
                                       PIC 999,99999999
                                       USING CLI-LONGITUDE.
               10  VALUE "S - SALVAR"                  LINE 16 COL 10.
               10  VALUE "V - VOLTAR"                  LINE 17 COL 10. 
           05 ESCOLHA-INCLUIR-CLIENTE.
               10  VALUE "DIGITE A OPCAO DESEJADA"     LINE 20 COL 10.
               10  OPCAO-INC-CLI                       LINE 20 COL 35
                                       PIC X       TO WS-OPCAO-INC-CLI.
      *
      * --> MENU ALTERACAO DE CLIENTE
      *
       01  SCR-ALTERAR-CLIENTE.
           05 ALTERAR-CLIENTE-CABECALHO.
               10  VALUE "CADASTRO DE CLIENTE - AMBEV"           
                                       BLANK SCREEN    LINE 1 COL 50.
           05  CNPJ-BUSCA-CLIENTE.
               10  VALUE "DIGITE O CNPJ DO CLIENTE"    LINE 5 COL 10.
               10  SCR-CNPJ-ALT-CLI                    LINE 5 COL 35
                                       PIC 99.999.999/9999B99
                                       TO CLI-CNPJ.
      *   
      * --> MENU DE CADASTRO DE VENDEDORES
      *                         
       01 SCR-MENU-VENDEDORES.
           05  MENU-VENDEDORES-CABECALHO.
               10  VALUE "CADASTRO DE VENDEDORES - AMBEV"
                                       BLANK SCREEN    LINE 1 COL 35.
           05  MENU-VENDEDORES-OPCOES.
               10  VALUE "1 - INCLUSAO"                LINE 5 COL 10.
               10  VALUE "2 - ALTERACAO"               LINE 6 COL 10.
               10  VALUE "3 - EXCLUSAO"                LINE 7 COL 10.
               10  VALUE "4 - IMPORTACAO"              LINE 8 COL 10.
               10  VALUE "M - VOLTAR PARA O MENU PRINCIPAL"           
                                                       LINE 16 COL 10. 
           05 ESCOLHA-MENU-VENDEDORES.
               10  VALUE "DIGITE A OPCAO DESEJADA"     LINE 20 COL 10.
               10  OPCAO-VEN                           LINE 20 COL 35
                                       PIC X           TO WS-OPCAO-VEN.
      * 
      * --> MENU DE INCLUSAO DE VENDEDORES
      *
       01  SCR-INCLUIR-VENDEDOR.
           05  INCLUIR-VENDEDOR-CABECALHO.
               10  VALUE "CADASTRO DE VENDEDOR - AMBEV"           
                                       BLANK SCREEN    LINE 1 COL 50.
           05  INCLUIR-VENDEDOR-DADOS.
               10  VALUE "CODIGO: "                    LINE 5 COL 10.
               10  SCR-COD-VEN                         LINE 5 COL 35
                                       PIC ZZZZ  
                                       USING VEN-CODIGO.
               10  VALUE "CPF:"                        LINE 6 COL 10.
               10  SCR-CPF-VEN                         LINE 6 COL 35
                                       PIC 999.999.999B99
                                       USING VEN-CPF.
               10  VALUE "RAZAO SOCIAL:"               LINE 7 COL 10.
               10  SCR-NOME-VEN                        LINE 7 COL 35
                                       PIC X(40) USING VEN-NOME.        
               10  VALUE "LATITUDE:"                   LINE 8 COL 10.
               10  SCR-LATITUDE-VEN                    LINE 8 COL 35
                                       PIC 999,99999999
                                       USING VEN-LATITUDE.
               10  VALUE "LONGITUDE"                   LINE 9 COL 10.
               10  SCR-LONGITUDE-VEN                   LINE 9 COL 35 
                                       PIC 999,99999999
                                       USING VEN-LONGITUDE.
               10  VALUE "S - SALVAR"                  LINE 16 COL 10.
               10  VALUE "V - VOLTAR"                  LINE 17 COL 10. 
           05 ESCOLHA-INCLUIR-VENDEDOR.
               10  VALUE "DIGITE A OPCAO DESEJADA"     LINE 20 COL 10.
               10  OPCAO-INC-VEN                       LINE 20 COL 35
                                       PIC X       TO WS-OPCAO-INC-VEN.
      *
      * --> MENU DE ALTERACAO DE VENDEDORES
      *
       01  SCR-ALTERAR-VENDEDOR.
           05 ALTERAR-VENDEDOR-CABECALHO.
               10  VALUE "CADASTRO DE VENDEDOR - AMBEV"           
                                       BLANK SCREEN    LINE 1 COL 50.
           05  CNPJ-BUSCA-CLIENTE.
               10  VALUE "DIGITE O CPF DO VENDEDOR"    LINE 5 COL 10.
               10  SCR-CPF-ALT-VEN                     LINE 5 COL 35
                                       PIC 999.999.999B99
                                       TO VEN-CPF.
      *  
      * --> LABEL PARA MOSTRAR MENSAGENS
      *          
       01 OPCAO-STATUS                 HIGHLIGHT.
           05 STATUS-OUTPUT                            LINE 25 COL 10
                                       PIC X(40)       FROM WS-STATUS.
      *
      * --> RELATORIO DE CLIENTES
      *
       01 REL-CLIENTES.
           05  CABECALHO.
               10  VALUE "RELATORIO DE CLIENTES"       LINE 1 COL 50.
               10  VALUE "CODIGO"                      LINE 3 COL 13.
               10  VALUE "CNPJ"                        LINE 3 COL 23.
               10  VALUE "RAZAO SOCIAL"                LINE 3 COL 44.
               10  VALUE "LATITUDE"                    LINE 3 COL 87.
               10  VALUE "LONGITUDE"                   LINE 3 COL 102.
           05  CONTEUDO.
               10  OUT-COD-CLI         LINE LN COL 13
                                       PIC ZZZZZZZ 
                                       FROM REL-CLI-CODIGO (IND). 
               10  OUT-CNPJ-CLI        LINE LN COL 23
                                       PIC ZZ.ZZZ.ZZZ/ZZZZBZZ 
                                       FROM REL-CLI-CNPJ (IND). 
               10  OUT-RAZAO-SOCIAL-CLI  
                                       LINE LN COL 44
                                       PIC X(40) 
                                       FROM REL-CLI-RAZAO-SOCIAL (IND). 
               10  OUT-LATITUDE-CLI    LINE LN COL 87
                                       PIC ZZZ,ZZZZZZZZ 
                                       FROM REL-CLI-LATITUDE (IND).
               10  OUT-LONGITUDE-CLI   LINE LN COL 102
                                       PIC ZZZ,ZZZZZZZZ 
                                       FROM REL-CLI-LONGITUDE (IND).
      *
      * --> RELATORIO DE VENDEDORES
      *
       01 REL-VENDEDORES.
           05  CABECALHO-VEN.
               10  VALUE "RELATORIO DE VENDEDORES"     LINE 1 COL 50.
               10  VALUE "CODIGO"                      LINE 3 COL 13.
               10  VALUE "CPF"                         LINE 3 COL 21.
               10  VALUE "NOME"                        LINE 3 COL 38.
               10  VALUE "LATITUDE"                    LINE 3 COL 81.
               10  VALUE "LONGITUDE"                   LINE 3 COL 96.
           05  CONTEUDO-VEN.
               10  OUT-COD-VEN         LINE LN COL 13
                                       PIC ZZZ 
                                       FROM REL-VEN-CODIGO(IND). 
               10  OUT-CPF-VEN         LINE LN COL 21
                                       PIC ZZZ.ZZZ.ZZZBZZ 
                                       FROM REL-VEN-CPF(IND). 
               10  OUT-NOME-VEN        LINE LN COL 38
                                       PIC X(40) 
                                       FROM REL-VEN-NOME(IND).          
               10  OUT-LATITUDE-VEN    LINE LN COL 81
                                       PIC ZZZ,ZZZZZZZZ 
                                       FROM REL-VEN-LATITUDE(IND).
               10  OUT-LONGITUDE-VEN   LINE LN COL 96
                                       PIC ZZZ,ZZZZZZZZ 
                                       FROM REL-VEN-LONGITUDE(IND).
      * 
       01 CLEAR-SCREEN.
           05  CLEAR                   BLANK SCREEN.
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
      *
           MOVE SPACES                 TO WS-STATUS
           PERFORM UNTIL 1 <> 1
               DISPLAY SCR-MENU-PRINCIPAL
               DISPLAY OPCAO-STATUS
               ACCEPT OPCAO-PRINC
               EVALUATE WS-OPCAO
                   WHEN "01.01"
                       PERFORM 10000-MENU-CADASTRO-CLIENTE
                   WHEN "01.02"
                       PERFORM 20000-MENU-CADASTRO-VENDEDOR
                   WHEN "02.01"
                       PERFORM 30000-MOSTRAR-RELATORIO-CLIENTE
                   WHEN "02.02"
                       PERFORM 40000-MOSTRAR-RELATORIO-VENDEDOR
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
      *
           MOVE SPACES                 TO WS-STATUS
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
      *
           INITIALIZE REG-CLIENTE      REPLACING NUMERIC BY ZEROS
                                       ALPHANUMERIC BY SPACES
           MOVE SPACES                 TO WS-STATUS
           PERFORM UNTIL 1 <> 1
               DISPLAY SCR-INCLUIR-CLIENTE
               ACCEPT SCR-COD-CLI
               ACCEPT SCR-CNPJ-CLI
               ACCEPT SCR-RAZAO-SOCIAL-CLI
               ACCEPT SCR-LATITUDE-CLI
               ACCEPT SCR-LONGITUDE-CLI
               ACCEPT OPCAO-INC-CLI
               EVALUATE WS-OPCAO-INC-CLI
                   WHEN "S"
                       PERFORM 11100-ABRIR-ARQUIVO-CLIENTES
                       PERFORM 11200-PESQUISAR-CLIENTE
                       IF FS-CLI-NAO-ENCONTRADO
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
            END-PERFORM
           .
       11000-FIM. EXIT.    
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       11100-ABRIR-ARQUIVO-CLIENTES     SECTION.
      *----------------------------------------------------------------*
      *
           OPEN I-O ARQ-CLIENTES
           IF NOT FS-CLI-OK
               PERFORM 99991-ERRO-ARQUIVO-CLIENTES
           END-IF
           .
      *     
       11100-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       11200-PESQUISAR-CLIENTE      SECTION.
      *----------------------------------------------------------------*
      *
           READ ARQ-CLIENTES           INTO REG-CLIENTE
           IF  NOT FS-CLI-OK AND NOT FS-CLI-NAO-ENCONTRADO 
               AND NOT FS-CLI-FIM
               PERFORM 99991-ERRO-ARQUIVO-CLIENTES
           END-IF
           .
       11200-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       11300-GRAVAR-ARQUIVO-CLIENTES   SECTION.
      *----------------------------------------------------------------*
      *
           WRITE REG-CLIENTE
           IF NOT FS-CLI-OK
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
      *
           CLOSE ARQ-CLIENTES
           IF NOT FS-CLI-OK
               PERFORM 99991-ERRO-ARQUIVO-CLIENTES
           END-IF
           .
       11400-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       12000-ALTERAR-CLIENTE            SECTION.
      *----------------------------------------------------------------*
      *
           MOVE SPACES                 TO WS-STATUS
           PERFORM 11100-ABRIR-ARQUIVO-CLIENTES
           DISPLAY SCR-ALTERAR-CLIENTE
           ACCEPT SCR-CNPJ-ALT-CLI
           PERFORM 11200-PESQUISAR-CLIENTE
           IF FS-CLI-NAO-ENCONTRADO
               MOVE "CLIENTE NAO ENCONTRADO"
                                       TO WS-STATUS
               PERFORM 11400-FECHAR-ARQUIVO-CLIENTES
               PERFORM 10000-MENU-CADASTRO-CLIENTE
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
                           PERFORM 12100-ALTERAR-ARQUIVO-CLIENTES
                           PERFORM 11400-FECHAR-ARQUIVO-CLIENTES
                           PERFORM 10000-MENU-CADASTRO-CLIENTE
                       WHEN "v"
                       WHEN "V"
                           PERFORM 11400-FECHAR-ARQUIVO-CLIENTES
                           PERFORM 10000-MENU-CADASTRO-CLIENTE
                       WHEN OTHER
                           MOVE "OPCAO INVALIDA"   
                                               TO WS-STATUS
                   END-EVALUATE
               END-PERFORM
           END-IF
           .
       12000-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       12100-ALTERAR-ARQUIVO-CLIENTES  SECTION.                        
      *----------------------------------------------------------------*
      *
           REWRITE REG-CLIENTE
           IF NOT FS-CLI-OK
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
      *
           MOVE SPACES                 TO WS-STATUS
           PERFORM 11100-ABRIR-ARQUIVO-CLIENTES
           DISPLAY SCR-ALTERAR-CLIENTE
           ACCEPT SCR-CNPJ-ALT-CLI
           PERFORM 11200-PESQUISAR-CLIENTE
           IF FS-CLI-NAO-ENCONTRADO
               MOVE "CLIENTE NAO ENCONTRADO"
                                       TO WS-STATUS
           ELSE
               PERFORM 13100-EXCLUIR-ARQUIVO-CLIENTES
           END-IF
      *     
           PERFORM 11400-FECHAR-ARQUIVO-CLIENTES
           PERFORM 10000-MENU-CADASTRO-CLIENTE
           .
       13000-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       13100-EXCLUIR-ARQUIVO-CLIENTES  SECTION.                         
      *----------------------------------------------------------------*
      *
           DELETE ARQ-CLIENTES
           IF NOT FS-CLI-OK
               PERFORM 99991-ERRO-ARQUIVO-CLIENTES
           ELSE
               MOVE "CLIENTE REMOVIDO COM SUCESSO"
                                       TO WS-STATUS
           END-IF
           .
       13100-FIM. EXIT.  
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       20000-MENU-CADASTRO-VENDEDOR    SECTION.                        
      *----------------------------------------------------------------*
      *
           MOVE SPACES                 TO WS-STATUS
           PERFORM UNTIL 1 <> 1
               DISPLAY SCR-MENU-VENDEDORES
               DISPLAY OPCAO-STATUS
               ACCEPT OPCAO-VEN
               EVALUATE WS-OPCAO-VEN
                   WHEN "1"
                       MOVE SPACES     TO WS-STATUS
                       PERFORM 21000-INCLUIR-VENDEDOR 
                   WHEN "2"
                       MOVE SPACES     TO WS-STATUS
                       PERFORM 22000-ALTERAR-VENDEDOR
                   WHEN "3"
                       MOVE SPACES     TO WS-STATUS
                       PERFORM 23000-EXCLUIR-VENDEDOR
                   WHEN "m"
                   WHEN "M"
                       PERFORM 00000-MENU-PRINCIPAL
                   WHEN OTHER
                       MOVE "OPCAO INVALIDA"   
                                       TO WS-STATUS
               END-EVALUATE
           END-PERFORM
           .
       20000-FIM. EXIT.    
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       21000-INCLUIR-VENDEDOR          SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE REG-VENDEDOR     REPLACING NUMERIC BY ZEROS
                                       ALPHANUMERIC BY SPACES
           PERFORM UNTIL 1 <> 1
               DISPLAY SCR-INCLUIR-VENDEDOR
               ACCEPT SCR-COD-VEN
               ACCEPT SCR-CPF-VEN
               ACCEPT SCR-NOME-VEN
               ACCEPT SCR-LATITUDE-VEN
               ACCEPT SCR-LONGITUDE-VEN
               ACCEPT OPCAO-INC-VEN
               EVALUATE WS-OPCAO-INC-VEN
                   WHEN "S"
                       PERFORM 21100-ABRIR-ARQUIVO-VENDEDORES
                       PERFORM 21200-PESQUISAR-VENDEDOR
                       IF FS-VEN-NAO-ENCONTRADO
                           PERFORM 21300-GRAVAR-ARQUIVO-VENDEDORES
                       ELSE
                           MOVE "CPF JA CADASTRADO"       
                                           TO WS-STATUS
                       END-IF
                       PERFORM 21400-FECHAR-ARQUIVO-VENDEDORES
                       PERFORM 20000-MENU-CADASTRO-VENDEDOR
                   WHEN "V"
                       PERFORM 20000-MENU-CADASTRO-VENDEDOR
                   WHEN OTHER
                       MOVE "OPCAO INVALIDA"   
                                           TO WS-STATUS
               END-EVALUATE
            END-PERFORM
           .
       21000-FIM. EXIT.    
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       21100-ABRIR-ARQUIVO-VENDEDORES  SECTION.
      *----------------------------------------------------------------*
      *
           OPEN I-O ARQ-VENDEDORES
           IF NOT FS-VEN-OK
               PERFORM 99992-ERRO-ARQUIVO-VENDEDORES
           END-IF
           .
       21100-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       21200-PESQUISAR-VENDEDOR        SECTION.
      *----------------------------------------------------------------*
      *
           READ ARQ-VENDEDORES         INTO REG-VENDEDOR       
           IF NOT FS-VEN-OK AND NOT FS-VEN-NAO-ENCONTRADO
             AND NOT FS-VEN-FIM
               PERFORM 99992-ERRO-ARQUIVO-VENDEDORES
           END-IF
           .
       21200-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       21300-GRAVAR-ARQUIVO-VENDEDORES SECTION.
      *----------------------------------------------------------------*
      *
           WRITE REG-VENDEDOR
           IF NOT FS-VEN-OK
               PERFORM 99992-ERRO-ARQUIVO-VENDEDORES                    
           ELSE
               MOVE "VENDEDOR CADASTRADO COM SUCESSO"
                                       TO WS-STATUS
           END-IF
           .
       21300-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       21400-FECHAR-ARQUIVO-VENDEDORES SECTION.                       
      *----------------------------------------------------------------*
      *
           CLOSE ARQ-VENDEDORES
           IF NOT FS-VEN-OK
               PERFORM 99992-ERRO-ARQUIVO-VENDEDORES                    
           END-IF
           .
       21400-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       22000-ALTERAR-VENDEDOR          SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 21100-ABRIR-ARQUIVO-VENDEDORES
           DISPLAY SCR-ALTERAR-VENDEDOR
           ACCEPT SCR-CPF-ALT-VEN
           PERFORM 21200-PESQUISAR-VENDEDOR
           IF FS-VEN-NAO-ENCONTRADO
               MOVE "VENDEDOR NAO ENCONTRADO"
                                       TO WS-STATUS
               PERFORM 21400-FECHAR-ARQUIVO-VENDEDORES
               PERFORM 20000-MENU-CADASTRO-VENDEDOR
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
                           PERFORM 22100-ALTERAR-ARQUIVO-VENDEDORES
                           PERFORM 21400-FECHAR-ARQUIVO-VENDEDORES      
                           PERFORM 20000-MENU-CADASTRO-VENDEDOR
                       WHEN "v"
                       WHEN "V"
                           PERFORM 21400-FECHAR-ARQUIVO-VENDEDORES      
                           PERFORM 20000-MENU-CADASTRO-VENDEDOR
                       WHEN OTHER
                           MOVE "OPCAO INVALIDA"   
                                               TO WS-STATUS
                   END-EVALUATE
               END-PERFORM
           END-IF
           .
       22000-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       22100-ALTERAR-ARQUIVO-VENDEDORES SECTION.                       
      *----------------------------------------------------------------*
      *
           REWRITE REG-VENDEDOR
           IF NOT FS-VEN-OK
               PERFORM 99992-ERRO-ARQUIVO-VENDEDORES
           ELSE
               MOVE "VENDEDOR ALTERADO COM SUCESSO"
                                       TO WS-STATUS
           END-IF
           .
       22100-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       23000-EXCLUIR-VENDEDOR          SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 21100-ABRIR-ARQUIVO-VENDEDORES
           DISPLAY SCR-ALTERAR-VENDEDOR
           ACCEPT SCR-CPF-ALT-VEN
           PERFORM 21200-PESQUISAR-VENDEDOR
           IF FS-VEN-NAO-ENCONTRADO
               MOVE "VENDEDOR NAO ENCONTRADO"
                                       TO WS-STATUS
           ELSE
               PERFORM 23100-EXCLUIR-ARQUIVO-VENDEDORES
           END-IF
      *     
           PERFORM 21400-FECHAR-ARQUIVO-VENDEDORES
           PERFORM 20000-MENU-CADASTRO-VENDEDOR
           .
       23000-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       23100-EXCLUIR-ARQUIVO-VENDEDORES SECTION.                       
      *----------------------------------------------------------------*
      *
           DELETE ARQ-VENDEDORES
           IF NOT FS-VEN-OK
               PERFORM 99992-ERRO-ARQUIVO-VENDEDORES
           ELSE
               MOVE "VENDEDOR REMOVIDO COM SUCESSO"
                                       TO WS-STATUS
           END-IF
           .
       23100-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                 
      *----------------------------------------------------------------*
       30000-MOSTRAR-RELATORIO-CLIENTE SECTION.                      
      *----------------------------------------------------------------*
      *
           INITIALIZE REG-CLIENTE      REPLACING NUMERIC BY ZEROS
                                       ALPHANUMERIC BY SPACES
           MOVE 1          TO IND
           MOVE 5          TO LN
           DISPLAY CLEAR-SCREEN
           PERFORM 11100-ABRIR-ARQUIVO-CLIENTES
           PERFORM 31000-LER-ARQUIVO-CLIENTES
           PERFORM 32000-FORMATAR-RELATORIO-CLIENTE
                                       UNTIL WS-FS-CLIENTE = "10"
           PERFORM 11400-FECHAR-ARQUIVO-CLIENTES
           ACCEPT WS-OPCAO
           .
       30000-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       31000-LER-ARQUIVO-CLIENTES      SECTION.                       
      *----------------------------------------------------------------*
      *
           READ ARQ-CLIENTES           NEXT       
           IF NOT FS-CLI-OK AND NOT FS-CLI-FIM
               PERFORM 99991-ERRO-ARQUIVO-CLIENTES
           END-IF
           .
       31000-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       32000-FORMATAR-RELATORIO-CLIENTE SECTION.                        
      *----------------------------------------------------------------*
      *
           MOVE REG-CLIENTE            TO REL-INFO-CLIENTE (IND)
           DISPLAY REL-CLIENTES
           ADD 1                       TO IND
           ADD 1                       TO LN
           PERFORM 31000-LER-ARQUIVO-CLIENTES                           
           .
       32000-FIM. EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *                                                                 
      *----------------------------------------------------------------*
       40000-MOSTRAR-RELATORIO-VENDEDOR SECTION.                      
      *----------------------------------------------------------------*
      *
           INITIALIZE REG-VENDEDOR     REPLACING NUMERIC BY ZEROS
                                       ALPHANUMERIC BY SPACES
           MOVE 1                      TO IND
           MOVE 5                      TO LN
           DISPLAY CLEAR-SCREEN
           PERFORM 21100-ABRIR-ARQUIVO-VENDEDORES
           PERFORM 41000-LER-ARQUIVO-VENDEDORES
           PERFORM 42000-FORMATAR-RELATORIO-VENDEDOR
                                       UNTIL WS-FS-VENDEDOR = "10"
           PERFORM 21400-FECHAR-ARQUIVO-VENDEDORES
           ACCEPT WS-OPCAO
           .
       40000-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       41000-LER-ARQUIVO-VENDEDORES    SECTION.                       
      *----------------------------------------------------------------*
      *
           READ ARQ-VENDEDORES           NEXT       
           IF NOT FS-VEN-OK AND NOT FS-VEN-FIM
               PERFORM 99992-ERRO-ARQUIVO-VENDEDORES
           END-IF
           .
       41000-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       42000-FORMATAR-RELATORIO-VENDEDOR SECTION.                       
      *----------------------------------------------------------------*
      *
           MOVE REG-VENDEDOR           TO REL-INFO-VENDEDOR(IND)
           DISPLAY REL-VENDEDORES
           ADD 1                       TO IND
           ADD 1                       TO LN
           PERFORM 41000-LER-ARQUIVO-VENDEDORES                         
           .
       42000-FIM. EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       99991-ERRO-ARQUIVO-CLIENTES     SECTION.                
      *----------------------------------------------------------------*
      *
           IF FS-CLI-ERRO-LAYOUT    
               MOVE "ERRO NO LAYOUT DO ARQUIVO"    TO WS-STATUS
           ELSE IF FS-CLI-CANCELA
               MOVE "ERRO NO ACESSO AO ARQUIVO"    TO WS-STATUS
           END-IF
      *     
           PERFORM 11400-FECHAR-ARQUIVO-CLIENTES
           PERFORM 00000-MENU-PRINCIPAL
           .
       99991-FIM. EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       99992-ERRO-ARQUIVO-VENDEDORES     SECTION.                
      *----------------------------------------------------------------*
      *
           IF FS-VEN-ERRO-LAYOUT    
               MOVE "ERRO NO LAYOUT DO ARQUIVO"    TO WS-STATUS
           ELSE IF FS-VEN-CANCELA
               MOVE "ERRO NO ACESSO AO ARQUIVO"    TO WS-STATUS
           END-IF
      *     
           PERFORM 21400-FECHAR-ARQUIVO-VENDEDORES
           PERFORM 00000-MENU-PRINCIPAL
           .
       99992-FIM. EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
       END PROGRAM.
      *----------------------------------------------------------------*