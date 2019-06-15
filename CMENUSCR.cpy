      ******************************************************************
      * COPYBOOK.: CMENUSCR                                            *
      * AUTOR....: SAULO MARIO DE MOURA                                *
      * DATA.....: 14/06/2019                                          *
      * TELA DO MENU DE CADASTRO DE CLIENTES                           *
      ******************************************************************
       01 SCR-MENU-CLIENTES.
           05  MENU-CLIENTES-CABECALHO.
               10  VALUE "CADASTRO DE CLIENTES"
                                       BLANK SCREEN    LINE 1 COL 50.
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