      ******************************************************************
      * COPYBOOK.: PMENUSCR                                            *
      * AUTOR....: SAULO MARIO DE MOURA                                *
      * DATA.....: 14/06/2019                                          *
      * TELA DO MENU PRINCIPAL DO SISTEMA                              *
      ******************************************************************
       01  SCR-MENU-PRINCIPAL.
           05  MENU-PRINCIPAL-CABECALHO.
               
       10  VALUE "GERENCIAMENTO DE CARTELA DE CLIENTES" 
                                       BLANK SCREEN    LINE 1 COL 35.
           05  MENU-PRINCIPAL-OPCOES.
               10  VALUE "CADASTROS"                   LINE 5 COL 10.
               10  VALUE "01.01 - CADASTRO DE CLIENTE" LINE 6 COL 10.
               10  VALUE "01.02 - CADASTRO DE VENDEDOR"  
                                                       LINE 7 COL 10.
               10  VALUE "RELATORIOS"                  LINE 9 COL 10.
               10  VALUE "02.01 - RELATORIO DE CLIENTES" 
                                                       LINE 10 COL 10.
               10  VALUE "02.02 - RELATORIO DE VENDEDORES"
                                                       LINE 11 COL 10.
               10  VALUE "EXECUTAR"                    LINE 13 COL 10.
               10  VALUE "03.01 - EXECUTAR DISTRIBUICAO DE CLIENTES"   
                                                       LINE 14 COL 10.
               10  VALUE "S - SAIR"                    LINE 16 COL 10.
           05 ESCOLHA-MENU-PRINCIPAL.
               10  VALUE "DIGITE A OPCAO DESEJADA"     LINE 20 COL 10.
               10  OPCAO-PRINC                         LINE 20 COL 35
                                       PIC X(5)        TO WS-OPCAO.


