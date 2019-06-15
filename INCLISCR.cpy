      ******************************************************************
      * COPYBOOK.: INCLISCR                                            *
      * AUTOR....: SAULO MARIO DE MOURA                                *
      * DATA.....: 14/06/2019                                          *
      * TELA DO FORMULARIO DE CADASTRO DE CLIENTE                      *
      ******************************************************************
       01  SCR-INCLUIR-CLIENTE.
           05  INCLUIR-CLIENTE-CABECALHO.
               10  VALUE "CADASTRO DE CLIENTES"           
                                       BLANK SCREEN    LINE 1 COL 50.
           05  INCLUIR-CLIENTE-DADOS.
               10  VALUE "CODIGO: "                    LINE 5 COL 10.
               10  SCR-COD-CLI                         LINE 5 COL 35
                                       PIC ZZZZZZZ  
                                       USING CODIGO.
               10  VALUE "CNPJ:"                       LINE 6 COL 10.
               10  SCR-CNPJ-CLI                        LINE 6 COL 35
                                       PIC 99.999.999/9999B99
                                       USING CNPJ.
               10  VALUE "RAZAO SOCIAL:"                LINE 7 COL 10.
               10  SCR-RAZAO-SOCIAL-CLI                LINE 7 COL 35
                                       PIC X(40) 
                                       USING NOME.
               10  VALUE "LATITUDE:"                   LINE 8 COL 10.
               10  SCR-LATITUDE-CLI                    LINE 8 COL 35
                                       PIC 999,99999999
                                       USING LATITUDE.
               10  VALUE "LONGITUDE"                   LINE 9 COL 10.
               10  SCR-LONGITUDE-CLI                   LINE 9 COL 35 
                                       PIC 999,99999999
                                       USING LONGITUDE.
               10  VALUE "S - SALVAR"                  LINE 16 COL 10.
               10  VALUE "V - VOLTAR"                  LINE 17 COL 10. 
           05 ESCOLHA-INCLUIR-CLIENTE.
               10  VALUE "DIGITE A OPCAO DESEJADA"     LINE 20 COL 10.
               10  OPCAO-INC-CLI                       LINE 20 COL 35
                                       PIC X       TO WS-OPCAO-INC-CLI.