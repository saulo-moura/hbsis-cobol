      ******************************************************************
      * COPYBOOK.: INVENSCR                                            *
      * AUTOR....: SAULO MARIO DE MOURA                                *
      * DATA.....: 14/06/2019                                          *
      * TELA DO FORMULARIO DE CADASTRO DE VENDEDOR                     *
      ******************************************************************
       01  SCR-INCLUIR-VENDEDOR.
           05  INCLUIR-VENDEDOR-CABECALHO.
               10  VALUE "CADASTRO DE VENDEDORES"           
                                       BLANK SCREEN    LINE 1 COL 50.
           05  INCLUIR-VENDEDOR-DADOS.
               10  VALUE "CODIGO: "                    LINE 5 COL 10.
               10  SCR-COD-VEN                         LINE 5 COL 35
                                       PIC ZZZ  
                                       USING CODIGO.
               10  VALUE "CPF:"                        LINE 6 COL 10.
               10  SCR-CPF-VEN                         LINE 6 COL 35
                                       PIC 999.999.999B99
                                       USING CPF.
               10  VALUE "RAZAO SOCIAL:"               LINE 7 COL 10.
               10  SCR-NOME-VEN                        LINE 7 COL 35
                                       PIC X(40) 
                                       USING NOME.
               10  VALUE "LATITUDE:"                   LINE 8 COL 10.
               10  SCR-LATITUDE-VEN                    LINE 8 COL 35
                                       PIC 999,99999999
                                       USING LATITUDE.
               10  VALUE "LONGITUDE"                   LINE 9 COL 10.
               10  SCR-LONGITUDE-VEN                   LINE 9 COL 35 
                                       PIC 999,99999999
                                       USING LONGITUDE.
               10  VALUE "S - SALVAR"                  LINE 16 COL 10.
               10  VALUE "V - VOLTAR"                  LINE 17 COL 10. 
           05 ESCOLHA-INCLUIR-VENDEDOR.
               10  VALUE "DIGITE A OPCAO DESEJADA"     LINE 20 COL 10.
               10  OPCAO-INC-VEN                       LINE 20 COL 35
                                       PIC X       TO WS-OPCAO-INC-VEN.