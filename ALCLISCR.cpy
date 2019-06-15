      ******************************************************************
      * COPYBOOK.: ALCLISCR                                            *
      * AUTOR....: SAULO MARIO DE MOURA                                *
      * DATA.....: 14/06/2019                                          *
      * TELA DO FORMULARIO DE ALTERACAO DE CLIENTE                     *
      ******************************************************************
       01  SCR-ALTERAR-CLIENTE.
           05 ALTERAR-CLIENTE-CABECALHO.
               
       10  VALUE "CADASTRO DE CLIENTES"           
                                       BLANK SCREEN    LINE 1 COL 50.
           05  CNPJ-BUSCA-CLIENTE.
               10  VALUE "DIGITE O CNPJ DO CLIENTE"    LINE 5 COL 10.
               10  SCR-CNPJ-ALT-CLI                    LINE 5 COL 35
                                       PIC 99.999.999/9999B99
                                       TO CNPJ.