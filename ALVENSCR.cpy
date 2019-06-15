      ******************************************************************
      * COPYBOOK.: ALVENSCR                                            *
      * AUTOR....: SAULO MARIO DE MOURA                                *
      * DATA.....: 14/06/2019                                          *
      * TELA DO FORMULARIO DE ALTERACAO DE VENDEDOR                    *
      ******************************************************************
       01  SCR-ALTERAR-VENDEDOR.
           05 
       ALTERAR-VENDEDOR-CABECALHO.
               10  VALUE "CADASTRO DE VENDEDORES"           
                                       BLANK SCREEN    LINE 1 COL 50.
           05  CNPJ-BUSCA-CLIENTE.
               10  VALUE "DIGITE O CPF DO VENDEDOR"    LINE 5 COL 10.
               10  SCR-CPF-ALT-VEN                     LINE 5 COL 35
                                       PIC 999.999.999B99
                                       TO CPF.