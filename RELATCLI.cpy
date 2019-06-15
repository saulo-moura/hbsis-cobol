      ******************************************************************
      * COPYBOOK.: RELATVEN                                            *
      * AUTOR....: SAULO MARIO DE MOURA                                *
      * DATA.....: 14/06/2019                                          *
      * TELA DO RELATORIO DOS CLIENTES CADASTRADOS                     *
      ******************************************************************
       01 REL-CLIENTES.
           05  CABECALHO.
               10  VALUE "RELATORIO DE CLIENTES"       LINE 1 COL 50.
               10  VALUE "CODIGO"                      LINE 3 COL 13.
               10  VALUE "CNPJ"                        LINE 3 COL 23.
               10  VALUE "RAZAO SOCIAL"                LINE 3 COL 44.
               10  VALUE "LATITUDE"                    LINE 3 COL 87.
               10  VALUE "LONGITUDE"                   LINE 3 COL 102.
           05  CONTEUDO.
               10  OUT-COD-CLI                         LINE LN COL 13
                                       PIC ZZZZZZZ 
                                       FROM REL-CLI-CODIGO (IND). 
               10  OUT-CNPJ-CLI                        LINE LN COL 23
                                       PIC ZZ.ZZZ.ZZZ/ZZZZBZZ 
                                       FROM REL-CLI-CNPJ (IND). 
               10  OUT-RAZAO-SOCIAL-CLI  
                                                       LINE LN COL 44
                                       PIC X(40) 
                                       FROM REL-CLI-RAZAO-SOCIAL (IND). 
               10  OUT-LATITUDE-CLI                    LINE LN COL 87
                                       PIC ZZZ,ZZZZZZZZ 
                                       FROM REL-CLI-LATITUDE (IND).
               10  OUT-LONGITUDE-CLI                   LINE LN COL 102
                                       PIC ZZZ,ZZZZZZZZ 
                                       FROM REL-CLI-LONGITUDE (IND).
       01 SEM-DADOS.
           05 VALUE "NAO HA CLIENTES CADASTRADOS"      LINE 1 COL 
           50.
                                       