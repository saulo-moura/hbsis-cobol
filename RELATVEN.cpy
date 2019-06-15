      ******************************************************************
      * COPYBOOK.: RELATVEN                                            *
      * AUTOR....: SAULO MARIO DE MOURA                                *
      * DATA.....: 14/06/2019                                          *
      * TELA DO RELATORIO DOS VENDEDORES CADASTRADOS                   *
      ******************************************************************
       01 REL-VENDEDORES.
           05  CABECALHO-VEN.
              
        10  VALUE "RELATORIO DE VENDEDORES"            LINE 1 COL 50.
               10  VALUE "CODIGO"                      LINE 3 COL 13.
               10  VALUE "CPF"                         LINE 3 COL 21.
               10  VALUE "NOME"                        LINE 3 COL 38.
               10  VALUE "LATITUDE"                    LINE 3 COL 81.
               10  VALUE "LONGITUDE"                   LINE 3 COL 96.
           05  CONTEUDO-VEN.
               10  OUT-COD-VEN                         LINE LN COL 13
                                       PIC ZZZ 
                                       FROM REL-VEN-CODIGO(IND). 
               10  OUT-CPF-VEN                         LINE LN COL 21
                                       PIC ZZZ.ZZZ.ZZZBZZ 
                                       FROM REL-VEN-CPF(IND). 
               10  OUT-NOME-VEN                        LINE LN COL 38
                                       PIC X(40) 
                                       FROM REL-VEN-NOME(IND).          
               10  OUT-LATITUDE-VEN                    LINE LN COL 81
                                       PIC ZZZ,ZZZZZZZZ 
                                       FROM REL-VEN-LATITUDE(IND).
               10  OUT-LONGITUDE-VEN                   LINE LN COL 96
                                       PIC ZZZ,ZZZZZZZZ 
                                       FROM REL-VEN-LONGITUDE(IND).
       01 SEM-DADOS.
           05 VALUE "NAO HA VENDEDORES CADASTRADOS"    LINE 1 COL 50.