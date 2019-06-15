      ******************************************************************
      * PROGRAMA.: REGICADA                                            *
      * AUTOR....: SAULO MARIO DE MOURA                                *
      * DATA.....: 14/06/2019                                          *
      * COPYBOOK DOS REGISTROS DE CADASTRO DA CARTEIRA DE CLIENTES     *
      ******************************************************************
      * 
       01  REGISTRO.
           05 CODIGO                   PIC 9(07).
           05 REDEFINES CODIGO.
              10 FILLER                PIC X(4).
              10 CODIGO-3              PIC 9(3).
           05 CNPJ                     PIC 9(14).
           05 REDEFINES CNPJ.
              10 CPF                   PIC 9(11).
              10 FILLER                PIC X(3).                        
           05 NOME                     PIC X(40).
           05 LATITUDE                 PIC S9(03)V9(08).
           05 LONGITUDE                PIC S9(03)V9(08).


