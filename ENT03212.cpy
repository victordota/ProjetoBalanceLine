      ******************************************************************
      * NOME BOOK : ENT03212
      * DESCRICAO : ARQUIVO DE MOVIMENTO DE CLIENTES
      * TAMANHO   :  12 BYTES
      ************************* DADOS DE ENTRADA ***********************
      * COD-AGENCIA           : CODIGO DA AGENCIA
      * NUM-CONTA             : NUMERO DA CONTA
      * DAT-PAGTO             : DATA DO PAGAMENTO ==> aaaammdd
      ******************************************************************

          01 ARQENT02-REGISTRO.
             03 ARQENT02-COD-AGENCIA     PIC S9(03) COMP-3 VALUE +0.
             03 ARQENT02-NUM-CONTA       PIC S9(03) COMP-3 VALUE +0.
             03 ARQENT02-DAT-PAGTO       PIC  9(08)        VALUE 0.
