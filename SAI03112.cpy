      ******************************************************************
      * NOME BOOK : SAI03112
      * DESCRICAO : ARQUIVO DE SAIDA DE CLIENTES
      * TAMANHO   :  16 BYTES
      ************************* DADOS DE SAIDA *************************
      * COD-AGENCIA         : CODIGO DA AGENCIA
      * NUM-CONTA           : NUMERO DA CONTA
      * DAT-PAGTO           : DATA DO PAGAMENTO ==> dd.mm.aaaa
      ******************************************************************

          01 ARQSAI01-REGISTRO.
             03 ARQSAI01-COD-AGENCIA          PIC 9(03) VALUE ZEROS.
             03 ARQSAI01-NUM-CONTA            PIC 9(03) VALUE ZEROS.
             03 ARQSAI01-DAT-PAGTO            PIC X(10) VALUE SPACES.
