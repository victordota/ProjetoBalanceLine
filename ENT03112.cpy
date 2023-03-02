      ******************************************************************
      * NOME BOOK : ENT03112
      * DESCRICAO : ARQUIVO DE CADASTRO DE CLIENTES
      * TAMANHO   :  56 BYTES
      ************************* DADOS DE ENTRADA ***********************
      * COD-AGENCIA        : CODIGO DA AGENCIA
      * NUM-CONTA          : NUMERO DA CONTA
      * NOM-CLIENTE        : NOME DO CLIENTE
      * DAT-EMPRE          : DATA DO EMPRESTIMO ==> dd.mm.aaaa
      ******************************************************************

          01 ARQENT01-REGISTRO.
             03 ARQENT01-COD-AGENCIA     PIC 9(03) VALUE 0.
             03 ARQENT01-NUM-CONTA       PIC 9(03) VALUE 0.
             03 ARQENT01-NOM-CLIENTE     PIC X(40) VALUE SPACES.
             03 ARQENT01-DAT-EMPRE       PIC X(10) VALUE SPACES.
