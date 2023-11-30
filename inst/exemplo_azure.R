
tjsp::tjsp_baixar_cjpg("dano moral", paginas= 1, diretorio = "data-raw")

cjpg <- tjsp::tjsp_ler_cjpg(diretorio = "data-raw")

perguntas <- c(
  "Faça um breve resumo da decisão judicial",
  "Qual o nome do requerente? Coloque apenas o nome",
  "O requerente é pessoa física ou jurídica? Retorne pf para pessoa física ou pj para pessoa jurídica",
  "Qual o nome do requerido? Coloque apenas o nome",
  "O requerido é pessoa física ou jurídica? Retorne pf para pessoa física ou pj para pessoa jurídica",
  "A decisão foi procedente, parcialmente procedente ou improcedente? Responda apenas procedente, parcial ou improcedente",
  "Se foi procedente ou parcialmente procedente, houve condenação por dano moral? responda sim ou não",
  "Se houve condenação por dano moral, qual foi o valor em reais?"
)

colunas <- c("resumo","requerente","pessoa_requerente","requerido","pessoa_requerido","merito","dano_moral","valor")

resultado <- azure_openai_extrair(x = cjpg$julgado[1],
                                  perguntas = perguntas,
                                  artigo = "a",
                                  colunas = colunas,
                                  api_key = api_key,
                                  recurso = "openai-jurimetria",
                                  implementacao = "Jurimetria-gpt35-16k")

jqr::jq(resultado)