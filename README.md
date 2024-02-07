# Extrator Lattes XML
Código em linguagem R para extração de produção científica de currículos da plataforma Lattes em formato XML

Extrator de dados de produção científica utilizado para construção a seção "produção" do site do Grupo de Pesquisa Estudos Métricos em Informação da UNESP - Campus Marília/SP (https://www.marilia.unesp.br/GPEMI)

O script é composto por três códigos:

1) Execute o primeiro código e selecione os arquivos de curriculos lates em formato XML
- Este processo salvará, em seu diretório, um arquivo denominado "Producao.txt" contendo a produção bibliográfica de todos os currículos: artigos, trabalhos publicados em anais de evento, livros e capítulos de livro
- O arquivo é organizado em formato tabular com cabeçalhos: Título, Autores, Ano, Fonte, Tipologia e Link/DOI

2) O segundo código irá construir a rede de coautorias. Execute-o e selecione o arquivo "Producao" gerado anteriormente
- É necessário informar a frequência mínima de coautorias entre os autores - Sugere-se o valor 1 (visualização completa da rede)
- Execute "prod" para obter a tabela de frequencia de producao por autor

3) O tercerio código salva a rede em formato HTML em seu diretório.
