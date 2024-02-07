##########################################################

#INSTRUCOES:

#Código em linguagem R para extração de produção científica de currículos da plataforma Lattes em formato XML

#Extrator de dados de produção científica utilizado para construção a seção "produção" do site do Grupo de Pesquisa Estudos Métricos em Informação da UNESP - Campus Marília/SP (https://www.marilia.unesp.br/GPEMI)

#O script é composto por três códigos:
  
#  1) Execute o primeiro código e selecione os arquivos de curriculos lates em formato XML
#  - Este processo salvará, em seu diretório, um arquivo denominado "Producao.txt" contendo a produção bibliográfica de todos os currículos: artigos, trabalhos publicados em anais de evento, livros e capítulos de livro
#  - O arquivo é organizado em formato tabular com cabeçalhos: Título, Autores, Ano, Fonte, Tipologia e Link/DOI

#  2) O segundo código irá construir a rede de coautorias. Execute-o e selecione o arquivo "Producao" gerado anteriormente
#  - É necessário informar a frequência mínima de coautorias entre os autores - Sugere-se o valor 1 (visualização completa da rede)
#  - Execute "prod" para obter a tabela de frequencia de producao por autor

#  3) O tercerio código irá salvar a rede em formato HTML em seu diretório.

##########################################################


#CODIGO 1 - GERANDO ARQUIVO DE PRODUCOES


{
  
  #bibliotecas - instale-as se necessário
  
  library(xml2)
  library(tidyverse)
  library(stringr)
  library(stringi)
  library(dplyr)
  
  #Seleciona o curriculo xml
  
  xml_select<-choose.files()
  
  t<-as.vector(xml_select)
  
  #Variaveis vazias
  
  #titulos
  
  titulo_artigo<-character()
  titulo_trab_evento<-character()
  titulo_cap<-character()
  titulo_livro<-character()
  
  #fontes
  fonte<-character()
  fonte_trab_evento<-character()
  fonte_cap<-character()
  fonte_livro<-character()
  
  #anos
  ano<-character()
  ano_trab_evento<-character()
  ano_cap<-character()
  ano_livro<-character()
  
  #links e DOI
  
  link<-character()
  link_trab_evento<-character()
  link_cap<-character()
  link_livro<-character()
  
  #Loop para extração
  
  for (i in 1:length(t)){
    
    #EXTAÇÂO DOS TITULOS
    
    #extrai titulo artigo
    
    extract1 <- xml_text(xml_find_all(read_xml(t[i]), "//ARTIGO-PUBLICADO/DADOS-BASICOS-DO-ARTIGO/@TITULO-DO-ARTIGO"))
    
    titulo_artigo<-append(extract1, titulo_artigo)
    
    #extrai titulo trabalho evento
    
    extract2<- xml_text(xml_find_all(read_xml(t[i]), "//TRABALHO-EM-EVENTOS/DADOS-BASICOS-DO-TRABALHO/@TITULO-DO-TRABALHO"))
    
    titulo_trab_evento<-append(extract2, titulo_trab_evento)
    
    #extrai titulo capitulo livro
    
    extract3<- xml_text(xml_find_all(read_xml(t[i]), "//LIVROS-E-CAPITULOS/CAPITULOS-DE-LIVROS-PUBLICADOS/CAPITULO-DE-LIVRO-PUBLICADO/DADOS-BASICOS-DO-CAPITULO/@TITULO-DO-CAPITULO-DO-LIVRO"))
    
    titulo_cap<-append(extract3, titulo_cap)
    
    #extrai titulo livro
    
    extract4<-xml_text(xml_find_all(read_xml(t[i]), ".//LIVROS-E-CAPITULOS/LIVROS-PUBLICADOS-OU-ORGANIZADOS/LIVRO-PUBLICADO-OU-ORGANIZADO/DADOS-BASICOS-DO-LIVRO/@TITULO-DO-LIVRO"))
    
    titulo_livro<-append(extract4, titulo_livro)
    
    #EXTRAÇÃO DAS FONTES
    
    #Extrai fonte dos artigos
    
    extract5<-xml_text(xml_find_all(read_xml(t[i]), "//ARTIGO-PUBLICADO/DETALHAMENTO-DO-ARTIGO/@TITULO-DO-PERIODICO-OU-REVISTA"))
    
    fonte<-append(extract5, fonte)
    
    #Extrai fonte dos trabalho em eventos
    
    extract6<-xml_text(xml_find_all(read_xml(t[i]), "//TRABALHO-EM-EVENTOS/DETALHAMENTO-DO-TRABALHO/@TITULO-DOS-ANAIS-OU-PROCEEDINGS"))
    
    fonte_trab_evento<-append(extract6, fonte_trab_evento)
    
    #Extrai fonte dos capítulos de livros
    
    extract7<-xml_text(xml_find_all(read_xml(t[i]), "//LIVROS-E-CAPITULOS/CAPITULOS-DE-LIVROS-PUBLICADOS/CAPITULO-DE-LIVRO-PUBLICADO/DETALHAMENTO-DO-CAPITULO/@TITULO-DO-LIVRO"))
    
    fonte_cap<-append(extract7, fonte_cap)
    
    #Extrai fonte dos livros
    
    extract8<-xml_text(xml_find_all(read_xml(t[i]), ".//LIVROS-E-CAPITULOS/LIVROS-PUBLICADOS-OU-ORGANIZADOS/LIVRO-PUBLICADO-OU-ORGANIZADO/DETALHAMENTO-DO-LIVRO/@NOME-DA-EDITORA"))
    
    fonte_livro<-append(extract8, fonte_livro)
    
    #EXTRAÇÃO DOS ANOS DAS PUBLICAÇÕES
    
    #Extrai ano dos artigos
    
    extract9<- xml_text(xml_find_all(read_xml(t[i]), "//ARTIGO-PUBLICADO/DADOS-BASICOS-DO-ARTIGO/@ANO-DO-ARTIGO"))
    
    ano<-append(extract9, ano)
    
    #extrai ano dos trabalhos em eventos
    
    extract10<-xml_text(xml_find_all(read_xml(t[i]), "//TRABALHO-EM-EVENTOS/DADOS-BASICOS-DO-TRABALHO/@ANO-DO-TRABALHO"))
    
    ano_trab_evento<-append(extract10, ano_trab_evento)
    
    #extrai ano dos capitulos de livros
    
    extract11<-xml_text(xml_find_all(read_xml(t[i]), "//LIVROS-E-CAPITULOS/CAPITULOS-DE-LIVROS-PUBLICADOS/CAPITULO-DE-LIVRO-PUBLICADO/DADOS-BASICOS-DO-CAPITULO/@ANO"))
    
    ano_cap<-append(extract11, ano_cap)
    
    #extrai ano dos livros
    
    extract12<-xml_text(xml_find_all(read_xml(t[i]), ".//LIVROS-E-CAPITULOS/LIVROS-PUBLICADOS-OU-ORGANIZADOS/LIVRO-PUBLICADO-OU-ORGANIZADO/DADOS-BASICOS-DO-LIVRO/@ANO"))
    
    ano_livro<-append(extract12, ano_livro)
    
    #EXTRAÇÃO DOS LINKS e DOI
    
    #Extrai link ou DOI artigos
    
    extract13<-xml_text(xml_find_all(read_xml(t[i]), "//ARTIGO-PUBLICADO/DADOS-BASICOS-DO-ARTIGO/@DOI"))
    
    link<-append(extract13, link)
    
    #Extrai links ou DOI trabalho em eventos
    
    extract14<-xml_text(xml_find_all(read_xml(t[i]), "//TRABALHO-EM-EVENTOS/DADOS-BASICOS-DO-TRABALHO/@HOME-PAGE-DO-TRABALHO"))
    
    link_trab_evento<-append(extract14, link_trab_evento)
    
    #Extrai links ou DOI capitulo de livro
    
    extract15<-xml_text(xml_find_all(read_xml(t[i]), "//LIVROS-E-CAPITULOS/CAPITULOS-DE-LIVROS-PUBLICADOS/CAPITULO-DE-LIVRO-PUBLICADO/DADOS-BASICOS-DO-CAPITULO/@HOME-PAGE-DO-TRABALHO"))
    
    link_cap<-append(extract15, link_cap)
    
    #Extrai links livros
    
    extract16<-xml_text(xml_find_all(read_xml(t[i]), ".//LIVROS-E-CAPITULOS/LIVROS-PUBLICADOS-OU-ORGANIZADOS/LIVRO-PUBLICADO-OU-ORGANIZADO/DADOS-BASICOS-DO-LIVRO/@HOME-PAGE-DO-TRABALHO"))
    
    link_livro<-append(extract16, link_livro)
    
  }
  
  #extrações auxiliares
  
  aux<-character() #aux artigos
  aux_evento<-character() #aux trabalho evento
  aux_cap<-character() #aux capitulo livro
  aux_livro<-character() #aux livro
  
  
  #Loop de extraçao auxiliar
  
  for (i in 1:length(t)){
    
    #aux artigo
    
    extract_aux<-xml_find_all(read_xml(t[i]), ".//ARTIGO-PUBLICADO")
    
    aux<-append(extract_aux, aux)
    
    
    #aux trabalho em eventos
    
    extract_aux_evento<-xml_find_all(read_xml(t[i]), ".//TRABALHO-EM-EVENTOS")
    
    aux_evento<-append(extract_aux_evento, aux_evento)
    
    #aux capitulos
    
    extract_aux_cap<-xml_find_all(read_xml(t[i]), ".//LIVROS-E-CAPITULOS/CAPITULOS-DE-LIVROS-PUBLICADOS/CAPITULO-DE-LIVRO-PUBLICADO")
    
    aux_cap<-append(extract_aux_cap, aux_cap)
    
    #aux livros
    
    extract_aux_livro<-xml_find_all(read_xml(t[i]), ".//LIVROS-E-CAPITULOS/LIVROS-PUBLICADOS-OU-ORGANIZADOS/LIVRO-PUBLICADO-OU-ORGANIZADO")
    
    aux_livro<-append(extract_aux_livro, aux_livro)
    
  }
  
  
  #Extração dos nomes completos
  
  #Variaveis vazias
  
  autores_artigo<-character()
  autores_trab_evento<-character()
  autores_cap<-character()
  autores_livro<-character()
  
  for (i in 1:length(aux)) {
    
    # AUTORES de cada artigo 
    
    autores <- xml_find_all(aux[[i]], "./AUTORES")
    
    nome_autor <- xml_attr(autores, "NOME-COMPLETO-DO-AUTOR")
    
    nomes_artigo<-paste(nome_autor, collapse = "; ")
    
    autores_artigo<-append(nomes_artigo, autores_artigo)  
    
  }
  
  autores_artigo<-rev(autores_artigo)
  
  
  # AUTORES de cada trabalho em evento
  
  for (i in aux_evento){
    
    autores_evento <- xml_find_all(i, "./AUTORES")
    
    nome_autor_evento <- xml_attr(autores_evento, "NOME-COMPLETO-DO-AUTOR")
    
    nomes_evento<-paste(nome_autor_evento, collapse = "; ")
    
    autores_trab_evento<-append(nomes_evento, autores_trab_evento)  
    
  }
  
  autores_trab_evento<-rev(autores_trab_evento)
  
  # AUTORES de cada trabalho em Capitulo
  
  for (i in aux_cap){
    
    autores_cap_livro <- xml_find_all(i, "./AUTORES")
    
    nome_autor_cap <- xml_attr(autores_cap_livro, "NOME-COMPLETO-DO-AUTOR")
    
    nomes_cap<-paste(nome_autor_cap, collapse = "; ")
    
    autores_cap<-append(nomes_cap, autores_cap)  
    
  }
  
  autores_cap<-rev(autores_cap)
  
  # AUTORES de cada Livro
  
  for (i in aux_livro){
    
    aut_livro <- xml_find_all(i, "./AUTORES")
    
    nome_autor_livro <- xml_attr(aut_livro, "NOME-COMPLETO-DO-AUTOR")
    
    nomes_livro<-paste(nome_autor_livro, collapse = "; ")
    
    autores_livro<-append(nomes_livro, autores_livro)  
    
  }
  
  autores_livro<-rev(autores_livro)
  
  #Ajustes para construção do DF final
  
  if(length(autores_artigo)>0){
    
    df_artigo<-data.frame("Autores"= stri_trans_general(str_to_title(autores_artigo),"Latin-ASCII"), "Título"= stri_trans_general(str_to_title(titulo_artigo),"Latin-ASCII"), "Fonte"= fonte, "Ano"= ano, "Tipologia"="Artigo", "Link_DOI"= link)
    
  } else {
    
    df_artigo<-data.frame("Autores"="", "Título"="", "Fonte"="", "Ano"="", "Tipologia"="", "Link_DOI"="")}
  
  
  if(length(autores_trab_evento)>0){
    
    df_trab_evento<-data.frame("Autores"= stri_trans_general(str_to_title(autores_trab_evento),"Latin-ASCII"), "Título"= stri_trans_general(str_to_title(titulo_trab_evento),"Latin-ASCII"), "Fonte"= fonte_trab_evento, "Ano"= ano_trab_evento, "Tipologia"="Trabalho em eventos", "Link_DOI"= link_trab_evento)
    
  } else {
    
    df_trab_evento<-data.frame("Autores"="", "Título"="", "Fonte"="", "Ano"="", "Tipologia"="", "Link_DOI"="")}
  
  
  if(length(autores_cap)>0){
    
    df_capitulo<-data.frame("Autores"= stri_trans_general(str_to_title(autores_cap),"Latin-ASCII"), "Título"= stri_trans_general(str_to_title(titulo_cap),"Latin-ASCII"), "Fonte"= fonte_cap, "Ano"= ano_cap, "Tipologia"="Capítulo de livro", "Link_DOI"= link_cap)
    
  } else {
    
    df_capitulo<-data.frame("Autores"="", "Título"="", "Fonte"="", "Ano"="", "Tipologia"="", "Link_DOI"="")}
  
  
  if(length(autores_livro)>0){
    
    df_livro<-data.frame("Autores"= stri_trans_general(str_to_title(autores_livro),"Latin-ASCII"), "Título"= stri_trans_general(str_to_title(titulo_livro),"Latin-ASCII"), "Fonte"= fonte_livro, "Ano"= ano_livro, "Tipologia"="Livro", "Link_DOI"= link_livro)
    
  } else {
    
    df_livro<-data.frame("Autores"="", "Título"="", "Fonte"="", "Ano"="", "Tipologia"="", "Link_DOI"="")}
  
  
  #ORGANIZACAO FINAL
  
  df_final<-rbind(df_artigo, df_trab_evento, df_capitulo, df_livro)
  
  
  write.table(distinct(df_final), file="Producao.txt", row.names = FALSE, sep="\t", dec=",", col.names = TRUE) #Salva o arquivo
  
}


#CÓDIGO 2 - CONSTRUCAO DA REDE DE COAUTORIAS

#Execute "prod" para obter a tabela de frequencia de producao por autor


{
 
  #bibliotecas - instale-as se necessário
   
  library(dplyr)
  library(igraph)
  library(visNetwork)
  library(stringi)
  
  #leitura do arquivo
  
  corpus<-read.table(file.choose(), header = TRUE, sep = "\t", quote="\"")
  
  #Extração da coluna Autores
  
  base<-corpus$Autores 
  
  #Ajuste para remocao de acentos
  
  base<-stri_trans_general(base, "Latin-ASCII")
  
  base<-trimws(base)
  
  #split por virgula
  
  #producao
  
  autores <- unique(unlist(strsplit(base, "; ")))
  
  autores_prod <- (unlist(strsplit(base, "; ")))   #Autores
  
  prod<-as.data.frame(table(autores_prod)) #Producao por autor
  
  prod<-prod[order(prod$Freq,decreasing=TRUE),]
  
  #ajuste para contagem
  
  b4<-strsplit(as.character(base), split = "; " , fixed = FALSE)
  
  b5<-as.data.frame(do.call(cbind, b4))
  
  #Ajuste para stack
  
  b6<-stack(b5)
  
  #Matriz de coautoria
  
  mtx<-table(stack(b5))
  
  mtx[mtx>1]<-1
  
  mtx_coaut<-mtx%*%t(mtx) 
  
  diag(mtx_coaut)<-0
  
  #Rede igraph
  
  rede_coaut<-graph_from_adjacency_matrix(mtx_coaut, weighted = T, mode = "undirected")
  
  #rede visNetwork
  
  vis_coaut<-toVisNetworkData(rede_coaut)
  
  #organizacao da rede
  
  edges_aux<-readline("Digite o número mínimo (maior que zero) de frequência de coautoria entre dois autores:")
  
  node_coaut<-data.frame("id"=vis_coaut$nodes$id, "label"=vis_coaut$nodes$label)
  links_coaut<-as.data.frame(vis_coaut$edges) 
  colnames(links_coaut)[3]<-'width'
  links_coaut<-filter(links_coaut, width>=edges_aux)
  
  #construção da rede de coautorias
  
  if (edges_aux >= 2){
    
    id_aux<-unique(as.character(c(links_coaut$from, links_coaut$to)))
    node_coaut<-data.frame("id"=id_aux, "label"=id_aux)
    links_coaut<-filter(links_coaut, width>=edges_aux)
    
  }
  
  
  
  #VISUALIZA a rede de coautorias
  
  visNetwork(node_coaut, links_coaut) %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
    visIgraphLayout(layout = "layout_with_fr")
  
}

#CODIGO 3 - SALVAR A REDE EM HTML

visSave(graph=visNetwork(node_coaut, links_coaut) %>%
          visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
          visIgraphLayout(layout = "layout_with_fr"), "rede de coautorias.html")