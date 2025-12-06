install.packages("rdracor")
# install.packages("remotes")
remotes::install_github("dracor-org/rdracor")
library(rdracor)
get_dracor_api_url()
#> [1] "https://dracor.org/api/v1"
set_dracor_api_url("https://staging.dracor.org/api/v1")
corpora <- get_dracor_meta()
summary(corpora)
#> DraCor hosts 17 corpora comprising 3184 plays.
#> 
#> The last updated corpus was German Drama Corpus (2024-04-03 10:50:28).
plot(corpora)
shake <- get_dracor(corpus = "shake")
summary(shake)
#> 697 plays in German Drama Corpus 
#> Corpus id: ger, repository: https://github.com/dracor-org/gerdracor  
#> Description: Edited by Frank Fischer and Peer Trilcke. Features more than 600 German-language plays from the 1540s to the 1940s. For a corpus description and full credits please see the [README on GitHub](https://github.com/dracor-org/gerdracor).
#> Written years (range): 1549–1947 
#> Premiere years (range): 1515–1981    
#> Years of the first printing (range): 1517–1962
get_play_metadata(play = "hamlet", 
                  corpus = "shake",
                  full_metadata = FALSE) #use full_metadata = FALSE for faster download 
#> $id
#> [1] "ger000088"
#> 
#> $uri
#> [1] "https://dracor.org/api/v1/corpora/ger/plays/lessing-emilia-galotti"
#> 
#> $name
#> [1] "lessing-emilia-galotti"
#> 
#> $corpus
#> [1] "ger"
#> 
#> $title
#> [1] "Emilia Galotti"
#> 
#> $authors
#> # A tibble: 1 × 4
#>   name                      fullname                 shortname refs        
#>   <chr>                     <chr>                    <chr>     <list>      
#> 1 Lessing, Gotthold Ephraim Gotthold Ephraim Lessing Lessing   <df [2 × 2]>
#> 
#> $normalizedGenre
#> [1] "Tragedy"
#> 
#> $libretto
#> [1] FALSE
#> 
#> $allInSegment
#> [1] 30
#> 
#> $allInIndex
#> [1] 0.6976744
#> 
#> $characters
#> # A tibble: 13 × 12
#>    id         name  isGroup gender numOfScenes numOfSpeechActs numOfWords degree
#>    <chr>      <chr> <lgl>   <chr>        <int>           <int>      <int>  <int>
#>  1 der_prinz  Der … FALSE   MALE            17             157       4002      8
#>  2 der_kamme… Der … FALSE   MALE             2               6         33      1
#>  3 conti      Conti FALSE   MALE             2              24        604      1
#>  4 marinelli  Mari… FALSE   MALE            19             221       4343      9
#>  5 camillo_r… Cami… FALSE   MALE             1               6         78      1
#>  6 claudia    Clau… FALSE   FEMALE          13              73       1581      7
#>  7 pirro      Pirro FALSE   MALE             4              25        263      5
#>  8 odoardo    Odoa… FALSE   MALE            12             108       2441      6
#>  9 angelo     Ange… FALSE   MALE             2              28        487      2
#> 10 emilia     Emil… FALSE   FEMALE           7              64       1702      6
#> 11 appiani    Appi… FALSE   MALE             5              48        852      4
#> 12 battista   Batt… FALSE   MALE             4              11        152      4
#> 13 orsina     Orsi… FALSE   FEMALE           6              64       2111      4
#> # ℹ 4 more variables: weightedDegree <int>, closeness <dbl>, betweenness <dbl>,
#> #   eigenvector <dbl>
#> 
#> $segments
#> # A tibble: 43 × 4
#>    type  number title                              speakers 
#>    <chr>  <int> <chr>                              <list>   
#>  1 scene      1 Erster Aufzug | Erster Auftritt    <chr [2]>
#>  2 scene      2 Erster Aufzug | Zweiter Auftritt   <chr [2]>
#>  3 scene      3 Erster Aufzug | Dritter Auftritt   <chr [1]>
#>  4 scene      4 Erster Aufzug | Vierter Auftritt   <chr [2]>
#>  5 scene      5 Erster Aufzug | Fünfter Auftritt   <chr [1]>
#>  6 scene      6 Erster Aufzug | Sechster Auftritt  <chr [2]>
#>  7 scene      7 Erster Aufzug | Siebenter Auftritt <chr [2]>
#>  8 scene      8 Erster Aufzug | Achter Auftritt    <chr [2]>
#>  9 scene      9 Zweiter Aufzug | Erster Auftritt   <chr [2]>
#> 10 scene     10 Zweiter Aufzug | Zweiter Auftritt  <chr [2]>
#> # ℹ 33 more rows
#> 
#> $yearWritten
#> NULL
#> 
#> $yearPremiered
#> [1] "1772"
#> 
#> $yearPrinted
#> [1] "1772"
#> 
#> $yearNormalized
#> [1] 1772
#> 
#> $wikidataId
#> [1] "Q782653"
#> 
#> $subtitle
#> [1] "Ein Trauerspiel in fünf Aufzügen"
#> 
#> $relations
#> # A tibble: 4 × 4
#>   directed type            source       target   
#>   <lgl>    <chr>           <chr>        <chr>    
#> 1 TRUE     parent_of       odoardo      emilia   
#> 2 TRUE     parent_of       claudia      emilia   
#> 3 TRUE     associated_with marinelli    der_prinz
#> 4 TRUE     associated_with camillo_rota der_prinz
#> 
#> $source
#> $source$name
#> [1] "TextGrid Repository"
#> 
#> $source$url
#> [1] "http://www.textgridrep.org/textgrid:rksp.0"
#> 
#> 
#> $datePremiered
#> [1] "1772-03-13"
#> 
#> $originalSource
#> [1] "Gotthold Ephraim Lessing: Emilia Galotti. Ein Trauerspiel in fünf Aufzügen. In: Werke. Zweiter Band. Herausgegeben von Herbert G. Göpfert. München: Hanser 1971, S. 127–204."
hamlet <- get_net_cooccur_igraph(play = "hamlet", corpus = "shake")
plot(hamlet)