{-|
Module       : Tarefa3
Description  : Compressão de um estado do jogo
Stability    : Experimental


O objetivo desta tarefa é implementar criar uma função que comprima o estado do jogo em uma @String@ e outra que reverta o processo, ou seja, que , a partir da @String@

resultante da codificação, volte a construir a exata lista de @String@ correspondente ao estado em que o jogo se encontrava. Quanto menor for a @String@ resultante da

compressão, melhor, dado que se poupa mais informação.

-}

module Main where
import System.Environment
import Text.Read
import Data.Maybe
import System.Random
import Data.List
import Data.Char

{- | A função @encode@ divide a tarefa de comprimir o estado do mapa em duas partes. A primeira  é de comprimir o mapa propriamente dito, a segunda é decomprimir a 

     informação das @Strings@ abaixo do mapa, relativa a power ups, jogadores e bombas.

     Outro pormenor da nossa função @encode@ é que guarda no início da @String@ a dimensão do mapa, a fim de ser mais fácil de trabalhar na descodificação desta @String@.-}
encode :: [String] -> String
encode l = (show d) ++ "." ++ (encodePadrao2 (encodePadrao (encodeBricks (removeVazios (removeCard (stringMapa (tiraLinhas (inserirPowerups l l)))) 0) ""))) ++ encodeInfo (stringInfo (tiraLinhas (inserirPowerups l l)))
        where d = (length (l !! 0))







{- | Como seria de esperar, a função @decode@ é fortemente influenciada pelo raciocínio e estratégia adotados na encode, dado que tem de, de certa forma, realizar

     o processo inverso. Assim, esta função também divide o seu papel em duas partes, a de construir o mapa e a de acrescentar por baixo a informação relativa a 

     power ups, bombas e jogadores.-}
decode :: String -> [String]
decode l = unirBombas ((constroiMapa (lilstring (fillMapaCard (gerarMapaCard d d) (addVazios (decodeBricks (decodePadrao (decodePadrao2 l3))))) d)) ++ (tail (unlineInfo (decodeInfo l) ""))) 0
          where d  = read (fst  (span (/= '.') l))
                l1 = tail (snd  (span (/= '.') l))
                l2 = fst        (span (/= '/') l1)
                l3 = fst        (span (/= '>') l2)





{- | A função main recebe como input um estado do jogo (geralmente importado dos exemplos na pasta de testes através do stdin) e um comando, -e para encode ou

-d para decode, que determinarão a função a utilizar na informação importada. -}
main :: IO ()
main = do a <- getArgs
          let p = a !! 0
          w <- getContents
          if length a == 1 && length p == 2 && (p=="-e" || p=="-d")
             then if p=="-e" then putStr $ encode $ lines w
                             else putStr $ unlines $ decode w
             else putStrLn "Parâmetros inválidos"





----------------------------------------------------------------ENCODE--------------------------------------------------------------------------------------

{- | O primeiro passo da compressão é a função @inserirPowerups@ que insere no mapa os powerups (com caracteres diferentes caso estejam por baixo de tijolos ou 

     não). Assim, não há a necessidade de ter as @Strings@ que se encontram depois do mapa, à exceção de alguma informação relativa a bombas e as @Strings@ dos 

     jogadores, que não inserimos no mapa por ser incómodo inserir dígitos no mapa, uma vez que utilizamos dígitos no nosso método de compressão.  -}
inserirPowerups :: [String] -> [String] -> [String]
inserirPowerups mapa [] = mapa
inserirPowerups mapa (h:t) 
      |elem (h!!0) "+!" && (mapa !! (pOrden h)) !! (pAbcis h) == '?'      = inserirPowerups ((take (pOrden h) mapa) ++ [(take (pAbcis h) (mapa !! (pOrden h))) ++ [(h!!0)] ++ (drop (pAbcis h + 1) (mapa !! pOrden h))] ++ (drop (pOrden h +1) mapa)) t
      |(h!!0) == '+'                                                      = inserirPowerups ((take (pOrden h) mapa) ++ [(take (pAbcis h) (mapa !! (pOrden h))) ++      "-" ++ (drop (pAbcis h + 1) (mapa !! pOrden h))] ++ (drop (pOrden h +1) mapa)) t
      |(h!!0) == '!'                                                      = inserirPowerups ((take (pOrden h) mapa) ++ [(take (pAbcis h) (mapa !! (pOrden h))) ++      ":" ++ (drop (pAbcis h + 1) (mapa !! pOrden h))] ++ (drop (pOrden h +1) mapa)) t
      |(h!!0) == '*'                                                      = inserirPowerups ((take (pOrden h) mapa) ++ [(take (pAbcis h) (mapa !! (pOrden h))) ++      "*" ++ (drop (pAbcis h + 1) (mapa !! pOrden h))] ++ (drop (pOrden h +1) mapa)) t
      |otherwise               = inserirPowerups mapa t













{- | A função @tiralinhas@ rempve as @Strings@ com informação relativa a powerups do mapa, e retira a informação relativa à posição das bombas. -}
tiraLinhas :: [String] -> String 
tiraLinhas [] = ""
tiraLinhas (h:t) 
      | h!!0 == '#'             =                                                                         h ++ tiraLinhas t 
      | elem (h!!0) "0123"      = "/"           ++                                                        h ++ tiraLinhas t
      | h!!0 == '*'             = ">"           ++ (drop (4 + length (pAbcisSt h) + length (pOrdenSt h)) h) ++ tiraLinhas t
      |otherwise                =                                                                              tiraLinhas t









{- | A função @stringinfo@ recolhe a informação que resta das bombas (em @strings@ que deixámos com o @Char@ '>' início) e a informação dos jogadores (em linhas 

     separadas por barras ('/') ). -}
stringInfo :: String -> String
stringInfo [] = []
stringInfo (h:t) =   if elem h ">/"           then (h:t)           else stringInfo t















{- | A função @stringMapa@, a partir de uma string com a informação do mapa e a restande das bombas e jogadores, devolve apenas a parte relaiva ao mapa, que é 

     processada por uma função diferente do resto.-}
stringMapa :: String -> String
stringMapa [] = []
stringMapa (h:t) = if elem h ">/" then [] else h:stringMapa t










{- | Esta função retira os cardinais da string relativa ao mapa, uma vez que estes estão sempre nos mesmos sítios e os podemos recriar nas mesmas posições.-}
removeCard :: String -> String
removeCard [] = []
removeCard (h:t) = if h == '#' then removeCard t else h: removeCard t







{- | Esta função substitui os chars ' ' por números que representam a quantidade de espaços seguidos que se encontram nessa posição da string.-}
removeVazios :: String -> Int -> String
removeVazios [] _ = ""
removeVazios (h:t) c 
     |h == ' '       =                     removeVazios t (c+1)
     |c == 0         =                   h:removeVazios t   0
     |otherwise      = (show c) ++ [h] ++  removeVazios t   0 














{- | Esta função procede da mesma forma que a @removeVazios@, mas para os tijolos (caracter '?') e substitui os aglomerados destes mesmos caracteres por letras

     (e.g. a=1, b=2, c=3 ...)-}
encodeBricks :: String -> String -> String
encodeBricks [] st = st
encodeBricks (h:t) st 
     |h == '?' && elem (last st) ['a'..'f']  = encodeBricks t ((take (length st -1) st) ++ [(succ (last st))])
     |h == '?'                               = encodeBricks t                      (st  ++ "a")
     |otherwise                              = encodeBricks t                      (st  ++ [h])












{- | A função @encodePadrão@, dada a string resultante da compressão já feita, reconhece padrões de dois caracteres. A esses padrões faz corresponder uma letra 

     maiúscula.-}
encodePadrao :: String -> String
encodePadrao [] = ""
encodePadrao [a] = [a]
encodePadrao (h:r:t) 
     |h=='1' && r=='a'      = 'A':encodePadrao t
     |h=='2' && r=='a'      = 'B':encodePadrao t
     |h=='3' && r=='a'      = 'C':encodePadrao t
     |h=='4' && r=='a'      = 'D':encodePadrao t
     |h=='5' && r=='a'      = 'E':encodePadrao t
     |h=='6' && r=='a'      = 'F':encodePadrao t
     |h=='1' && r=='b'      = 'G':encodePadrao t
     |h=='2' && r=='b'      = 'H':encodePadrao t
     |h=='3' && r=='b'      = 'I':encodePadrao t
     |h=='4' && r=='b'      = 'J':encodePadrao t
     |h=='5' && r=='b'      = 'K':encodePadrao t
     |h=='6' && r=='b'      = 'L':encodePadrao t
     |h=='1' && r=='c'      = 'M':encodePadrao t
     |h=='2' && r=='c'      = 'N':encodePadrao t
     |h=='3' && r=='c'      = 'O':encodePadrao t
     |h=='4' && r=='c'      = 'P':encodePadrao t
     |h=='5' && r=='c'      = 'Q':encodePadrao t
     |h=='6' && r=='c'      = 'R':encodePadrao t
     |h=='+' && r=='+'      = 'X':encodePadrao t
     |h=='!' && r=='!'      = 'Y':encodePadrao t
     |h=='+' && r=='!'      = 'W':encodePadrao t
     |h=='!' && r=='+'      = 'Z':encodePadrao t
     |otherwise             =   h:encodePadrao (r:t)













{- | A função @encodePadrao2@ procede das mesma forma que a @encodePadrao@, no entanto reconhece conjuntos de duas letras máiúsculas e substitui-os por um símbolo.-}
encodePadrao2 :: String -> String
encodePadrao2 [] = ""
encodePadrao2 [a] = [a]
encodePadrao2 (h:r:t)
      |h == 'A' && r == 'A' = '(':encodePadrao2 t
      |h == 'A' && r == 'B' = ')':encodePadrao2 t
      |h == 'A' && r == 'C' = '^':encodePadrao2 t
      |h == 'A' && r == 'D' = '~':encodePadrao2 t
      |h == 'B' && r == 'B' = '|':encodePadrao2 t
      |h == 'B' && r == 'A' = '%':encodePadrao2 t
      |h == 'B' && r == 'C' = '&':encodePadrao2 t
      |h == 'B' && r == 'D' = '=':encodePadrao2 t
      |h == 'C' && r == 'C' = '_':encodePadrao2 t
      |h == 'C' && r == 'A' = '[':encodePadrao2 t
      |h == 'C' && r == 'B' = ';':encodePadrao2 t
      |h == 'C' && r == 'D' = '@':encodePadrao2 t
      |h == 'D' && r == 'D' = '}':encodePadrao2 t
      |h == 'D' && r == 'A' = ']':encodePadrao2 t
      |h == 'D' && r == 'B' = '{':encodePadrao2 t
      |h == 'D' && r == 'C' = '"':encodePadrao2 t
      |otherwise            =   h:encodePadrao2 (r:t)










{- | A função @encodeInfo@ efetua um processo semelhante ao da @encodePadrao@ mas destinado e adaptado à informação relativa a bombas e jogadores que não foi 

     incluida no mapa. -}
encodeInfo :: String -> String
encodeInfo [] = []
encodeInfo [h] = [h]
encodeInfo (h:r:t)
      |[h,r] == "1 "       = 'a':encodeInfo t
      |[h,r] == "2 "       = 'b':encodeInfo t
      |[h,r] == "3 "       = 'c':encodeInfo t
      |[h,r] == "4 "       = 'd':encodeInfo t
      |[h,r] == "5 "       = 'e':encodeInfo t
      |[h,r] == "6 "       = 'f':encodeInfo t
      |[h,r] == "7 "       = 'g':encodeInfo t
      |[h,r] == "8 "       = 'h':encodeInfo t
      |[h,r] == "9 "       = 'i':encodeInfo t
      |[h,r] == "0 "       = 'j':encodeInfo t
      |otherwise           =   h:encodeInfo (r:t)

---------------------------------------------------------------DECODE----------------------------------------------------------------------------------------

{- | A função @decodeInfo@ faz a substituição inversa da @encodeInfo@.-}
decodeInfo :: String -> String
decodeInfo [] = []
decodeInfo (h:t)
      |h == 'a'          = "1 " ++ decodeInfo t 
      |h == 'b'          = "2 " ++ decodeInfo t 
      |h == 'c'          = "3 " ++ decodeInfo t 
      |h == 'd'          = "4 " ++ decodeInfo t 
      |h == 'e'          = "5 " ++ decodeInfo t 
      |h == 'f'          = "6 " ++ decodeInfo t 
      |h == 'g'          = "7 " ++ decodeInfo t 
      |h == 'h'          = "8 " ++ decodeInfo t 
      |h == 'i'          = "9 " ++ decodeInfo t 
      |h == 'j'          = "0 " ++ decodeInfo t 
      |otherwise         =       h:decodeInfo t













{- | A função @decodePadrao2@ efetua as substituições inversas da @encodePadrao2@-}
decodePadrao2 :: String -> String
decodePadrao2 [] = []
decodePadrao2 (h:t)
      |h == '('      = "AA" ++ decodePadrao2 t
      |h == ')'      = "AB" ++ decodePadrao2 t
      |h == '^'      = "AC" ++ decodePadrao2 t
      |h == '~'      = "AD" ++ decodePadrao2 t
      |h == '|'      = "BB" ++ decodePadrao2 t
      |h == '%'      = "BA" ++ decodePadrao2 t
      |h == '&'      = "BC" ++ decodePadrao2 t
      |h == '='      = "BD" ++ decodePadrao2 t
      |h == '_'      = "CC" ++ decodePadrao2 t
      |h == '['      = "CA" ++ decodePadrao2 t
      |h == ';'      = "CB" ++ decodePadrao2 t
      |h == '@'      = "CD" ++ decodePadrao2 t
      |h == '}'      = "DD" ++ decodePadrao2 t
      |h == ']'      = "DA" ++ decodePadrao2 t
      |h == '{'      = "DB" ++ decodePadrao2 t
      |h == '"'      = "DC" ++ decodePadrao2 t
      |otherwise     =       h:decodePadrao2 t













{- | A função @decodePadrao@ efetua as substituições inversas da @encodePadrao@.-}
decodePadrao :: String -> String 
decodePadrao [] = []
decodePadrao (h:t)
      |h == 'A'      = "1a" ++ decodePadrao t
      |h == 'B'      = "2a" ++ decodePadrao t
      |h == 'C'      = "3a" ++ decodePadrao t
      |h == 'D'      = "4a" ++ decodePadrao t
      |h == 'E'      = "5a" ++ decodePadrao t
      |h == 'F'      = "6a" ++ decodePadrao t
      |h == 'G'      = "1b" ++ decodePadrao t
      |h == 'H'      = "2b" ++ decodePadrao t
      |h == 'I'      = "3b" ++ decodePadrao t
      |h == 'J'      = "4b" ++ decodePadrao t
      |h == 'K'      = "5b" ++ decodePadrao t
      |h == 'L'      = "6b" ++ decodePadrao t
      |h == 'M'      = "1c" ++ decodePadrao t
      |h == 'N'      = "2c" ++ decodePadrao t
      |h == 'O'      = "3c" ++ decodePadrao t
      |h == 'P'      = "4c" ++ decodePadrao t
      |h == 'Q'      = "5c" ++ decodePadrao t
      |h == 'R'      = "6c" ++ decodePadrao t
      |h == 'X'      = "++" ++ decodePadrao t
      |h == 'Y'      = "!!" ++ decodePadrao t
      |h == 'W'      = "+!" ++ decodePadrao t
      |h == 'Z'      = "!+" ++ decodePadrao t
      |otherwise     =       h:decodePadrao t






{- | A função @decodeBricks@ substitui os as letras que encontra pela quantidade de tijolos a que corresponde essa letra.-}
decodeBricks :: String -> String
decodeBricks [] = []
decodeBricks (h:t) = if       elem h st         then         (replicate (howMuch h st) '?')   ++   decodeBricks t                   else h:decodeBricks t
             where st = ['a'..'z']












{- | Esta função substitui os dígitos na string pela quantidade correspondente de espaços seguidos.-}
addVazios :: String -> String 
addVazios [] = []
addVazios [h] = if elem h ['0'..'9']    then (replicate (read [h]) ' ') else [h]
addVazios (h:r:t) 
         |elem h ['0'..'9']  && elem r ['0'..'9']  = (replicate (read [h,r]) ' ')   ++ (addVazios   t  ) 
         |elem h ['0'..'9']                        = (replicate (read [h])   ' ')   ++ (addVazios (r:t))
         |otherwise                                =                                  h:addVazios (r:t)
         









{- | Esta função, a partir da dimensão do mapa (armazenada no inicio da string resultante da decode), gera um mapa vazio (apenas com os cardinais).-}
gerarMapaCard :: Int -> Int -> String
gerarMapaCard d 0 = []
gerarMapaCard 5 _ = "######   ## # ##   ######"
gerarMapaCard d n
      |n==d || n==1         = (gerarMapaCard d (n-1))   ++     replicate d '#'
      |even n               = (gerarMapaCard d (n-1))   ++     ('#':(replicate (d-2) ' ') ++ "#")
      |otherwise            = (gerarMapaCard d (n-1))   ++     (intersperse ' ' (replicate (1 + div d 2) '#'))









{- | A função @fllMapaCard@ procede à substituição dos espaços do mapa vazio em que não há cardinais pelos caracteres da string a ser descodificada, cujo número de

     elementos coincide com o número de caracteres que serão precisos, naturalmente.-}
fillMapaCard :: String -> String -> String
fillMapaCard [] _ = []
fillMapaCard t [] =                                   fillMapaCard t [' ']
fillMapaCard (h:t) (x:xs) = if h == ' '        then x:fillMapaCard t xs                  else h:fillMapaCard t (x:xs)













{- | A função @lilstring@, a partir de uma dimensão __d__, divide uma @String@ numa lista de __d__ strings, com __d__ elementos cada uma.-}
lilstring :: String -> Int -> [String]      
lilstring "" _ = []
lilstring st d = [(take d st)]      ++       (lilstring (drop d st) d)




{- | A função @constroiMapa@ gera o output final da @decode@, retirando os power ups e as bombas de dentro do mapa e registando a informação relativa

     aos mesmos em strings que o seguem, como num normal estado do mapa.-}
constroiMapa :: [String] -> [String]
constroiMapa mapa = (retirarPowerups mapa) ++ (gerarInfoMapa1 mapa 0) ++ (gerarInfoMapa2 mapa 0) ++ (gerarInfoMapa3 mapa 0) 










{- | A função @unlineInfo@, a partir da string codificada separa a informação relativa a cada bomba e a cada jogador, gerando uma lista de @Strings@ que 

     serve para ser junta com a informação retirada do mapa.-}
unlineInfo :: String -> String -> [String]
unlineInfo [] cc = [cc]
unlineInfo (h:t) cc 
       |h == '/'                                 = cc:unlineInfo t ""
       |h == '>'                                 = cc:unlineInfo t ">"
       |otherwise                                =    unlineInfo t (cc ++ [h])























{- | Esta função tem como objetivo gerar uma lista de @Strings@ com toda a informação relativa aos power ups bombs, recorrendo a uma função auxiliar, que 

     concretiza este processo para cada linha.-}
gerarInfoMapa1 :: [String] -> Int -> [String]
gerarInfoMapa1 [] _ = []
gerarInfoMapa1 (h:t) l = (powersNumaString1 h l 0) ++ (gerarInfoMapa1 t (l+1))






{- | Esta função serve de auxiliar à @gerarInfoMapa1@, gerando também uma lista de @Strings@, dado que podem haver vários powerups numa linha.-}
powersNumaString1 :: String -> Int -> Int -> [String]
powersNumaString1 [] _ _ = []
powersNumaString1 (h:t) l c = if elem h "+-"               then ('+':' ':(show c) ++ " " ++ (show l)):powersNumaString1 t l (c+1)                 else    powersNumaString1 t l (c+1)





















{- | Esta função é exatamente igual à @gerarInfoMapa1@, no entanto trata dos power ups flames. Procedemos desta forma, porque os powerups têm de surgir 

    organizados no mapa (primeiro bombs, depois flames) -}
gerarInfoMapa2 :: [String] -> Int -> [String]
gerarInfoMapa2 [] _ = []
gerarInfoMapa2 (h:t) l = (powersNumaString2 h l 0) ++ (gerarInfoMapa2 t (l+1))



{- | Função que serve de auziliar à @gerarInfoMapa2@ . -}
powersNumaString2 :: String -> Int -> Int -> [String]
powersNumaString2 [] _ _ = []
powersNumaString2 (h:t) l c = if elem h "!:"               then ('!':' ':(show c) ++ " " ++ (show l)):powersNumaString2 t l (c+1)                else powersNumaString2 t l (c+1)
























{- | Outra função semelhante à @gerarInfoMapa1@, que trata da informação relativa a bombas.-}
gerarInfoMapa3 :: [String] -> Int -> [String]
gerarInfoMapa3 [] _ = []
gerarInfoMapa3 (h:t) l = (powersNumaString3 h l 0) ++ (gerarInfoMapa3 t (l+1))



{- | Esta função gera informação relativa às bombas que se encontrem numa dad string do mapa. -}
powersNumaString3 :: String -> Int -> Int -> [String]
powersNumaString3 [] _ _ = []
powersNumaString3 (h:t) l c = if elem h "*"               then (h:' ':(show c) ++ " " ++ (show l)):powersNumaString3 t l (c+1)                   else powersNumaString3 t l (c+1)




























{- | A função unir bombas aproveita a informação recuperada das bombas colocadas no mapa e volta a unir essas @Strings@ com as relativas à informação 

     alheia à localização das bombas, que foram deiixadas de fora.-}
unirBombas :: [String] -> Int -> [String]
unirBombas [] _        = []
unirBombas (h:t) n
      |h!!0 == '*'     = (procurarBomba h t n 0):unirBombas t (n+1)
      |h!!0 == '>'     =                         unirBombas t n
      |otherwise       =                       h:unirBombas t n

 







{- | A função @procurarBomba@  percorre o mapa em busca da string com a informação restante de cada bomba que é retirada do mapa.-}
procurarBomba :: String -> [String] -> Int -> Int -> String
procurarBomba st (h:t) n c 
     |(h!!0) == '>' && n == c      = st ++ " " ++ (drop 1 h)
     |(h!!0) == '>'                = procurarBomba st t n (c+1)
     |otherwise                    = procurarBomba st t n c










{- | A função @retirarPowerups@ percorre o mapa e chama uma função que retira os caracteres dos powerups de cada @String@.-}
retirarPowerups :: [String] -> [String]
retirarPowerups []    = []
retirarPowerups (h:t) =  (subsPowerups h):retirarPowerups t






{- | Esta função percorre uma string e substitui qualquer caracter relativo a power ups pelo caracter que lá se encontrava inicialmente, o que é

     possível porque definimos caracteres diferentes para power ups debaixo de tijolos e power ups destapados. -}
subsPowerups :: String -> String
subsPowerups []           = ""
subsPowerups (h:t) 
      |elem h "*-:"       = ' ':subsPowerups t
      |elem h "+!"        = '?':subsPowerups t
      |otherwise          =   h:subsPowerups t




----------------------------------------------------------------COMUM-------------------------------------------------------------------------------------------




{- | Esta função converte a @String@ relativa à ordenada de um objeto num @Int@, para ser mais cómodo trabalhar com esta informação. -}
pOrden :: String -> Int     
pOrden st = read (pOrdenSt st)


{- | Esta função converte a @String@ relativa à abcissa de um objeto num @Int@, para ser mais cómodo trabalhar com esta informação. -}
pAbcis :: String -> Int   
pAbcis st = read (pAbcisSt st)



{- | Esta função, a partir de uma @String@ relativa a um dado objeto (jogador, power up, bomba..), a parte que corresponde à abcissa da posição desse objeto

     no mapa. -}
pAbcisSt :: String -> String
pAbcisSt st 
      |st !! 3 == ' '     = (st !! 2):"" 
      |st !! 4 == ' '     = take 2 (drop 2 st)
      |otherwise          = take 3 (drop 2 st)


{- | Esta função, a partir de uma @String@ relativa a um dado objeto (jogador, power up, bomba..), a parte que corresponde à ordenada da posição desse objeto

     no mapa. -}
pOrdenSt :: String -> String
pOrdenSt st 
      |last st /= ' '  = pOrdenSt (st ++ " ")
      |pAbcis st >= 100           = if st !! 7 == ' '  then (st !! 6):"" else if st !! 8 == ' ' then take 2 (drop 6 st) else take 3 (drop 6 st)
      |pAbcis st >= 10            = if st !! 6 == ' '  then (st !! 5):"" else if st !! 7 == ' ' then take 2 (drop 5 st) else take 3 (drop 5 st) 
      |otherwise                  = if st !! 5 == ' '  then (st !! 4):"" else if st !! 6 == ' ' then take 2 (drop 4 st) else take 3 (drop 4 st) 



{- | Esta função faz corresponder a uma letra do alfabeto (minúscula) o número da sua ordem no alfabeto (e.g. a=1, b=2, c=3...). -}
howMuch :: Char -> String -> Int
howMuch c (h:t) = if c == h                   then 1                      else 1 + howMuch c t

---------------------------------------------------------------------------------------------------------------------------------





