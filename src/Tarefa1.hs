{-|
Module       : Tarefa1
Description  : Criação do mapa (lista de strings)
Stability    : Experimental


O objetivo deste módulo é criar o mapa do nosso projeto Bomberman. A função mapa receberá uma @dimensão@ e uma @seed@ (cujo objetivo

será gerar uma lista de números aleatórios que corresponde a tijolos, possivelmente a cobrir __power ups__, ou espaços vazios. O output

desta função é uma lista de @Strings@ onde está contida toda a informação do jogo em relação ao espaço onde os jogadores estarão .

-}


module Tarefa1 where
import System.Environment
import Text.Read
import Data.Maybe
import System.Random
import Data.List
import System.IO














-- | Função que recebe os inputs do terminal e utiliza a função @mapa@ para imprimir no @stdout@ a lista de @Strings@ resultantes.
tarefa1 :: IO ()
tarefa1 = do a <- getArgs
             let s = readMaybe (a !! 0)
             let l = readMaybe (a !! 1)
             if length a == 2 && isJust s && isJust l && fromJust s >= 5 && odd (fromJust s)
                then putStr $ unlines $ mapa (fromJust s) (fromJust l)
                else putStrLn "Parâmetros inválidos"














mapa :: Int -> Int -> [String]
{- ^A função @mapa@ cria a lista de strings. Optámos por dvidir este processo em duas partes, a construção do mapa propriamente dito, e a

    concatenação dessa lista de @Strings@ com a informação relativa aos powerups. Ambas essas subdivisões da função @mapa@ fazem uso da lista

    de números aleatórios gerados a partir da @seed@  com um número de elementos que depende da dimensão __d__.

@

>mapM_ putStrLn (mapa 9 0)

#########
#       #
# #?#?# #
#  ?  ? #
#?# # #?#
# ?  ?  #
# #?#?# #
#  ??   #
#########
+ 5 2
+ 3 3
! 5 5


@

    -}
mapa d s = (lilstring (rewriteL (emptyMap d d) ls) d) ++ (powers (rewriteL (emptyMap d d) ls) ls d)
         where ls =   take      ((d-2)^2 - (div (d-3) 2)^2 - 12)    $   randomRs (0,99) (mkStdGen s)




{- | Função que gera o __mapa__ do jogo vazio, em forma de @String@. Os espaços vazios __fixos__ foram marcados com um @'X'@ de modo a serem

     facilmente substituídos por outros caracteres. Optámos por manter, nesta  fase, o mapa em formato @String@ porque isso facilita a nossa

     forma de trabalhar com a informação, tendo em conta que nós pretendemos utilizar funções que percorram os elementos do mapa.

     Como o caso dos mapas de dimensão 5 é especial, em que há sobreposição de espaços vazios fixos, criámos uma definição própria para ele.

Exemplo:

@

> emptyMap 9 9

"##########XX   XX##X# # #X##       ## # # # ##       ##X# # #X##XX   XX##########"

@

Apesar deste ser o output, podemos tentar visualizar este resultado da seguinte forma:

@

"#########
 #XX   XX#
 #X# # #X#
 #       #
 # # # # #
 #       #
 #X# # #X#
 #XX   XX#
 #########"

 @
-}
emptyMap :: Int -> Int -> String
emptyMap d 0 = []
emptyMap 5 _ = "######XXX##X#X##XXX######"
emptyMap d n
      |n==d || n==1         = (emptyMap d (n-1))    ++    replicate d '#'
      |n==(d-1) || n==2     = (emptyMap d (n-1))    ++    "#XX" ++ replicate (d-6) ' '                                   ++ "XX#"
      |n==(d-2) || n==3     = (emptyMap d (n-1))    ++    "#X"  ++ (intersperse ' ' (replicate ((div (d-4) 2) + 1) '#')) ++ "X#"
      |even n               = (emptyMap d (n-1))    ++    "#"   ++ (replicate (d-2) ' ')                                 ++"#"
      |otherwise            = (emptyMap d (n-1))    ++    ( intersperse ' ' (replicate ((div d 2) + 1) '#')    )







{- | Função que traduz os __X__ criados na função @emptyMap@ em espaços vazios, e substitui os espaços que não estavam marcados por caracteres

     resultantes da lista de números aleatórios. Note-se que a definição @rewriteL ls [] = rewriteL ls [1]@ existe para prevenir que a função

     receba uma lista de números vazia, apesar de sabermos que, quando esta fica sem elementos, não será necessaŕio mais nenhum deles, e por isso

     é irrelevante o elemento que adicionamos à lista para a função poder lidar com ela.

@

> rewriteL (emptyMap 9 9) (take 28 $ randomRs (0,99) (mkStdGen s))

"##########       ## #?#?# ##  ?  ? ##?# # #?## ?  ?  ## #?#?# ##  ??   ##########"

@
-}
rewriteL :: String -> [Int] -> String
rewriteL [] ls = ""
rewriteL ls [] = rewriteL ls [1]
rewriteL (h:t) (x:xs)
      |h == 'X'   =              ' ':rewriteL t (x:xs)
      |h == ' '   =    (translate x):rewriteL t xs
      |otherwise  =              '#':rewriteL t (x:xs)







translate :: Int -> Char
{- ^Função que traduz os numeros aleatórios em símbolos para o mapa.

@

>translate 10

'?'

@
-}
translate x
    | x <= 39      = '?'
    |otherwise     = ' '







lilstring :: String -> Int -> [String]
{- ^Função que  agrupa os elementos de uma string __st__ em strings de comprimento __n__ (que será igual ao comprimento do lado do mapa), pela mesma

    ordem em que aparecem em __st__. Sendo aplicada esta função, a informação toma a forma do output final, @[String]@. -}
lilstring "" _ = []
lilstring st d = [(take d st)]   ++   (lilstring (drop d st) d)






{-| O papel desempenhado pela função @powers@ é o de adicionar por baixo do mapa a informação relativa aos __power ups__. Como os bombs têm de vir primeiro

    do que os flames, decidimos criar duas funções semelhantes que constroem as @Strings@ relativas a essa informação e cujos outputs são concatenados na
	@powers@

@

>powers (rewriteL (emptyMap 9 9) ls) ls 9

["+ 5 2","+ 3 3","! 5 5"]


nota: seja ls = take ((d-2)^2 - (div (d-3) 2)^2 - 12) $ randomRs (0,99) (mkStdGen 0)
@

	-}
powers :: String -> [Int] -> Int -> [String]
powers st ls d = (powers1 st ls d 1) ++ (powers2 st ls d 1)








powers1 :: String -> [Int] -> Int -> Int -> [String]
-- ^  Esta função faz uso de um contador __n__, a partir do qual podemos calcular as coordenadas, em função de __d__, dos __power ups__ que encontramos.
powers1 [] _ _ _ = []
powers1 st [] d n = powers1 st [1] d n
powers1 (h:t) (x:xs) d n
      |h == '?' && x < 2           = ("+ " ++ show ((mod n d) -1) ++ " " ++ show (div n d)):powers1 t xs d (n+1)
      |h == '?'                    = powers1 t xs d (n+1)
      |h == ' '                    = if elem n (dozeVazios d) then powers1 t (x:xs) d (n+1) else powers1 t xs d (n+1)
      |otherwise                   = powers1 t (x:xs) d (n+1)








powers2 :: String -> [Int] -> Int -> Int -> [String]
-- ^  Função semelhante à @powers1@, com a única diferença de gerar as strings respeitantes aos __power ups__ flames.
powers2 [] _ _ _ = []
powers2 st [] d n = powers2 st [1] d n
powers2 (h:t) (x:xs) d n
      |h == '?' && x > 1 && x < 4  = ("! " ++ show ((mod n d) -1) ++ " " ++ show (div n d)):powers2 t xs d (n+1)
      |h == '?'                    = powers2 t xs d (n+1)
      |h == ' '                    = if elem n (dozeVazios d) then powers2 t (x:xs) d (n+1) else powers2 t xs d (n+1)
      |otherwise                   = powers2 t (x:xs) d (n+1)


dozeVazios :: Int -> [Int]
-- ^Lista das 12 células, em função de __d__, que estarão sempre vazias.
dozeVazios d = [(d + 2),
                (d + 3),
                (2*d - 2),
                (2*d - 1),
                (2*d + 2),
                (3*d-1),
                ( (d-3)*d + 2),
                ( (d-2)*d - 1),
                ( (d-2)*d + 2),
                ( (d-2)*d + 3),
                ( (d-1)*d - 2),
                ( (d-1)*d - 1)       ]
