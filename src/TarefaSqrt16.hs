module TarefaSqrt16 where

import Data.Char (isDigit)
import System.Environment
import Text.Read
import Data.Maybe





avanca :: [String] -> Int -> [String]
avanca mapa n = if n > time then          explosionsMap (allahu mapa) (allahu mapa)
                            else espiral (explosionsMap (allahu mapa) (allahu mapa)) n
              where time = (d-2)^2
                    d = length (mapa!!0)







tarefaSqrt16 :: IO ()
tarefaSqrt16 = do
    a <- getArgs
    let ticks = readMaybe (a !! 0)
    w <- getContents
    if isJust ticks
        then putStr $ unlines $ avanca (lines w) (fromJust ticks)
        else putStrLn "Parâmetros inválidos"





-----------------------------------------DIMINUIÇÃO DOS TIMERS E EXPLOSÃO DE BOMBAS-----------------------------------------------

allahu :: [String] -> [String]
-- ^ Função que percorre o mapa e, a cada bomba, reduz o tepo do contador em 1.
allahu []    = []
allahu (h:t) = if     (h!!0) == '*'        then (tickBomb h):allahu t 
                                           else           h :allahu t




tickBomb :: String -> String
-- ^ Função que reduz o contador na string da bomba.
tickBomb h = if last h == '0' then (take (length h-2) h) ++ "9" 
                              else (take (length h-1) h) ++ (show (read (drop (length h-1) h)-1))





explosionsMap :: [String] -> [String] -> [String]
-- ^ Função que percorre o mapa para encontrar bombas que têm de explodir, levando outro argumento com o mapa, que vai sendo alterado à medida que vão havendo explosões.
explosionsMap mapa []    =                                                                       mapa 
explosionsMap mapa (h:t) = if   (h!!0) == '*' && last h == '0'       then explosionsMap (explode mapa h) t 
                                                                     else explosionsMap          mapa    t


explode :: [String] -> String -> [String]
-- ^ Função que, a partir do estado do mapa e de uma string de uma bomba prestes a explodir, gera o mapa depois da explosão.
explode mapa string = removeBomb (flameUp   (flameDown  (flameRight   (    flameLeft mapa    x y r  )   x y r)     x y r)    x y r) x y
                   where x = pAbcis1 string
                         y = pOrden1 string
                         r = range  string







flameUp :: [String] -> Int -> Int -> Int -> [String]
-- ^ Função que explode os espaços do mapa acima da bomba e chama as funções para efetuar as alterações necessárias no etsado do jogo.
flameUp mapa _ _ 0  =                                                                                                                        mapa
flameUp mapa x y n  | ((mapa !! y) !! x) == '#'            =                                                                                 mapa
                    | ((mapa !! y) !! x) == '?'            = (take y mapa)    ++    [(take x (mapa !! y))   ++ " " ++   (drop (x+1) (mapa !! y ))] ++ (drop (y+1) mapa)
                    | isTherePower  mapa x y               =                                                           removePower           mapa x  y 
                    | isTherePlayer mapa x y               =                                                           removePlayer (flameUp mapa x (y-1) (n-1)) x y
                    | isThereBomb   mapa x y               =                                                           blowTimer    (flameUp mapa x (y-1) (n-1)) x y
                    | otherwise                            =                                                                         flameUp mapa x (y-1) (n-1)





flameDown :: [String] -> Int -> Int -> Int -> [String]
-- ^ Função que explode os espaços do mapa abaixo da bomba e chama as funções para efetuar as alterações necessárias no etsado do jogo.
flameDown mapa _ _ 0  =                                                                                                                          mapa
flameDown mapa x y n  | ((mapa !! y) !! x) == '#'            =                                                                                   mapa
                      | ((mapa !! y) !! x) == '?'            = (take y mapa)    ++    [(take x (mapa !! y))   ++ " " ++   (drop (x+1) (mapa !! y ))] ++ (drop (y+1) mapa)
                      | isTherePower  mapa x y               =                                                           removePower             mapa x  y 
                      | isTherePlayer mapa x y               =                                                           removePlayer (flameDown mapa x (y+1) (n-1)) x y
                      | isThereBomb   mapa x y               =                                                           blowTimer    (flameDown mapa x (y+1) (n-1)) x y
                      | otherwise                            =                                                                         flameDown mapa x (y+1) (n-1)





flameRight :: [String] -> Int -> Int -> Int -> [String]
-- ^ Função que explode os espaços do mapa à direita da bomba e chama as funções para efetuar as alterações necessárias no etsado do jogo.
flameRight mapa _ _ 0  =                                                                                                                           mapa
flameRight mapa x y n  | ((mapa !! y) !! x) == '#'            =                                                                                    mapa
                       | ((mapa !! y) !! x) == '?'            = (take y mapa)    ++    [(take x (mapa !! y))   ++ " " ++   (drop (x+1) (mapa !! y ))] ++ (drop (y+1) mapa)
                       | isTherePower  mapa x y               =                                                           removePower              mapa x  y 
                       | isTherePlayer mapa x y               =                                                           removePlayer (flameRight mapa (x+1) y (n-1)) x y
                       | isThereBomb   mapa x y               =                                                           blowTimer    (flameRight mapa (x+1) y (n-1)) x y
                       | otherwise                            =                                                                         flameRight mapa (x+1) y (n-1)


flameLeft :: [String] -> Int -> Int -> Int -> [String]
-- ^ Função que explode os espaços do mapa à esquerda da bomba e chama as funções para efetuar as alterações necessárias no etsado do jogo.
flameLeft mapa _ _ 0  =                                                                                                                          mapa
flameLeft mapa x y n  | ((mapa !! y) !! x) == '#'            =                                                                                   mapa
                      | ((mapa !! y) !! x) == '?'            = (take y mapa)    ++    [(take x (mapa !! y))   ++ " " ++   (drop (x+1) (mapa !! y ))] ++ (drop (y+1) mapa)
                      | isTherePower  mapa x y               =                                                           removePower             mapa x  y 
                      | isTherePlayer mapa x y               =                                                           removePlayer (flameLeft mapa (x-1) y (n-1)) x y
                      | isThereBomb   mapa x y               =                                                           blowTimer    (flameLeft mapa (x-1) y (n-1)) x y
                      | otherwise                            =                                                                         flameLeft mapa (x-1) y (n-1)







----------------------------------------------------ESPIRAL-------------------------------------------------------------------------------------------------







espiral :: [String] -> Int -> [String]
-- ^ Função que coloca as pedras e esmaga o que estiver nessas coordenadas.
espiral mapa n = eraseObj (placeRock mapa n) n



cordLeft :: (Int,Int) -> Int -> [(Int,Int)]
cordLeft c 0 = [c]
cordLeft (a,b) n =  (a,b):cordLeft (a-1,b) (n-1)

cordRight :: (Int,Int) -> Int -> [(Int,Int)]
cordRight c 0 = [c]
cordRight (a,b) n = (a,b):cordRight (a+1,b) (n-1)

cordUp :: (Int,Int) -> Int -> [(Int,Int)]
cordUp c 0 = []
cordUp (a,b) n =    (a,b):cordUp (a,b-1) (n-1)

cordDown :: (Int,Int) -> Int -> [(Int,Int)]
cordDown c 0 = [c]
cordDown (a,b) n =  (a,b):cordDown (a,b+1) (n-1)

quadrado :: (Int,Int) -> Int -> [(Int,Int)]
quadrado (a,b) d = l1 ++ l2 ++ l3 ++ l4
                where l1 = (cordRight (a,b)        (d-1))
                      l2 = tail (cordDown  (last l1)    (d-1))
                      l3 = tail (cordLeft  (last l2)    (d-1))
                      l4 = tail (cordUp    (last l3)    (d-1))

listaSpiral :: (Int,Int) -> Int -> [(Int,Int)]
-- ^ Função que ger auma lista de pares de coordenadas que respresntam a ordem pela qual o mapa será preenchido por pedras.
listaSpiral (a,b) 1 = [(a,b)]
listaSpiral (a,b) d = q ++ listaSpiral (a+1,b+1) (d-2)
                    where q = (quadrado (a,b) d)

placeRock :: [String] -> Int -> [String]
-- ^ Função que coloca a pedra relativa a um instante do jogo.
placeRock mapa n = (take y mapa) ++  [(take x (mapa !! y)) ++ "#" ++ (drop (x+1) (mapa !! y))] ++ (drop (y+1) mapa)
                 where (x,y) = (listaSpiral (1,1) d) !! (49 - n)
                       d = length (mapa !! 0) - 2


eraseObj :: [String] -> Int -> [String]
-- ^ Função que "esmaga" o que estiver numas dadas coordenadas (onde será colocad auma pedra)
eraseObj mapa n = removePower (removePlayer (removeBomb mapa x y) x y) x y
                where (x,y) = (listaSpiral (1,1) d) !! (49 - n)
                      d = length (mapa !! 0) - 2









---------------------------------FUNÇÕES QUE EFETUAM ALTERAÇÕES NO MAPA AQUANDO UMA EXPLOSÃO---------------------------------

removePower :: [String] -> Int -> Int -> [String]
-- ^ Função que remove o powerup numas dadas coordenadas.
removePower [] _ _ = []
removePower (h:t) x y   = if     elem (h!!0) "+!"     &&    pAbcis1 h == x    &&     pOrden1 h == y        then               t 
                                                                                                         else h:removePower t x y 


removePlayer :: [String] -> Int -> Int -> [String]
-- ^ Função que remove o jogador numas dadas coordenadas.
removePlayer [] _ _ = []
removePlayer (h:t) x y  = if     elem (h!!0) "0123"  &&    pAbcis1 h == x     &&     pOrden1 h == y        then                t 
                                                                                                         else h:removePlayer t x y


blowTimer :: [String] -> Int -> Int -> [String]
-- ^ Função que coloca o timer da bomba numas coordenadas a 1.
blowTimer [] _ _ = []
blowTimer (h:t) x y     = if          (h!!0) == '*'   &&    pAbcis1 h == x    &&     pOrden1 h == y        then                  (stringTimer h):          t 
                                                                                                         else                                h:blowTimer t x y

stringTimer :: String -> String
-- ^ Função que coloca o timer da string de uma bomba a 1 (a menos que esteja para explodir).
stringTimer st = if last st == '0' then st else (take (length st - 1) st) ++ "1"

removeBomb :: [String] -> Int -> Int -> [String]
-- ^ Função que remove o powerup numas dadas coordenadas.
removeBomb [] _ _ = []
removeBomb (h:t) x y   = if    (h!!0)    ==   '*'     &&    pAbcis1 h == x    &&     pOrden1 h == y        then              t 
                                                                                                         else h:removeBomb t x y 








--------------------------FUNÇÕES QUE AVERIGUAM SE HÁ ALTERAÇÕES A FAZER, INTERAÇÃO COM AS CHAMAS DA BOMBA--------------


isTherePower :: [String] -> Int -> Int -> Bool
-- ^ Função que testa se há um powerup num dado espaço do mapa.
isTherePower [] _ _    = False 
isTherePower (h:t) x y = if      elem (h!!0) "+!"     &&       pAbcis1 h == x    &&     pOrden1 h == y              then  True
                                                                                                                  else  isTherePower t x y

isTherePlayer :: [String] -> Int -> Int -> Bool
-- ^ Função que testa se há um jogador num dado espaço do mapa.
isTherePlayer [] _ _ = False
isTherePlayer (h:t) x y = if     elem (h!!0) "0123"   &&       pAbcis1 h == x    &&     pOrden1 h == y              then  True
                                                                                                                  else  isTherePlayer t x y


isThereBomb :: [String] -> Int -> Int -> Bool
-- ^ Função que testa se há um powerup num dado espaço do mapa.
isThereBomb [] _ _    = False 
isThereBomb (h:t) x y = if            (h!!0) == '*'   &&       pAbcis1 h == x    &&     pOrden1 h == y              then  True
                                                                                                                  else   isThereBomb t x y





















--------------------------------------------FUNÇÕES AUXILIARES QUE TRABALHAM COM COORDENADAS EXPRESSAS EM STRINGS---------------------------------------------


{- | Esta pequena função traduz a função @pOrdenSt1@ num @Int@, de forma a podermos trabalhar com esta informação em índices, por exemplo.-}
pOrden1 :: String -> Int     
pOrden1 st = read (pOrdenSt1 st)


{- | O papel desempenhado pela @pAbcis@ é análogo ao da @pOrden@, mas diz respeito à abcissa de um dado objeto (e.g. jogador, bomba, power up..)-}
pAbcis1 :: String -> Int   
pAbcis1 st = read (pAbcisSt1 st)


{- | Esta função obtem a parte da string correspondente a um jogador, ou outro objeto com coordenadas, em que se encontra a informação sobre a abcissa

     das coordenadas desse mesmo objeto. Como esta informação começa sempre no 3º caracter de cada string, apenas temos de considerar o quão grande é o

     número relativo a esta mesma coordenada.-}
pAbcisSt1 :: String -> String
pAbcisSt1 st 
      |st !! 3 == ' '     =          (st !! 2):"" 
      |st !! 4 == ' '     = take 2 (drop 2 st)
      |otherwise          = take 3 (drop 2 st)






{- | Apesar de esta função ter exaamente  mesmo papel que a @pAbcisSt@, a ordenada dos objetos vem em segundo lugar, e, por isso, os caracteres da string

    que correspondem a esta informação também dependem da quantidade de caracteres utilizados para descrever a abcissa.-}
pOrdenSt1 :: String -> String
pOrdenSt1 st 
      |last st /= ' '  = pOrdenSt1 (st ++ " ")
      |pAbcis1 st >= 100           = if st !! 7 == ' '  then (st !! 6):"" else if st !! 8 == ' ' then take 2 (drop 6 st) else take 3 (drop 6 st)
      |pAbcis1 st >= 10            = if st !! 6 == ' '  then (st !! 5):"" else if st !! 7 == ' ' then take 2 (drop 5 st) else take 3 (drop 5 st) 
      |otherwise                  = if st !! 5 == ' '  then (st !! 4):"" else if st !! 6 == ' ' then take 2 (drop 4 st) else take 3 (drop 4 st) 



range :: String -> Int
-- ^ Função que retorna o range da bomba descrita numa dada string.
range string = 1 + read (fst         (span (isDigit) (drop (6 + (length (pAbcisSt1 string)) + (length (pOrdenSt1 string))) string))              )


