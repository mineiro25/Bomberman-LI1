{-|
Module       : Tarefa2
Description  : Reação a comandos (movimentos dos jogadores)
Stability    : Experimental


O objetivo desta tarefa é implementar um sistema de reação a comandos, que dado um número de um jogador e um movimento, retorne o estado do mapa após

esse moviment ter sido efetuado ou o mesmo estado do jogo, no caso de a jogada não ter sido possível.

-}


module Tarefa2 where
import Data.Char (isDigit)
import System.Environment
import Data.List
import Data.Char





{- |  A função move recebe um estado de jogo, um @Int@ e um @Char@, que indicam o jogador e ajogada pretendida, respetivamente. Ao prcorrer a lista de

      @Strings@ correspondente ao estado do jogo, a função verifica qual string é a correspondente ao jogador no argumento, para depois utilizar essa

      informação nas suas funções auxiliares. S uma dada jgada é possível, então o mapa é reescrito.-}    
move :: [String] -> Int -> Char -> [String]
move lst p m 
      |(last lst) !! 0 == '#'              =                                                                                  lst
      |(last lst) !! 0 == head (show p)    = if     (possibleM lst m)       then        (replaceRows lst m)        else       lst
      |otherwise                           =                                                   (move (init lst) p m) ++ [last lst]







{- | A função main recebe dois como input os argumentos relativos ao jogador e à jogada, chamando a função @move@, aqui definida, com esses mesmos argumentos.-}
tarefa2 :: IO ()
tarefa2 = do a <- getArgs
             let p = a !! 0
             let c = a !! 1
             w <- getContents
             if length a == 2 && length p == 1 && isDigit (head p) && length c == 1 && head c `elem` "UDLRB"
                then putStr $ unlines $ move (lines w) (read p) (head c)
                else putStrLn "Parâmetros inválidos"






{- | A função possibleM verifica se uma dada jogada, para uma dada lista de @Strings@, em que a última @String@ é a correspondente ao jogador que

     quer efetuar a jogada, é possível. -}
possibleM :: [String] -> Char -> Bool
possibleM lst m 
      |m == 'U'        = if (lst !! (pOrden (last lst) - 1)) !! (pAbcis (last lst)) /= ' '                                     then False else True
      |m == 'L'        = if (lst !! (pOrden (last lst))) !! (pAbcis (last lst) - 1) /= ' '                                     then False else True
      |m == 'R'        = if (lst !! (pOrden (last lst))) !! (pAbcis (last lst) + 1) /= ' '                                     then False else True
      |m == 'D'        = if (lst !! (pOrden (last lst) + 1)) !! (pAbcis (last lst)) /= ' '                                     then False else True
      |m == 'B'        = if bombaSpot lst (last lst)  || numeroBombasP lst (last lst) >= (numeroVezes '+' (last lst) +1)       then False else True







{- | Esta função averigua se há uma bomba nas cordenadas em que o jogador se encontra, dado que não pode haver mais de uma bomba por célula.-}
bombaSpot :: [String] ->  String -> Bool
bombaSpot lst st
      |(last lst) !! 0 == '*'       = if pOrdenSt (last lst) == pOrdenSt st && pAbcisSt (last lst) == pAbcisSt st then True else bombaSpot (init lst) st 
      |(last lst) !! 0 == '#'       =                                                                                  False
      |otherwise                    =                                                                                            bombaSpot (init lst) st









{- | A função @numerobombasP@ verifica o número de bombas que um dado jogador já tem colocadas num mapa.-}
numeroBombasP :: [String] -> String -> Int
numeroBombasP [] _ = 0
numeroBombasP lst st
     |head (last lst) == '*' && (last lst) !! (4 + length (pOrdenSt (last lst)) + length (pAbcisSt (last lst))) == head st  = 1 + numeroBombasP (init lst) st 
     |otherwise                                                                                                             =     numeroBombasP (init lst) st









{- | A função @replaceRows@ é o "coração" da nossa solução para esta tarefa, dado que é ela que reescreve o mapa quandouma jogada é feita, ao eliminar

     @Strings@ de powerups que tenham sido apanhados na jogada, adicionando powerups a jogadores, reescrevedo as coordenadas dos mesmos, e também adicionando

     bombas, no caso de ser essa a jogada. -}
replaceRows :: [String] -> Char -> [String]
replaceRows [] _  = []
replaceRows lst m 
      |m == 'R'       = (removePowers (init lst) (right (last lst))) ++ [addPower (isPower lst (right (last lst)))   (right (last lst))]
      |m == 'L'       = (removePowers (init lst) (left (last lst)))  ++ [addPower (isPower lst (left  (last lst)))    (left (last lst))]
      |m == 'U'       = (removePowers (init lst) (up (last lst)))    ++ [addPower (isPower lst (up    (last lst)))      (up (last lst))]
      |m == 'D'       = (removePowers (init lst) (down (last lst)))  ++ [addPower (isPower lst (down  (last lst)))    (down (last lst))]
      |m == 'B'       =                                                                                             bomb lst (last lst)






{- | Esta pequena função traduz a função @pOrdenSt@ num @Int@, de forma a podermos trabalhar com esta informação em índices, por exemplo.-}
pOrden :: String -> Int     
pOrden st = read (pOrdenSt st)


{- | O papel desempenhado pela @pAbcis@ é análogo ao da @pOrden@, mas diz respeito à abcissa de um dado objeto (e.g. jogador, bomba, power up..)-}
pAbcis :: String -> Int   
pAbcis st = read (pAbcisSt st)


{- | Esta função obtem a parte da string correspondente a um jogador, ou outro objeto com coordenadas, em que se encontra a informação sobre a abcissa

     das coordenadas desse mesmo objeto. Como esta informação começa sempre no 3º caracter de cada string, apenas temos de considerar o quão grande é o

     número relativo a esta mesma coordenada.-}
pAbcisSt :: String -> String
pAbcisSt st 
      |st !! 3 == ' '     =          (st !! 2):"" 
      |st !! 4 == ' '     = take 2 (drop 2 st)
      |otherwise          = take 3 (drop 2 st)






{- | Apesar de esta função ter exaamente  mesmo papel que a @pAbcisSt@, a ordenada dos objetos vem em segundo lugar, e, por isso, os caracteres da string

    que correspondem a esta informação também dependem da quantidade de caracteres utilizados para descrever a abcissa.-}
pOrdenSt :: String -> String
pOrdenSt st 
      |last st /= ' '  = pOrdenSt (st ++ " ")
      |pAbcis st >= 100           = if st !! 7 == ' '  then (st !! 6):"" else if st !! 8 == ' ' then take 2 (drop 6 st) else take 3 (drop 6 st)
      |pAbcis st >= 10            = if st !! 6 == ' '  then (st !! 5):"" else if st !! 7 == ' ' then take 2 (drop 5 st) else take 3 (drop 5 st) 
      |otherwise                  = if st !! 5 == ' '  then (st !! 4):"" else if st !! 6 == ' ' then take 2 (drop 4 st) else take 3 (drop 4 st) 















{- | Esta função reescreve uma string de um jogador que se mova para a direita, somando 1 à sua abcissa. Funcionando d forma semelhante, definimos também as

     funções @left@, @up@ e @down@ 

@

>right "0 1 1 ++"
"0 2 1 ++"

>down "0 2 1 ++"
"0 2 2 ++"

@

-}
right :: String -> String        
right st = (take 2 st) ++ show (pAbcis st +1) ++ " " ++ pOrdenSt st         ++ (drop (startPowers st) st)


left :: String -> String
left st =  (take 2 st) ++ show (pAbcis st -1) ++ " " ++ pOrdenSt st         ++ (drop (startPowers st) st)


up :: String -> String
up st =    (take 2 st) ++ pAbcisSt st         ++ " " ++ show (pOrden st -1) ++ (drop (startPowers st) st)


down :: String -> String
down st =  (take 2 st) ++ pAbcisSt st         ++ " " ++ show (pOrden st +1) ++ (drop (startPowers st) st)
























{- | A função @bomb@ e responsável por colocar uma bomba no mapa, recorrendo ao próprio mapa e à string relativa ao jogador que a colocou. -}
bomb :: [String] -> String -> [String]
bomb lst st
     |elem ((last lst) !! 0) "!+#"  =                                                                                                                 lst  ++ [bombSt st] 
     |(last lst) !! 0 == '*'        = if pAbcis st > pAbcis (last lst) && pOrden st > pOrden (last lst) then  lst ++ [bombSt st] else (bomb (init lst) st) ++ [last lst] 
     |otherwise                     =                                                                                                 (bomb (init lst) st) ++ [last lst]









{- | Esta função é responsável pela construção da string com a informação de uma nova bomba, a introduzir na lista de @Strings@ com a informação do jogo.

@

>bombSt "0 1 1 !"
"* 1 1 1 2 10"

@
-}
bombSt :: String -> String
bombSt st = '*':      drop 1 ((take (startPowers st) st))       ++  " "  ++       take 1 (st)       ++ " "  ++      show (numeroVezes '!' st + 1 )       ++  " 10"









{- | Esta função indica quantas vezes um dado char ocorre numa string, o que é bastante útil, por exemplo no casode querermos saber quandos power ups

     de um certo tipo um jogador tem.-}
numeroVezes :: Char -> String -> Int
numeroVezes _ [] = 0
numeroVezes c (h:t) =     if      c == h         then      1 + numeroVezes c t               else        numeroVezes c t













{- | A função remove powers averigua se há power ups em coordenadas coincidentes com as do jogador (após se mover), removendo as strings 

     relativas a esse mesmo powrup, dado que foi apanhado pelo jogador.-}
removePowers :: [String] -> String -> [String] 
removePowers lst st 
      |(last lst) !! 0 == '#'  = lst
      |(last lst) !! 0 == '+'  = if pOrdenSt (last lst) == pOrdenSt st && pAbcisSt (last lst) == pAbcisSt st then (init lst) else (removePowers (init lst) st) ++ [last lst]
      |(last lst) !! 0 == '!'  = if pOrdenSt (last lst) == pOrdenSt st && pAbcisSt (last lst) == pAbcisSt st then (init lst) else (removePowers (init lst) st) ++ [last lst]
      |otherwise               =                                                                                                  (removePowers (init lst) st) ++ [last lst]





















{- | Esta função percorre a lista de @Strings@ e, a partir das coordenadas da posição para onde o jogador se vai mover, verifica se há algum powerup nessa localização

     para o jogador apanhar. -}
isPower :: [String] -> String -> Char
isPower lst st
      |(last lst) !! 0 == '#'  =  '0'
      |(last lst) !! 0 == '+'  = if pOrdenSt (last lst) == pOrdenSt st && pAbcisSt (last lst) == pAbcisSt st then '+' else isPower (init lst) st
      |(last lst) !! 0 == '!'  = if pOrdenSt (last lst) == pOrdenSt st && pAbcisSt (last lst) == pAbcisSt st then '!' else isPower (init lst) st
      |otherwise               =                                                                                           isPower (init lst) st












{- | A função @addPower@ adiciona a uma @String@ de um jogador um dado powerup. -}
addPower :: Char -> String -> String 
addPower c st 
     |c == '!'          =  if (numeroVezes '!' st) == 0 && (numeroVezes '+' st) == 0           then st ++ " !"                     else     st ++ "!"
     |c == '+'          = (take ((startPowers st) + 1 + (numeroVezes c st)) st) ++ "+" ++ (drop ((startPowers st) + 1 + (numeroVezes c st)) st) 
     |otherwise         =                                                                                                                   st










{- | A @startPowers@, a partir de uma @String@ (relativa a um jogador) define o número de caracteres que vêm antes do começo da informação que diz respeito aos powerups.-}
startPowers :: String -> Int
startPowers st = length       (    (pOrdenSt st) ++ (pAbcisSt st)    )        + 3

