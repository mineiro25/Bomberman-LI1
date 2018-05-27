module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.Pure.Game
import Data.Char
import Tarefa1
import Tarefa2
import TarefaSqrt16
import System.Environment




type Brick = (Int,Int)
-- ^ (abcissa, ordenada)
type Player   = (Int,Int,Int,Int,Int)
-- ^ (numero,abcissa,ordenada,bombs,flames)
type Powerup  = (Char,Int,Int)
-- ^ (powerup, abcissa, ordenada)
type Bomb    = (Int,Int,Int,Int,Int)
-- ^ (abcissa, ordenada, player, range, timer)
type Estado   = ([String],[Brick],[Brick],[Powerup],[Bomb],[Player],Float)
-- ^ (lista de strings, pedras, tijolos, powerups, bombas, players, tempo)


-- | O estado inicial do jogo.
estadoInicial ::  Int -> Int -> Int -> Estado
estadoInicial d s n     = (             map,
                         retiraRocks  0 map,
                        retiraBricks  0 map,
                             retiraPups map,
                            retiraBombs map,
                            geraPlayers map,
                      fromIntegral (2*d^2)    )
         where map = inserePlayers (mapa d s) n d





------------------------------------------------------------------------------------------------------------------
-- | Função que desenha o jogo.
desenhaEstado :: [Picture] ->  [Picture] -> Estado -> Picture
desenhaEstado l l2 s = Pictures [pedras (l!!0) d s, tijolos (l!!1) d s, (Pictures (powersImg (l!!2) (l!!3) d (takePowers s) s)), bombasImg (l!!4) d s, playersImg [(l!!5),(l!!6),(l!!7),(l!!8)] d s, scoreB l2 s]
              where d = length (head (takeLString s))




-- | Função que altera o estado do jogo quando acontece um evento.
reageEvento :: Event -> Estado -> Estado
reageEvento (EventKey (Graphics.Gloss.Interface.Pure.Game.Char 'w') _ _ _) s   = geraEstado (move (takeLString s) 0 'U') (takeTime s)
reageEvento (EventKey (Graphics.Gloss.Interface.Pure.Game.Char 'a') _ _ _) s   = geraEstado (move (takeLString s) 0 'L') (takeTime s)
reageEvento (EventKey (Graphics.Gloss.Interface.Pure.Game.Char 's') _ _ _) s   = geraEstado (move (takeLString s) 0 'D') (takeTime s)
reageEvento (EventKey (Graphics.Gloss.Interface.Pure.Game.Char 'd') _ _ _) s   = geraEstado (move (takeLString s) 0 'R') (takeTime s)
reageEvento (EventKey (Graphics.Gloss.Interface.Pure.Game.Char 'c') _ _ _) s   = geraEstado (move (takeLString s) 0 'B') (takeTime s)
reageEvento (EventKey (Graphics.Gloss.Interface.Pure.Game.Char 'm') _ _ _) s   = geraEstado (move (takeLString s) 1 'B') (takeTime s)
reageEvento (EventKey (SpecialKey KeyUp                           ) _ _ _) s   = geraEstado (move (takeLString s) 1 'U') (takeTime s)
reageEvento (EventKey (SpecialKey KeyDown                         ) _ _ _) s   = geraEstado (move (takeLString s) 1 'D') (takeTime s)
reageEvento (EventKey (SpecialKey KeyLeft                         ) _ _ _) s   = geraEstado (move (takeLString s) 1 'L') (takeTime s)
reageEvento (EventKey (SpecialKey KeyRight                        ) _ _ _) s   = geraEstado (move (takeLString s) 1 'R') (takeTime s)
reageEvento  _                                                             s   =                               s


-- | Função que altera o estado do jogo quando o tempo avança @n@ segundos.
reageTempo :: Float -> Estado -> Estado
reageTempo t (a,b,c,d,e,f,g) = geraEstado (avanca a (truncate (g - 1))) (g - 1)



-- | Frame rate
fr :: Int
fr = 3

-- | Display mode
dm :: Display
dm = InWindow "Bomberman" (800, 600) (0, 0)

-- | Função principal que invoca o jogo.
main :: IO ()
main = do  list <- getArgs
           let d = read (list !! 0) :: Int
           let s = read (list !! 1) :: Int
           let n = read (list !! 2) :: Int
           p0  <- loadBMP "mario_img/rocks.bmp"
           p1  <- loadBMP "mario_img/brick.bmp"
           p2  <- loadBMP "mario_img/powerup_bombs.bmp"
           p3  <- loadBMP "mario_img/powerup_flames.bmp"
           p4  <- loadBMP "mario_img/bomb1.bmp"
           p5  <- loadBMP "mario_img/pl/0.bmp"
           p6  <- loadBMP "mario_img/pl/1.bmp"
           p7  <- loadBMP "mario_img/pl/2.bmp"
           p8  <- loadBMP "mario_img/pl/3.bmp"
           sb4 <- loadBMP "mario_img/sb/sb.bmp"
           sb0 <- loadBMP "mario_img/sb/0_sb.bmp"
           sb1 <- loadBMP "mario_img/sb/1_sb.bmp"
           sb2 <- loadBMP "mario_img/sb/2_sb.bmp"
           sb3 <- loadBMP "mario_img/sb/3_sb.bmp"
           let l  = [p0,p1,p2,p3,p4,p5,p6,p7,p8]
           let l2 = [sb0,sb1,sb2,sb3,sb4]
           play dm                              -- display mode
                (makeColorI 119 145 255 255)     -- côr do fundo da janela
                fr                               -- frame rate
                (estadoInicial d s n)            -- estado inicial
                (desenhaEstado l l2)                -- desenha o estado do jogo
                reageEvento                      -- reage a um evento
                reageTempo                       -- reage ao passar do tempo






--------------------------------------------------------------------INSERÇÃO DE IMAGENS -----------------------------------------------------------

----------------------------------------------------------------- PEDRAS --------------------------------------------------------------------------




pedras :: Picture -> Int -> Estado -> Picture
-- ^ Esta função gera uma imagem com todas as pedras contidas no mapa.
pedras p0 d s = Pictures [placeMe d x y (resized p0 d)  | (x,y) <- takeRocks s]




----------------------------------------------------------------- TIJOLOS --------------------------------------------------------------------------




tijolos :: Picture -> Int -> Estado -> Picture
-- ^ Esta função gera uma imagem com todos os tijolos contidos no mapa.
tijolos p1 d s = Pictures [placeMe d x y (resized p1 d)  | (x,y) <- takeBricks s]



-----------------------------------------------------------------POWERUPS----------------------------------------------------------------------------



powersImg :: Picture -> Picture -> Int -> [Powerup] -> Estado -> [Picture]
-- ^ Esta função gera uma imagem com todos os powerups contidos no mapa.
powersImg _ _ _ [] _ = []
powersImg p2 p3 d (h:t) s
          | ((takeLString s) !! (takeY h)) !! takeX h == ' '    = if takeChar h == '+' then (placeMe d (takeX h) (takeY h) (resized p2 d)) : (powersImg p2 p3 d t s)
                                                                                       else (placeMe d (takeX h) (takeY h) (resized p3 d)) : (powersImg p2 p3 d t s)
          | otherwise                                           =                                                                            (powersImg p2 p3 d t s)





-------------------------------------------------------------------BOMBAS---------------------------------------------------------------------------



bombasImg :: Picture -> Int -> Estado -> Picture
-- ^ Esta função gera uma imagem com todas as bombas contidas no mapa.
bombasImg p4 d s = Pictures [placeMe d x y (resized p4 d)   | (x,y,_,_,_) <- takeBombs s]


-----------------------------------------------------------------JOGADORES--------------------------------------------------------------------------



playersImg :: [Picture] -> Int -> Estado -> Picture
-- ^ Esta função gera uma imagem com todos os jogadores contidos no mapa.
playersImg  [p5,p6,p7,p8] d s =   Pictures          [if c == 0   then placeMe d x y (resized p5  d) 
                                                else if c == 1   then placeMe d x y (resized p6  d) 
                                                else if c == 2   then placeMe d x y (resized p7  d) 
                                                else                  placeMe d x y (resized p8  d)             | (c,x,y,_,_) <- takePlayers s]

----------------------------------------------------------SCOREBOARD---------------------------------------------------------------------------------


scoreB :: [Picture] -> Estado -> Picture
scoreB l2 s = Pictures [Translate 300 0 (l2!!4), Pictures (sbPlayers l2 (takePlayers s))]

sbPlayers :: [Picture] -> [Player] -> [Picture]
sbPlayers _ [] = []
sbPlayers l2 ((a,_,_,_,_):t)  | a == 0           =  [Translate 300 120    (l2!!0)] ++ (sbPlayers l2 t)
                              | a == 1           =  [Translate 300 20     (l2!!1)] ++ (sbPlayers l2 t)
                              | a == 2           =  [Translate 300 (-80)  (l2!!2)] ++ (sbPlayers l2 t)
                              | a == 3           =  [Translate 300 (-180) (l2!!3)] ++ (sbPlayers l2 t)
                              |otherwise         =                                    (sbPlayers l2 t)























---------------------------------------------------------------CONSTRUÇÃO DO ESTADO----------------------------------------------------------------------


inserePlayers :: [String] -> Int -> Int -> [String]
-- ^ Função que insere num mapa gerado pela tarefa1 um número especificado de jogadores.
inserePlayers mapa n d | n == 1          = mapa ++ ["0 1 1"                                                                    ]
                       | n == 2          = mapa ++ ["0 1 1",     "1 " ++ x ++ " " ++ x                                         ]
                       | n == 3          = mapa ++ ["0 1 1",     "1 " ++ x ++ " " ++ x ,    "2 " ++ x ++ " 1"                  ]
                       | otherwise       = mapa ++ ["0 1 1",     "1 " ++ x ++ " " ++ x ,    "2 " ++ x ++ " 1" ,    "3 1 " ++ x ]
                       where x = show (d-2)


geraEstado :: [String] -> Float -> Estado
-- ^ Função que, a partir de uma lista de strings representativa de um mapa e do tempo, gera uma estrutura do tipo Estado, anteriormente definido.
geraEstado map t = (                 map,
                      retiraRocks  0 map,
                     retiraBricks  0 map,
                          retiraPups map,
                         retiraBombs map,
                         geraPlayers map,
                                       t  )
  



retiraRocks :: Int -> [String] -> [Brick]
-- ^ Função que retira do mapa as pedras em forma de túpulo.
retiraRocks _ []    = []
retiraRocks l (h:t) = (retiraRocksRow l 0 h) ++ retiraRocks (l+1) t



retiraRocksRow :: Int -> Int -> String -> [Brick]
-- ^ Função que retira de cada linha do mapa as pedras, em forma de túpulo.
retiraRocksRow _ _ []    = []
retiraRocksRow l c (h:t) = if    h == '#'         then (c,l):retiraRocksRow l (c+1) t
                                                  else       retiraRocksRow l (c+1) t








retiraBricks :: Int -> [String] -> [Brick]
-- ^ Função que retira do mapa os tijolos em forma de túpulo.
retiraBricks _ []    = []
retiraBricks l (h:t) = (retiraBricksRow l 0 h) ++ retiraBricks (l+1) t



retiraBricksRow :: Int -> Int -> String -> [Brick]
-- ^ Função que retira de cada linha do mapa os tijolos, em forma de túpulo.
retiraBricksRow _ _ []    = []
retiraBricksRow l c (h:t) = if    h == '?'        then (c,l):retiraBricksRow l (c+1) t
                                                  else       retiraBricksRow l (c+1) t



retiraBombs :: [String] -> [Bomb]
-- ^ Função que retira do mapa as bombas colocadas.
retiraBombs [] = []
retiraBombs (h:t) = if h!!0 == '*' then (                                         pAbcis h,
                                                                                  pOrden h,
                    read (take 1 (drop (length (pAbcisSt h) + length (pOrdenSt h) + 4) h)),
                    read (take 1 (drop (length (pAbcisSt h) + length (pOrdenSt h) + 6) h)),
                            read (drop (length (pAbcisSt h) + length (pOrdenSt h) + 8) h)               ) : retiraBombs t
                                   else                                                                     retiraBombs t






retiraPups :: [String] -> [Powerup]
-- ^ Função que a partir do estado do jogo obtém informação dos powerups.
retiraPups [] = []
retiraPups (h:t) = if elem (h!!0) "+!" then (tuplePower h):retiraPups t else retiraPups t

tuplePower :: String -> Powerup
-- ^ Função que converte num túpulo uma string com informação de dum dado powerup.
tuplePower string = (   head string,
                      pAbcis string,
                      pOrden string  )











geraPlayers :: [String] -> [Player]
-- ^ Função que gera a partir de um estado do jogo (no formato da tarefa 2) uma lista de túpulos @Player@.
geraPlayers []    = []
geraPlayers (h:t) = if isDigit (h!!0)      then (tuplePlayer h) : geraPlayers t
                                           else                   geraPlayers t


tuplePlayer :: String -> Player
-- ^ Função que, a partir de uma string de um jogador, gera o túpulo relativo ao mesmo.
tuplePlayer string = (   read (take 1 string) :: Int,
                               pAbcis string    ,
                               pOrden string    ,
                          howMany '+' string    ,
                          howMany '!' string     )



-------TÚPULOS DE ESTADO-----------


takeLString :: Estado -> [String]
-- ^ Função que retira de um estado a lista de strings associada à representação do mapa.
takeLString (a,_,_,_,_,_,_) = a


takeRocks :: Estado -> [Brick]
-- ^ Função que retira de um estado a lista de pedras no mapa.
takeRocks  (_,a,_,_,_,_,_) = a



takeBricks :: Estado -> [Brick]
-- ^ Função que retira de um estado a lista de tijolos no mapa.
takeBricks  (_,_,a,_,_,_,_) = a


takePowers  :: Estado -> [Powerup]
-- ^ Função que retira de um estado a lista de powerups no mapa, em forma de túpulos.
takePowers  (_,_,_,a,_,_,_) = a


takeBombs   :: Estado -> [Bomb]
-- ^ Função que retira de um estado a lista de bombas no mapa, em forma de túpulos.
takeBombs   (_,_,_,_,a,_,_) = a


takePlayers :: Estado -> [Player]
-- ^ Função que retira de um estado a lista de jogadores no mapa, em forma de túpulos.
takePlayers (_,_,_,_,_,a,_) = a


takeTime    :: Estado -> Float
-- ^ Função que retira de um estado o tempo do jogo.
takeTime    (_,_,_,_,_,_,a) = a


-------TÚPULOS DE POWERUPS---------

takeChar    :: Powerup -> Char
-- ^Função que descobre o tipo de powerup representado pelo túpulo (obtém o seu @Char@ representativo)
takeChar (a,_,_)          = a

takeX       :: Powerup -> Int
-- ^ Função que obtém a abcissa de um determinado powerup.
takeX    (_,x,_)          = x

takeY       :: Powerup -> Int
-- ^ Função que obtém a ordenada de um determinado powerup.
takeY    (_,_,y)          = y



howMany :: Char -> String -> Int
-- ^ Função que conta o número de vezes que um certo caracter ocorre numa string, com utilididade, por exemplo, no caso de saber quantos powerups de determinado tipo tem um jogador.
howMany _ [] = 0
howMany c (h:t) = if  h == c      then 1 + howMany c t
                                  else     howMany c t


isInt :: Float -> Bool
isInt x = if x == fromIntegral (truncate x) then True else False 


----------------------------------------------------FUNCOES AUXILIARES À COMPONENTE GRÁFICA ----------------------------------------------

resized :: Picture -> Int -> Picture
-- ^ Função que redimensiona uma imagem importada de acordo om uma dimensão do mapa.
resized p d = Scale (r d) (r d) p



r :: Int -> Float
-- ^ Função que, a partir da dimensão do mapa, indica o rácio necessário para redimensionar as imagens importadas (que têm sempre 85px, por escolha nossa)
r d = (squareSide d) / 85



squareSide :: Int -> Float
-- ^ Dimensão do lado de cada quadrado do mapa, em função da dimensão do mapa de jogo.
squareSide d = (600 / ((fromIntegral d) ))


placeMe :: Int -> Int -> Int -> Picture -> Picture
placeMe d x y p = Translate (-400 + (squareSide d)/2 + (fromIntegral x)*dim) (300 - (squareSide d)/2 - (fromIntegral y)*dim) p
               where dim = squareSide d








