module Main where

import Graphics.Gloss         
import Graphics.Gloss.Data.Picture  
import Graphics.Gloss.Interface.Pure.Game

-- | Uma representação do estado do jogo.
type Estado = ...

-- | O estado inicial do jogo.
estadoInicial :: Estado
estadoInicial = ...

-- | Função que desenha o jogo.
desenhaEstado :: Estado -> Picture
desenhaEstado s = ...

-- | Função que altera o estado do jogo quando acontece um evento.
reageEvento :: Event -> Estado -> Estado
reageEvento e s = ...

-- | Função que altera o estado do jogo quando o tempo avança @n@ segundos.
reageTempo :: Float -> Estado -> Estado
reageTempo f s = ...

-- | Frame rate
fr :: Int
fr = 50

-- | Display mode
dm :: Display
dm = InWindow "Novo Jogo" (800, 600) (0, 0)
    
-- | Função principal que invoca o jogo.
main :: IO ()
main = play dm              -- display mode
            (greyN 0.5)     -- côr do fundo da janela
            fr              -- frame rate
            estadoInicial   -- estado inicial
            desenhaEstado   -- desenha o estado do jogo
            reageEvento     -- reage a um evento
            reageTempo      -- reage ao passar do tempo