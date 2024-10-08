module Lib
    ( someFunc
    ) where
import Control.Monad (guard)
import Data.List
import Debug.Trace (trace)
import System.Random (randomRIO)




data Content = Black | White | Empty | WhiteDama | BlackDama deriving (Eq, Read)

data Nome = Brancas | Pretas deriving (Read)

instance Show Content where
    show Black = " ● "
    show White = " ○ "
    show Empty = "   "
    show BlackDama = " ⦿ "
    show WhiteDama = " ⓞ "

oponente :: Content -> [Content]
oponente Black = [White,WhiteDama]
oponente BlackDama = [White,WhiteDama]
oponente WhiteDama = [Black,BlackDama]
oponente White = [Black,BlackDama]

getGrupo :: Content -> [Content]
getGrupo Black = [Black, BlackDama]
getGrupo BlackDama = [Black, BlackDama]
getGrupo WhiteDama = [White,WhiteDama]
getGrupo White = [White,WhiteDama]

instance Show Nome where
    show Brancas = "brancas"
    show Pretas  = "pretas"


type Coord = (Int, Int) -- (x, y)
type Cell = (Content, String, Coord)  -- (Conteudo, Cor, Coordenada)

type Board = [[Cell]]

viraDama :: Coord -> Content -> Cell 
viraDama (x,0) White = createCell x 0 WhiteDama
viraDama (x,7) Black = createCell x 7 BlackDama
viraDama (x,y) valor = createCell x y valor

--setando posições das peças no tabuleiro inicial
initialSetCell :: Int -> Int -> Int -> Cell
initialSetCell n x y
    | even n =  (Empty, "\x1b[48;5;230m", (x, y)) 
    | otherwise = if (y < 3) 
        then (Black, "\x1b[48;5;151m", (x, y)) 
        else 
            if (y > 4) 
                then (White, "\x1b[48;5;151m", (x, y))
                else (Empty, "\x1b[48;5;151m", (x, y))  

emptyCell :: Int -> Int -> Int -> Cell
emptyCell n r c
    | even n = (Empty, "\x1b[48;5;230m", (r, c))  -- Célula com fundo vermelho
    | otherwise = (Empty, "\x1b[48;5;151m", (r, c))  -- Célula com fundo azul

createCell :: Int -> Int -> Content -> Cell
createCell r c content = (content, "\x1b[48;5;151m", (r, c))

emptyBoard :: Int -> Board
emptyBoard size = [[emptyCell (y + x) x y | x <- [0..size-1]] | y <- [0..size-1]]

--criando tabuleiro inicial
createBoard :: Int -> Board
createBoard size = [[initialSetCell (y + x) x y | x <- [0..size-1]] | y <- [0..size-1]]

getCoordX :: String -> Int
getCoordX "a" = 0
getCoordX "b" = 1
getCoordX "c" = 2
getCoordX "d" = 3
getCoordX "e" = 4
getCoordX "f" = 5
getCoordX "g" = 6
getCoordX "h" = 7




showCell :: Cell -> String
showCell (content, color, coord) = color ++ show content ++ "\x1b[0m"  -- Reset da cor

showBoard :: Board -> IO ()
showBoard board = do
    let letters = "01234567"
    let numberedRows = zip [1..] board
    putStr (unlines (map (showRow letters) numberedRows))
    putStrLn "   a  b  c  d  e  f  g  h"

  where
    showRow letters (rowNum, row) = letters !! (rowNum - 1) : " " ++ concatMap (\cell -> showCell cell ++ "") row

showContent :: Content -> String
showContent n = show n

getContent :: Cell -> Content
getContent (x, _, _) = x

getCell :: Board -> Int -> Int -> Cell
getCell board col row = (board !! row) !! col

--mover a peça diretamente
movePiece :: Board -> Coord -> Coord -> Board
movePiece board (x, y) (x', y') = 
    [[if 
        (r == y && c == x) then emptyCell 1 c r --casa da celula inicial fica vazia
        else if (r == y' && c == x') 
            then viraDama (c,r) (getContent(getCell board x y)) --casa da celula final recebe o conteudo
            else getCell board c r | c <- [0..7]] | r <- [0..7]] --demais casas nao mudam



--casasPossiveis :: Coord -> Content -> [Coord] 
--casasPossiveis (c,r) valor = do
--    (c1, r1) <- [(c+1, r-1),(c-1, r-1),(c+2, r-2),(c-2, r-2)] -- para as peças brancas
--    (c2, r2) <- [(c+1, r+1),(c-1, r+1),(c+2, r+2),(c-2, r+2)] -- para as pecas pretas
--    guard (c1 `elem` [0..7] && r1 `elem` [0..7])
--    guard (c2 `elem` [0..7] && r2 `elem` [0..7])
--    if valor == White then return (c1, r1) else return (c2, r2)

ehCasaLivre :: Board -> Coord -> Bool
ehCasaLivre board (c, r) = getContent (getCell board c r) == Empty



comerPeca :: Board -> Coord -> Content -> [Coord]
comerPeca board (c, r) valor = 
    [(c+2, y) | y <- [r+2, r-2], 
                (c+2) <= 7 && (c+1) <= 7 && y <= 7 && y >= 0 && 
                ehCasaLivre board (c+2, y) && 
                (((getContent (getCell board (c+1) (r+1))) `elem` oponente valor) || 
                 ((getContent (getCell board (c+1) (r-1))) `elem` oponente valor))] 
    ++ 
    [(c-2, y) | y <- [r+2, r-2], 
                (c-2) >= 0 && (c-1) >= 0 && y <= 7 && y >= 0 && 
                ehCasaLivre board (c-2, y) && 
                (((getContent (getCell board (c-1) (r+1))) `elem` oponente valor) || 
                 ((getContent (getCell board (c-1) (r-1))) `elem` oponente valor))]

casasPossiveis :: Board -> Coord -> Content -> [Coord] 
casasPossiveis board (c,r) valor = nub (movimentoSimples ++ (comerPeca board (c,r) valor))
    where
        movimentoSimples 
            | valor == Black = [(x,r+1) | x <- [c+1,c-1], x >= 0 && x <= 7 && (r+1) <= 7 && ehCasaLivre board (x,r+1)]
            | valor == White = [(x,r-1) | x <- [c+1,c-1], x >= 0 && x <= 7 && (r-1) >= 0 && ehCasaLivre board (x,r-1)]
            | otherwise = [x | x <- (nub ((zip (reverse [0..(c-1)]) [(r+1)..7]) ++ (zip [(c+1)..7] (reverse [0..(r-1)]))++[(x,x+a) | x <- [0..7], a <- [r-c], (x+a) >= 0 && (x+a) <= 7 && x /= c])), ehCasaLivre board x]
             
--verificando se o movimento é valido
ehMovimentoValido :: Board ->  Coord -> Coord -> Content -> Bool
ehMovimentoValido board inicial movimento valor = movimento `elem` (casasPossiveis board inicial valor)

--ehMovimentoValido :: Coord -> Coord -> Content -> Bool
--ehMovimentoValido inicial movimento valor =
--    if ehCaptura && length casas >= 2
--        then elem movimento casas
--        else elem movimento casas
--    where
--        ehCaptura = abs (fst movimento - fst inicial) == 2 && abs (snd movimento - snd inicial) == 2
--        casas = casasPossiveis inicial valor

verifyContent :: Content -> Bool
verifyContent content = content == White

--devolve array com as casas possiveis de serem ocupadas dado uma celula inicial

showCasas :: [Coord] -> String
showCasas x = show x

removerPeca :: Board -> Int -> Int -> Board -- board x y
removerPeca board c r = 
    [[if r == y && c == x then emptyCell 1 c r else getCell board x y | x <- [0..7]] | y <- [0..7]]

moverPeca :: Board -> Coord -> Coord -> Board
moverPeca board (c, r) (c1, r1) = 
    if c >= 0 && r >= 0 && c1 >= 0 && r1 >= 0 && ehMovimentoValido board (c, r) (c1, r1) (getContent (getCell board c r))
        then if abs (fst (c1, r1) - c) == 2 && abs (snd (c1, r1) - r) == 2
                 then movePiece (removerPeca board (c + div (fst (c1, r1) - c) 2) (r + div (snd (c1, r1) - r) 2)) (c, r) (c1, r1)
                 else movePiece board (c, r) (c1, r1)
        else board

posicoesOponente :: Board -> Content -> [Coord] -- retorna lista das posicoes do oponente
posicoesOponente board valor = [(x,y) | x <- [0..7], y <- [0..7], (getContent (getCell board x y)) `elem` (oponente valor)]

posicoesJogador :: Board -> Content -> [Coord] -- retorna lista das posicoes do jogador
posicoesJogador board valor = [(x,y) | x <- [0..7], y <- [0..7], (getContent (getCell board x y)) `elem` (getGrupo valor)]

contemPecaOponente :: Board -> Content -> Coord -> Bool
contemPecaOponente board content (a, b) =
    let
        cellContent = getContent (getCell board a b)
        opponentContents = oponente content
    in
    cellContent `elem` opponentContents



-- validMoves :: Board -> Coord -> Content -> [Coord]
-- validMoves board (a, b) valor =
--     -- Gera todos os movimentos válidos
--     [ (a-1, b-1) | a-2 `elem` [0..7]
--                 , b-2 `elem` [0..7]
--                 , ehCasaLivre board (a-2, b-2)
--                 , contemPecaOponente board valor (a-1, b-1)
--                 ] ++
--     [ (a+1, b-1) | a+2 `elem` [0..7]
--                 , b-2 `elem` [0..7]
--                 , ehCasaLivre board (a+2, b-2)
--                 , contemPecaOponente board valor (a+1, b-1)
--                 ] ++
--     [ (a+1, b+1) | a+2 `elem` [0..7]
--                 , b+2 `elem` [0..7]
--                 , ehCasaLivre board (a+2, b+2)
--                 , contemPecaOponente board valor (a+1, b+1)
--                 ] ++
--     [ (a-1, b+1) | a-2 `elem` [0..7]
--                 , b+2 `elem` [0..7]
--                 , ehCasaLivre board (a-2, b+2)
--                 , contemPecaOponente board valor (a-1, b+1)
--                 ]

-- validMoves :: Board -> Coord -> Content -> [Coord]
-- validMoves board (a, b) valor =
--     trace ("Validando movimentos para coordenada: " ++ show (a, b)) $
--     trace ("Movimentos válidos: " ++ show movimentos) movimentos
--   where
--     movimentos = [ (a-1, b-1) | a-2 >= 0, b-2 >= 0, ehCasaLivre board (a-2, b-2), contemPecaOponente board valor (a-1, b-1) ] ++
--                  [ (a+1, b-1) | a+2 <= 7, b-2 >= 0, ehCasaLivre board (a+2, b-2), contemPecaOponente board valor (a+1, b-1) ] ++
--                  [ (a+1, b+1) | a+2 <= 7, b+2 <= 7, ehCasaLivre board (a+2, b+2), contemPecaOponente board valor (a+1, b+1) ] ++
--                  [ (a-1, b+1) | a-2 >= 0, b+2 <= 7, ehCasaLivre board (a-2, b+2), contemPecaOponente board valor (a-1, b+1) ]

-- usei chatGPT para refatorar essa funcao validMoves
validMoves :: Board -> Coord  -> Content -> [Coord]
validMoves board  (x, y) content=
    let
        movimentos = [(x + dx, y + dy) | (dx, dy) <- [(1,1), (1,-1), (-1,1), (-1,-1)]]
        movimentosValidos = filter (\(nx, ny) -> ehNoTabuleiro (nx, ny) && ehCasaLivre board (nx, ny)) movimentos
        movimentosComCaptura = filter (\(nx, ny) -> contemPecaOponente board content (nx, ny) && ehCasaLivre board (nx + 2 * (nx - x), ny + 2 * (ny - y))) movimentosValidos
    in
        movimentosComCaptura ++ movimentosValidos

ehNoTabuleiro :: Coord -> Bool
ehNoTabuleiro (x, y) = x >= 0 && x < 8 && y >= 0 && y < 8


posicoesPretas :: Board -> [Coord] 
posicoesPretas board = [(x,y) | x <- [0..7], y <- [0..7], (getContent (getCell board x y)) `elem` (oponente White)]


-- possiveisMovimentos teve auxilio do chatGPT em sua implementacao
possiveisMovimentos :: Board -> [Coord] -> [(Coord, Coord)]
possiveisMovimentos board coords = 
    [(origem, destino) | origem <- coords,
                          destino <- validMoves board origem Black]
  where
    validMovesResults = [validMoves board origem Black | origem <- coords]


pegaMovimentoAleatorio :: Board -> IO (Coord, Coord)
pegaMovimentoAleatorio board = do
    let possiveisMov = possiveisMovimentos board (posicoesPretas board)
    if null possiveisMov
        then do 
            return ((-1, -1), (-1, -1))
        else do
            randomNumber <- randomRIO (0, length possiveisMov - 1)
            return (possiveisMov !! randomNumber)


printAndReturn :: Coord -> IO ()
printAndReturn coord = do
    print coord

proxJogador :: Content -> Content
proxJogador Black = White
proxJogador White = Black

obrigatorioComer :: Board -> Content -> [Coord]
obrigatorioComer board valor = do 
    (a, b) <- posicoesJogador board valor
    (c, d) <- posicoesOponente board valor
    guard $ a `elem` [0..7] && b `elem` [0..7]
    guard $ (a+2) `elem` [0..7] || (b+2) `elem` [0..7] || (a-2) `elem` [0..7] || (b-2) `elem` [0..7]
    guard $ (a+1) `elem` [0..7] || (b+1) `elem` [0..7] || (a-1) `elem` [0..7] || (b-1) `elem` [0..7]
    let moves = validMoves board (a, b) valor
    guard $ (c+1) `elem` [0..7] && (d+1) `elem` [0..7] && (c-1) `elem` [0..7] && (d-1) `elem` [0..7]
    guard $ (c, d) `elem` moves && (ehCasaLivre board (c+1, d+1) || ehCasaLivre board (c-1, d+1) || ehCasaLivre board (c-1, d-1) || ehCasaLivre board (c+1, d-1))
    return (a, b)


obrigatorioComerMov :: Board -> Content -> Board
obrigatorioComerMov board valor = 
    if length (obrigatorioComer board valor) > 0
        then moverPeca board ((obrigatorioComer board valor) !! 0) ((comerPeca board ((obrigatorioComer board valor) !! 0) valor) !! 0)
        else board

mostrarNome :: Content -> Nome
mostrarNome White = Brancas
mostrarNome Black = Pretas

somaJogadasDamas :: Content -> Int -> Int
somaJogadasDamas WhiteDama jogadasDamas =  jogadasDamas + 1
somaJogadasDamas BlackDama jogadasDamas =  jogadasDamas + 1
somaJogadasDamas White jogadasDamas =  0
somaJogadasDamas Black jogadasDamas =  0
somaJogadasDamas Empty count = 0




--inicia o jogo - é a função recursiva que mantem o jogo rodando recebendo tabuleiros atualizados com os movimentos
play :: Board -> Content -> Int -> IO ()
play board jogador contagemMovimentoDamas
    | contagemMovimentoDamas >= 20 = putStrLn "Empate!"
    | null (posicoesOponente board jogador) = putStrLn $ "Parabéns, " ++ show (mostrarNome jogador) ++ ", você ganhou."
    | not (null (obrigatorioComer board jogador)) = do
        let novoTabuleiro = obrigatorioComerMov board jogador
        play novoTabuleiro (proxJogador jogador) 0
    | otherwise = do
        putStrLn "Tabuleiro atual:"
        showBoard board
        putStrLn $ "Vez das pecas " ++ show (mostrarNome jogador)
        putStrLn "Digite a coordenada da peça que deseja movimentar (x,y):"
        input <- getLine
        let (xInicial, yInicial) = read input :: (String, Int)
        if (getCoordX xInicial, yInicial) `elem` posicoesJogador board jogador
            then do
                let origCell = getCell board (getCoordX xInicial) yInicial
                if getContent origCell /= Empty
                    then do
                        putStrLn "Digite a coordenada da posição final que deseja movimentar (x,y):"
                        input2 <- getLine
                        let (xFinal, yFinal) = read input2 :: (String, Int)
                        let destCell = getCell board (getCoordX xFinal) yFinal
                        if getContent destCell == Empty
                            then do
                                let validMove = ehMovimentoValido board (getCoordX xInicial, yInicial) (getCoordX xFinal, yFinal) (getContent origCell)
                                if validMove
                                    then do
                                        let tabuleiroComMovimento = moverPeca board (getCoordX xInicial, yInicial) (getCoordX xFinal, yFinal)
                                        let jogadasDamas = somaJogadasDamas (getContent destCell) contagemMovimentoDamas
                                        play tabuleiroComMovimento (proxJogador jogador) jogadasDamas
                                    else putStrLn "Movimento inválido." >> play board jogador contagemMovimentoDamas
                            else putStrLn "Posição final ocupada. Tente novamente." >> play board jogador contagemMovimentoDamas
                    else putStrLn "Não há peça na posição inicial." >> play board jogador contagemMovimentoDamas
            else putStrLn "Peça do oponente, favor tentar novamente." >> play board jogador contagemMovimentoDamas

--inicia o jogo - é a função recursiva que mantem o jogo rodando recebendo tabuleiros atualizados com os movimentos
playAuto :: Board -> Content -> Int -> IO ()
playAuto board jogador contagemMovimentoDamas
    | contagemMovimentoDamas >= 20 = putStrLn "Empate!"
    | null (posicoesOponente board jogador) = putStrLn $ "Parabéns, " ++ show (mostrarNome jogador) ++ ", você ganhou."
    | not (null (obrigatorioComer board jogador)) = do
        let novoTabuleiro = obrigatorioComerMov board jogador
        playAuto novoTabuleiro (proxJogador jogador) 0
    | otherwise = do
        putStrLn "Tabuleiro atual:"
        showBoard board
        putStrLn $ "Vez das pecas " ++ show (mostrarNome jogador)
        if jogador == Black 
            then do
                (origem, destino) <- pegaMovimentoAleatorio board
                let novoTabuleiro = moverPeca board origem destino
                playAuto novoTabuleiro (proxJogador jogador) (contagemMovimentoDamas + 1)
            else do
                putStrLn "Digite a coordenada da peça que deseja movimentar (x,y):"
                input <- getLine
                let (xInicial, yInicial) = read input :: (String, Int)
                if (getCoordX xInicial, yInicial) `elem` posicoesJogador board jogador
                    then do
                        let origCell = getCell board (getCoordX xInicial) yInicial
                        if getContent origCell /= Empty
                            then do
                                putStrLn "Digite a coordenada da posição final que deseja movimentar (x,y):"
                                input2 <- getLine
                                let (xFinal, yFinal) = read input2 :: (String, Int)
                                let destCell = getCell board (getCoordX xFinal) yFinal
                                if getContent destCell == Empty
                                    then do
                                        let validMove = ehMovimentoValido board (getCoordX xInicial, yInicial) (getCoordX xFinal, yFinal) (getContent origCell)
                                        if validMove
                                            then do
                                                let tabuleiroComMovimento = moverPeca board (getCoordX xInicial, yInicial) (getCoordX xFinal, yFinal)
                                                let jogadasDamas = somaJogadasDamas (getContent destCell) contagemMovimentoDamas
                                                playAuto tabuleiroComMovimento (proxJogador jogador) jogadasDamas
                                            else putStrLn "Movimento inválido." >> playAuto board jogador contagemMovimentoDamas
                                    else putStrLn "Posição final ocupada. Tente novamente." >> playAuto board jogador contagemMovimentoDamas
                            else putStrLn "Não há peça na posição inicial." >> playAuto board jogador contagemMovimentoDamas
                    else putStrLn "Peça do oponente, favor tentar novamente." >> playAuto board jogador contagemMovimentoDamas
                      
        
playZero :: Board -> IO()
playZero board = do
    putStrLn "Digite 1 para jogar sozinho com o computador, e 2 para jogar com outro adversário usuário: "
    input <- getLine
    let tipoDeJogo = read input :: Int
    if tipoDeJogo == 1 then playAuto board White 0 else play board White 0

someFunc :: IO ()
someFunc = do
   let newBoard = (createBoard 8)
   playZero newBoard
--    playAuto newBoard White 0

