module Lib
    ( someFunc
    ) where
import Control.Monad (guard)
import Data.List

data Content = Black | White | Empty | WhiteDama | BlackDama deriving (Eq, Read)

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

type Coord = (Int, Int) -- (x, y)
type Cell = (Content, String, Coord)  -- (Conteudo, Cor, Coordenada)

type Board = [[Cell]]

viraDama :: Coord -> Content -> Cell 
viraDama (x,0) White = createCell x 0 WhiteDama
viraDama (7,y) Black = createCell 7 y BlackDama
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


showCell :: Cell -> String
showCell (content, color, coord) = color ++ show content ++ "\x1b[0m"  -- Reset da cor

showBoard :: Board -> IO ()
showBoard board = do
    let letters = "01234567"
    let numberedRows = zip [1..] board
    putStr (unlines (map (showRow letters) numberedRows))
    putStrLn "   0  1  2  3  4  5  6  7"

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
comerPeca board (c,r) valor = [(c+2,y) | y <- [r+2,r-2], (c+2) <= 7 && (c+1) <= 7 && y <= 7 && y >= 0 && ehCasaLivre board (c+2,y) && (((getContent (getCell board (c+1) (r+1))) `elem` oponente valor) || ((getContent (getCell board (c+1) (r-1))) `elem` oponente valor))] ++ [(c-2,y) | y <- [r+2,r-2], (c-2) >= 0 && (c-1) >= 0 && y <= 7 && y >= 0 && ehCasaLivre board (c-2,y) && (((getContent (getCell board (c-1) (r+1))) `elem` oponente valor) || ((getContent (getCell board (c-1) (r-1))) `elem` oponente valor))]

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

removerPeca :: Board -> Int -> Int -> Board
removerPeca board c r = 
    [[if r == y && c == x then emptyCell 1 c r else getCell board x y | x <- [0..7]] | y <- [0..7]]

moverPeca :: Board -> Coord -> Coord -> Board
moverPeca board (c, r) movimento = 
    if ehMovimentoValido board (c, r) movimento (getContent (getCell board c r))
        then if abs (fst movimento - c) == 2 && abs (snd movimento - r) == 2
                 then movePiece (removerPeca board (c + div (fst movimento - c) 2) (r + div (snd movimento - r) 2)) (c, r) movimento
                 else movePiece board (c, r) movimento
        else board

posicoesOponente :: Board -> Content -> [Coord]
posicoesOponente board valor = [(x,y) | x <- [0..7], y <- [0..7], (getContent (getCell board x y)) `elem` (oponente valor)]

posicoesJogador :: Board -> Content -> [Coord]
posicoesJogador board valor = [(x,y) | x <- [0..7], y <- [0..7], (getContent (getCell board x y)) == valor]

obrigatorioComer :: Board -> Content -> [Coord]
obrigatorioComer board valor = do 
    (a,b) <- posicoesJogador board valor
    (c,d) <- posicoesOponente board valor
    guard $ a `elem` [0..7] && b `elem` [0..7]
    guard $ (a+2) `elem` [0..7] && (b+2) `elem` [0..7] && (a-2) `elem` [0..7] && (b-2) `elem` [0..7]
    guard $ (a+1) `elem` [0..7] && (b+1) `elem` [0..7] && (a-1) `elem` [0..7] && (b-1) `elem` [0..7]
    let validMoves = [ (a+1,b+1), (a+1,b-1), (a-1,b+1), (a-1,b-1) ]
    guard $ (c+1) `elem` [0..7] && (d+1) `elem` [0..7] && (c-1) `elem` [0..7] && (d-1) `elem` [0..7]
    guard $ (c,d) `elem` validMoves && (ehCasaLivre board (c+1,d+1) || ehCasaLivre board (c-1,d+1) || ehCasaLivre board (c-1,d-1) || ehCasaLivre board (c+1,d-1))
    return (a,b)

obrigatorioComerMov :: Board -> Content -> Board
obrigatorioComerMov board valor = 
    if length (obrigatorioComer board valor) > 0
        then moverPeca board ((obrigatorioComer board valor) !! 0) ((comerPeca board ((obrigatorioComer board valor) !! 0) valor) !! 0)
        else board

--inicia o jogo - é a função recursiva que mantem o jogo rodando recebendo tabuleiros atualizados com os movimentos
play :: Board -> IO ()
play board = do
    putStrLn "Tabuleiro atual:"
    showBoard board
    putStrLn "Escolha a cor da peça (White/Black):"
    jogador <- getLine
    let (valor) = read jogador :: Content
    if length (posicoesOponente board valor) > 0
        then do
            if length (obrigatorioComer board valor) > 0
                then do 
                    let novoTabuleiro = obrigatorioComerMov board valor
                    play novoTabuleiro
                else do
                    putStrLn "Digite a coordenada da peça que deseja movimentar (x,y):"
                    input <- getLine
                    let (xInicial, yInicial) = read input :: Coord
                    if (xInicial, yInicial) `elem` posicoesJogador board valor
                        then do
                            let origCell = getCell board xInicial yInicial
                            if getContent origCell /= Empty
                                then do
                                    putStrLn "Digite a coordenada da posição final que deseja movimentar (x,y):"
                                    input2 <- getLine
                                    let (xFinal, yFinal) = read input2 :: Coord
                                    let destCell = getCell board xFinal yFinal
                                    if getContent destCell == Empty
                                        then do
                                            let validMove = ehMovimentoValido board (xInicial, yInicial) (xFinal, yFinal) (getContent origCell)
                                            if validMove
                                                then do
                                                    let tabuleiroComMovimento = moverPeca board (xInicial, yInicial) (xFinal, yFinal)
                                                    play tabuleiroComMovimento
                                                else do
                                                    putStrLn "Movimento inválido."
                                                    play board
                                        else do
                                            putStrLn "Posição final ocupada. Tente novamente."
                                            play board
                                else do
                                    putStrLn "Não há peça na posição inicial."
                                    play board
                        else do
                            putStrLn "Peça do oponente, favor tentar novamente."
                            play board
        else do
            putStrLn "Parabéns! Você ganhou."



someFunc :: IO ()
someFunc = do
    let newBoard = (createBoard 8)
    play newBoard