module Photochop (
  Ficheiro
, textoParaFicheiro
, aplicaFlag
, prop_flip
, prob_pixelMax
, prop_numPixels
) where

import Data.List  
import Data.Char
import Test.QuickCheck
import System.Random

aplicaFlag :: String -> Ficheiro -> String
aplicaFlag flag ficheiro 
  | flag == "-fh" = show $ flipHorizontal ficheiro 
  | flag == "-fv" = show $ flipVertical ficheiro  
  | flag == "-hh" = show $ halfHeight ficheiro 
  | flag == "-hw" = show $ halfWidth ficheiro 
  | flag == "-gs" = show $ greyScale ficheiro 
  | flag == "-rc" = show $ applyColor 'r' ficheiro 
  | flag == "-bc" = show $ applyColor 'b' ficheiro 
  | flag == "-gc" = show $ applyColor 'g' ficheiro  
  | otherwise = "Erro: Flag não reconhecida."



data Pixel = Pixel { red :: Int
                   , green :: Int
                   , blue :: Int
                   } deriving (Eq)  


data Ficheiro = Ficheiro { formato :: String
                         , largura :: Int
                         , altura :: Int
                         , corMax :: Int
                         , matriz :: [[Pixel]]
                         } deriving (Eq)  


instance Show Pixel where
  show (Pixel red green blue) = (show red) ++ (numEspacos red) ++ (show green) ++ (numEspacos green) ++ (show blue) ++ (numEspacos blue)


instance Show Ficheiro where
   show (Ficheiro formato largura altura corMax imagem) = "P3" ++ "\n" ++ (show largura) ++ " " ++ (show altura) ++ "\n" ++ (show corMax) ++ "\n" ++ (imprimeFinal largura imagem)



instance Arbitrary Ficheiro where
  arbitrary = do
    largura <- choose (1, 100) :: Gen Int
    altura <- choose (1, 100) :: Gen Int
    corMax <- choose (0, 255) :: Gen Int
    listaInt <- geradorLista largura altura corMax
    return $ Ficheiro "P3" largura altura corMax (listaParaMatriz largura listaInt) 


geradorLista :: Int -> Int -> Int -> Gen [Int]
geradorLista largura altura corMax = sequence $ replicate (3 * largura * altura) (choose (0, corMax))


listaParaMatriz :: Int -> [Int] -> [[Pixel]]
listaParaMatriz largura listaInt = pixeisParaMatriz largura (tuplosParaPixeis (intParaTuplo listaInt))


intParaTuplo :: [Int] -> [(Int, Int, Int)]
intParaTuplo [] = []
intParaTuplo (x:y:z:xs) = (x, y, z) : intParaTuplo xs


prop_flip :: Ficheiro -> Bool
prop_flip x = x == (flipHorizontal . flipHorizontal . flipVertical . flipVertical) x


prob_pixelMax :: Ficheiro -> Bool 
prob_pixelMax x = null $ filter (\pixel -> (red pixel > corMax x) && (green pixel > corMax x) && (blue pixel > corMax x)) (concat (matriz x))


prop_numPixels :: Ficheiro -> Bool
prop_numPixels x = length (concat (matriz x)) == (altura x) * (largura x)


numEspacos :: Int -> String
numEspacos num 
    | num < 10 = "   "
    | num < 100 = "  "
    | num < 1000 = " "
    | otherwise = ""


imprimeFinal :: Int -> [[Pixel]] -> String 
imprimeFinal largura imagem = init $ inserir (largura * 12) '\n' (juntaLista (concat (imprimeImagem imagem)))


inserir :: Int -> Char -> String -> String
inserir n y xs = countdown n xs where
   countdown 0 xs = y:countdown n xs 
   countdown _ [] = []
   countdown m (x:xs) = x:countdown (m-1) xs


juntaLista :: [String] -> String 
juntaLista [] = []
juntaLista (x:xs) = x ++ (juntaLista xs)


imprimeImagem :: [[Pixel]] -> [[String]]
imprimeImagem matriz = map (map (\x -> show x)) matriz


textoParaFicheiro :: String -> Ficheiro
textoParaFicheiro xs = criaFicheiro . tuplosParaPixeis . stringParaTuplos . tail . removeEspacos $ removeComentarios xs


removeLinhas :: String -> String 
removeLinhas [] = [] 
removeLinhas (x:xs) = if (x == '\n') then (removeLinhas xs) else (x:(removeLinhas xs))


removeComentarios :: String -> String
removeComentarios xs = unlines . filter (\x -> (head x) /= '#') $ (filter (\x -> x /= "")) $ lines xs


removeEspacos :: String -> [String]
removeEspacos s =  case dropWhile isSpace s of
  "" -> []
  s' -> w : removeEspacos s''
    where (w, s'') = break isSpace s'


stringParaTuplos :: [String] -> [(Int, Int, Int)]
stringParaTuplos [] = []
stringParaTuplos (r:[]) = []
stringParaTuplos (r:g:[]) = []
stringParaTuplos (r:g:b:xs) = (read r,read g,read b) : stringParaTuplos xs


tuplosParaPixeis :: [(Int, Int, Int)] -> [Pixel]
tuplosParaPixeis [] = []
tuplosParaPixeis xs = map (\(r,g,b) -> Pixel r g b) xs


criaFicheiro :: [Pixel] -> Ficheiro
criaFicheiro [] = Ficheiro "P0" 0 0 0 [[Pixel 0 0 0]]
criaFicheiro (x:xs) = Ficheiro "P3" (red x) (green x) (blue x) $ pixeisParaMatriz (red x) xs


pixeisParaMatriz :: Int -> [Pixel] -> [[Pixel]]
pixeisParaMatriz _ [] = []
pixeisParaMatriz largura listaPixeis = (take largura listaPixeis) : (pixeisParaMatriz largura $ drop largura listaPixeis)


flipHorizontal :: Ficheiro -> Ficheiro
flipHorizontal (Ficheiro formato largura altura corMax imagem) = Ficheiro formato largura altura corMax $ map (\xs -> reverse xs) imagem


flipVertical :: Ficheiro -> Ficheiro
flipVertical (Ficheiro formato largura altura corMax imagem) = flipHorizontal $ Ficheiro formato largura altura corMax (rodaMatriz (rodaMatriz imagem))

rodaMatriz :: [[x]] -> [[x]]
rodaMatriz = transpose . map reverse


greyScale :: Ficheiro -> Ficheiro
greyScale (Ficheiro formato largura altura corMax imagem) = Ficheiro formato largura altura corMax $ map (\xs -> map pixelToGrey xs) imagem

pixelToGrey :: Pixel -> Pixel
pixelToGrey (Pixel r g b) = Pixel ((r+g+b) `div` 3) ((r+g+b) `div` 3) ((r+g+b) `div` 3)



applyColor :: Char -> Ficheiro -> Ficheiro 
applyColor cor (Ficheiro formato largura altura corMax imagem) 
    | cor == 'r' = Ficheiro formato largura altura corMax $ map (\xs -> map (applyFilter 'r') xs) imagem
    | cor == 'g' = Ficheiro formato largura altura corMax $ map (\xs -> map (applyFilter 'g') xs) imagem
    | cor == 'b' = Ficheiro formato largura altura corMax $ map (\xs -> map (applyFilter 'b') xs) imagem


applyFilter :: Char -> Pixel -> Pixel
applyFilter cor pixel
    | cor == 'r' = Pixel (red pixel) 0 0
    | cor == 'g' = Pixel 0 (green pixel) 0
    | cor == 'b' = Pixel 0 0 (blue pixel) 


halfWidth :: Ficheiro -> Ficheiro
halfWidth (Ficheiro formato largura altura corMax imagem) 
  | largura > 1 = Ficheiro formato (largura `div` 2) altura corMax $ map halfWidth' imagem
  | otherwise = Ficheiro formato largura altura corMax imagem


halfWidth' :: [Pixel] -> [Pixel]
halfWidth' [] = []
halfWidth' (a:b:pixs) = (Pixel ((red a + red b) `div` 2) ((green a + green b) `div` 2) ((blue a + blue b) `div` 2)):halfWidth' pixs


halfHeight :: Ficheiro -> Ficheiro
halfHeight (Ficheiro formato largura altura corMax imagem) 
  | altura > 1 = Ficheiro formato largura (altura `div` 2) corMax . transpose . matriz . halfWidth . Ficheiro formato largura altura corMax $ transpose imagem
  | otherwise = Ficheiro formato largura altura corMax imagem