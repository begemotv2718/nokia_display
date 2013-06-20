import Control.Monad
import Data.Char
import Text.ParserCombinators.Parsec
import Data.Bits
import Data.Maybe
import System.Environment
import Data.List
import Text.Printf
data FontBBox = FontBBox { fbbWidth:: Int,  fbbHeight:: Int, fbbXoffs:: Int, fbbYoffs:: Int} deriving(Show)
data Glyph = Glyph {
             name:: String
            ,encoding:: Int
            ,width:: Int
            ,height:: Int
            ,xoffset:: Int
            ,yoffset:: Int
            ,bitmap:: [Int]
            } deriving (Show)

--- parser ----
pHexLine :: Parser Int
pHexLine = do
           hexst <- many $  oneOf "0123456789ABCDEF"
           newline
           return (read $ "0x"++hexst)  

pBitMap :: Parser [Int]
pBitMap = do
          string "BITMAP"
          newline
          manyTill pHexLine $ lookAhead $ try (string "ENDCHAR")

pEncoding :: Parser Int
pEncoding = do
        string "ENCODING"
        spaces
        encstr<-manyTill digit newline
        return (read encstr)

pName :: Parser String
pName = do
        string "STARTCHAR " 
        spaces
        many $ noneOf "\n\r"

pGlyph :: Parser Glyph

pGlyph = do
        name <- pName
        newline
        encoding<- pEncoding                
        manyTill anyChar $ lookAhead $ try (string "BBX ")
        string "BBX "
        widthstr<- many digit
        spaces
        heightstr <- many digit
        spaces
        xoffstr <- many $ oneOf "0123456789-"
        spaces
        yoffstr <- many $ oneOf "0123456789-"
        manyTill anyChar $ lookAhead $ try (string "BITMAP")
        bmap<- pBitMap
        string "ENDCHAR"
        newline
        return $ Glyph name encoding (read widthstr) (read heightstr) (read xoffstr) (read yoffstr) bmap
       
pBDF :: Parser [Glyph] 
pBDF = do 
       manyTill anyChar $ lookAhead $ try (string "ENDPROPERTIES")
       string "ENDPROPERTIES"
       newline
       string "CHARS"
       spaces
       manyTill digit newline
       manyTill pGlyph $ lookAhead $ try (string "ENDFONT")

--- Glyph lookup functions ---
extractEnc:: Glyph -> (Int, Glyph)
extractEnc a = (encoding a, a)
mkLookupTable:: [Glyph]->[(Int,Glyph)]
mkLookupTable = map extractEnc
lookupGlyph :: Int->[Glyph]->Maybe Glyph
lookupGlyph i t = lookup i $ mkLookupTable t

---Glyph display functions ---
lookup2printable :: Int->[Glyph]->String
lookup2printable i t = fromMaybe "not found" (liftM (glyph2str. padGlyph fbb) $ lookupGlyph i t) 
lookup2CForm i t = fromMaybe "not found" (liftM (cForm. padGlyph fbb) $ lookupGlyph i t)

printlookup:: Int-> Either ParseError [Glyph] -> IO()
printlookup i (Right a) = putStrLn $ lookup2printable i a
printlookup i (Left _) = putStrLn "Parse error"
printCForm:: Int-> Either ParseError [Glyph] -> IO()
printCForm i (Right a) = putStrLn $ lookup2CForm i a
printCForm i (Left e) = putStrLn $ "Parse error:"++show e


glyph2str:: Glyph -> String
glyph2str a = concatMap ( (++ "\n"). map bool2char. reverse. nhighestBits' (width a) ) (bitmap a)

int2bitstr:: Int -> String
int2bitstr a = map (bool2char.testBit a) (reverse [0..7]) ++ "\n"
bool2char:: Bool -> Char
bool2char False = '.'
bool2char True  = '*'

---Bit 2 list of boolean ---

writeBit:: Bits a=>a->Bool->Int->a
writeBit a True n  = setBit a n
writeBit a False n = clearBit a n 

boolList2Bits:: Bits a=> [Bool] -> a

boolList2Bits xs = snd (foldl' applyBitAndCount (0,0) xs)   
  where applyBitAndCount:: Bits a =>(Int, a)->Bool->(Int, a)
        applyBitAndCount (n, accum) b = (n+1, writeBit accum b n)

bits2BoolList :: Bits a=> a->[Bool]
bits2BoolList a = map (testBit a) [0..]

bits2nBool :: Bits a => Int->a->[Bool]
bits2nBool n a = take n $ bits2BoolList a 

bytesize :: (Bits a,Integral a) => a->Int
bytesize x = ceiling(log(fromIntegral x +1)/(8*log 2))
nhighestBits :: (Bits a,Integral a) => Int->a->[Bool]
nhighestBits n x = drop (8*bytesize x - n) $  bits2nBool (8*bytesize x) x

nhighestBits' n x   
                    |n <= 8  = drop (8 -n ) $ bits2nBool 8 x
                    | n<=16  = drop (16-n) $ bits2nBool 16 x 
                    | n<=24  = drop (24-n) $ bits2nBool 24 x
                    |otherwise  = error "we can not do it"

-- Glyph manipulation --
fbb::FontBBox
fbb = FontBBox 5 8 0 (-1)

padGlyph:: FontBBox->Glyph->Glyph
padGlyph fb x = Glyph (name x) (encoding x) (fbbWidth fb ) (fbbHeight fb) (fbbXoffs fb) (fbbYoffs fb) newbmap
          where newbmap = replicate toppad 0 ++ bitmap x ++ replicate botpad 0
                botpad = yoffset x - fbbYoffs fb
                toppad = fbbHeight fb - height x - botpad 

chunksOf :: Int ->a-> [a] -> [[a]]
chunksOf k x = go
  where
    go t = case splitAt k t of
             (a,b) | null a    -> []
                   | otherwise -> pad k x a : go b
    pad::Int->a->[a]->[a] 
    pad len padder list = list ++ replicate (len - length list) padder


transposeGlyphBitMap:: Glyph -> [[Int]]
transposeGlyphBitMap g = map (map boolList2Bits.chunksOf 8 False) $ (reverse.transpose) $ map (nhighestBits' $ width g) $ bitmap g
cForm::Glyph -> String
cForm  = intercalate "," . map (printf "0x%02x") .concat .  transpose . transposeGlyphBitMap 

main = do
       args<- getArgs
       unless (length args == 2) $ error "Usage: bdfparse <file> <charcode>" 
       filestr <- readFile $ head args
       let pres = parse pBDF "error parsing" filestr
       let glyphlist = read ("["++ args!!1 ++ "]")::[Int]
       mapM_ (`printlookup` pres) glyphlist 
       mapM_ (`printCForm` pres)  glyphlist
          
