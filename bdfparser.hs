import Control.Monad
import Data.Char
import Text.ParserCombinators.Parsec
import Data.Bits
import Data.Maybe
import System.Environment

data Glyph = Glyph {
             name:: String
            ,encoding:: Int
            ,width:: Int
            ,height:: Int
            ,bitmap:: [Int]
            } deriving (Show)


pHexLine :: Parser Int
pHexLine = do
           ch1 <-  oneOf "0123456789ABCDEF"
           ch2 <-  oneOf "0123456789ABCDEF"
           newline
           return (16 * digitToInt ch1 + digitToInt ch2)  

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
        manyTill anyChar $ lookAhead $ try (string "BITMAP")
        bmap<- pBitMap
        string "ENDCHAR"
        newline
        return $ Glyph name encoding (read widthstr) (read heightstr) bmap
       
pBDF :: Parser [Glyph] 
pBDF = do 
       manyTill anyChar $ lookAhead $ try (string "ENDPROPERTIES")
       string "ENDPROPERTIES"
       newline
       string "CHARS"
       spaces
       manyTill digit newline
       manyTill pGlyph $ lookAhead $ try (string "ENDFONT")

extractEnc:: Glyph -> (Int, Glyph)
extractEnc a = (encoding a, a)
mkLookupTable:: [Glyph]->[(Int,Glyph)]
mkLookupTable = map extractEnc
lookupGlyph :: Int->[Glyph]->Maybe Glyph
lookupGlyph i t = lookup i $ mkLookupTable t

lookup2printable :: Int->[Glyph]->String
lookup2printable i t = fromMaybe "not found" (liftM glyph2str $ lookupGlyph i t) 

printlookup:: Int-> Either ParseError [Glyph] -> IO()
printlookup i (Right a) = putStrLn $ lookup2printable i a
printlookup i (Left _) = putStrLn "Parse error"


glyph2str:: Glyph -> String
glyph2str a = concatMap int2bitstr (bitmap a)

int2bitstr:: Int -> String
int2bitstr a = map (bool2char.testBit a) (reverse [0..7]) ++ "\n"
bool2char:: Bool -> Char
bool2char False = '.'
bool2char True  = '*'




main = do
       args<- getArgs
       unless (length args == 2) $ error "Wrong number of args" 
       filestr <- readFile $ head args
       let pres = parse pBDF "error parsing" filestr
       printlookup (read $ args!!1)  pres 
          
