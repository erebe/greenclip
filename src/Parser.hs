{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE NoStrict              #-}
{-# LANGUAGE OverloadedStrings     #-}


module Parser where

import           Protolude             hiding (readFile, to, (<&>), (&))

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text             as T
import           Data.Char             (ord, isDigit, isPrint)
import           Lens.Micro
import           Control.Applicative.Combinators
import           Control.Applicative.Permutations

data Pos = Pos
         { line      :: Integer -- The line of the character
         , character :: Integer -- The position in the line of the character
         , offset    :: Integer -- The position in the global flux
         } deriving (Show)

isNewLine :: Char -> Bool
isNewLine w = w == '\n'

incrementPos :: Pos -> Char -> Pos
incrementPos (Pos ln ch off) c = if isNewLine c
                                    then Pos (ln + 1) 0        (off + 1)
                                    else Pos ln       (ch + 1) (off + 1)

-- An input is a bytestring with a position representing the position
-- of the first character in the complete input
type Input = (ByteString, Pos)

newtype Parser a = Parser (Input -> (Either Pos a,Input))

tryP :: Parser a -> Parser a
tryP (Parser f) = Parser $ \input -> 
    case f input of
      (Left p, _) -> (Left p, input)
      result      -> result

getP :: Parser Char
getP = Parser $ \(text,pos) -> if BS.null text
                                  then (Left pos, (text,pos))
                                  else let c = BS.head text
                                       in (Right c, (BS.tail text, incrementPos pos c))

eofP :: Parser ()
eofP = Parser $ \(input,pos) ->
    if BS.null input then (Right (), (input,pos)) else (Left pos, (input,pos))

runParser :: ByteString -> Parser a -> Either Pos a
runParser text (Parser f) = fst $ f (text,pos)
    where pos :: Pos
          pos = Pos 1 0 0

instance Functor Parser where
    fmap f (Parser g) = Parser $ \input -> case g input of
                                             (Right x, out) -> (Right (f x), out)
                                             (Left e,  out) -> (Left e,      out)

instance Applicative Parser where
    (Parser f) <*> x = Parser $ \input ->
        let (result, out) = f input in
        case result of
          Right fn -> let Parser g = fn <$> x in g out
          Left e   -> (Left e, out)
    pure x = Parser $ \input -> (Right x, input)

instance Alternative Parser where
    empty = Parser $ \(text,pos) -> (Left pos, (text,pos))
    (Parser f) <|> (Parser g) = Parser $ \input ->
        case f input of
          (Left _, ninput) -> g ninput
          result           -> result

instance Monad Parser where
    (Parser f) >>= g = Parser $ \input ->
        case f input of
          (Left e, out)  -> (Left e, out)
          (Right x, out) -> let Parser gn = g x in gn out

instance MonadPlus Parser where

-- The config parser ----------------------------------------------------------

data PConfig = PC
             { maxHistoryLength           :: Maybe Int
             , historyPath                :: Maybe Text
             , staticHistoryPath          :: Maybe Text
             , imageCachePath             :: Maybe Text
             , usePrimarySelectionAsInput :: Maybe Bool
             , blacklistedApps            :: Maybe [Text]
             , trimSpaceFromSelection     :: Maybe Bool
             } deriving (Show)

maxHistoryLengthLens :: Lens' PConfig (Maybe Int)
maxHistoryLengthLens = lens maxHistoryLength $ \pc i -> pc { maxHistoryLength = i }

historyPathLens :: Lens' PConfig (Maybe Text)
historyPathLens = lens historyPath $ \pc t -> pc { historyPath = t }

staticHistoryPathLens :: Lens' PConfig (Maybe Text)
staticHistoryPathLens = lens staticHistoryPath $ \pc t -> pc { staticHistoryPath = t }

imageCachePathLens :: Lens' PConfig (Maybe Text)
imageCachePathLens = lens imageCachePath $ \pc t -> pc { imageCachePath = t }

usePrimarySelectionAsInputLens :: Lens' PConfig (Maybe Bool)
usePrimarySelectionAsInputLens = lens usePrimarySelectionAsInput
                                    $ \pc b -> pc { usePrimarySelectionAsInput = b }

blacklistedAppsLens :: Lens' PConfig (Maybe [Text])
blacklistedAppsLens = lens blacklistedApps $ \pc l -> pc { blacklistedApps = l }

trimSpaceFromSelectionLens :: Lens' PConfig (Maybe Bool)
trimSpaceFromSelectionLens = lens trimSpaceFromSelection $ \pc b -> pc { trimSpaceFromSelection = b }

data PConfigMemberParser where
    PCMP :: forall a. Text -> Parser a -> Lens' PConfig (Maybe a) -> PConfigMemberParser

defPConfig :: PConfig
defPConfig = PC Nothing Nothing Nothing Nothing Nothing Nothing Nothing

configOptions :: [PConfigMemberParser]
configOptions = [ PCMP "maxHistoryLength"           intP            maxHistoryLengthLens
                , PCMP "historyPath"                stringP         historyPathLens
                , PCMP "staticHistoryPath"          stringP         staticHistoryPathLens
                , PCMP "imageCachePath"             stringP         imageCachePathLens
                , PCMP "usePrimarySelectionAsInput" boolP           usePrimarySelectionAsInputLens
                , PCMP "blacklistedApps"            (listP stringP) blacklistedAppsLens
                , PCMP "trimSpaceFromSelection"     boolP           trimSpaceFromSelectionLens
                ]

condP :: (Char -> Bool) -> Parser Char
condP cond = getP >>= \r -> guard (cond r) >> return r

digitP :: Parser Int
digitP = condP isDigit >>= \c -> return (ord c - ord '0')

intP :: Parser Int
intP = do
    numbers <- reverse <$> some (tryP digitP)
    let (num,_) = foldl (\(n,o) d -> (n + d*o, o*10)) (0,1) numbers
    return num

whiteP :: Parser ()
whiteP = void $ condP $ \c -> c == ' ' || c == '\t' || c == '\n' || c == '\r'

whitesP :: Parser ()
whitesP = skipMany $ tryP whiteP

charP :: Char -> Parser ()
charP c = void $ condP (==c)

textP :: Text -> Parser ()
textP = mapM_ charP . T.unpack

stringP :: Parser Text
stringP = between open close $ T.pack <$> alphasP
    where open :: Parser ()
          open = whitesP >> charP '"'
          close :: Parser ()
          close = charP '"' >> whitesP
          alphasP :: Parser [Char]
          alphasP = many $ tryP $ condP $ \c -> isPrint c && not (c == '"') 

commaP :: Parser ()
commaP = whitesP >> tryP (charP ',') >> whitesP

listP :: Parser a -> Parser [a]
listP pel = between (charP '[' >> whitesP) (whitesP >> charP ']')
                  $ sepBy (tryP pel) commaP

boolP :: Parser Bool
boolP = ( tryP (textP "True") >> return True  )
    <|> ( textP "False"       >> return False )

newlineP :: Parser ()
newlineP = getP >>= \c -> guard (c == '\n')

pcmpP :: PConfigMemberParser -> Parser (Endo PConfig)
pcmpP (PCMP name parser lense) = do
    textP name
    whitesP
    charP '='
    whitesP
    r <- parser
    return $ Endo $ \pc -> pc & lense ?~ r

pcmpPerm :: PConfigMemberParser -> Permutation Parser (Endo PConfig)
pcmpPerm pcmp = toPermutation $ tryP $ pcmpP pcmp

optionsP :: [PConfigMemberParser] -> Permutation Parser (Endo PConfig)
optionsP (hopt : topts) = foldl (\perm pcmp -> (<>) <$> pcmpPerm pcmp <*> perm)
                                (pcmpPerm hopt) topts
optionsP [] = toPermutation empty -- Won't happen

configP :: Parser (Endo PConfig)
configP = do
    textP "Config"
    whitesP
    result <- between (charP '{' >> whitesP) (whitesP >> charP '}')
                    $ intercalateEffect commaP $ optionsP configOptions
    whitesP
    eofP
    return result



