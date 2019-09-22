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

-- The Parser -----------------------------------------------------------------

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

-- Parser configuration -------------------------------------------------------

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
    PCMP :: Parser a                -- How to parse the config option
         -> Lens' PConfig (Maybe a) -- How to set it
         -> Text                    -- The default name for this config option
         -> [ Text ]                -- Aliases for the config option
         -> Maybe a                 -- An eventual default value
         -> PConfigMemberParser

defPConfig :: PConfig
defPConfig = PC Nothing Nothing Nothing Nothing Nothing Nothing Nothing

configOptions :: [PConfigMemberParser]
configOptions = [ PCMP intP            maxHistoryLengthLens           "maxHistoryLength" 
                       [ ]
                       $ Just 50
                , PCMP stringP         historyPathLens                "historyPath" 
                       [ ]
                       $ Just "~/.cache/greenclip.history"
                , PCMP stringP         staticHistoryPathLens          "staticHistoryPath"
                       [ ]
                       $ Just "~/.cache/greenclip.staticHistory"
                , PCMP stringP         imageCachePathLens             "imageCachePath"
                       [ ]
                       $ Just "/tmp/greenclip/"
                , PCMP boolP           usePrimarySelectionAsInputLens "usePrimarySelectionAsInput"
                       [ ]
                       $ Just False
                , PCMP (listP stringP) blacklistedAppsLens            "blacklistedApps"
                       [ ]
                       $ Just [ ]
                , PCMP boolP           trimSpaceFromSelectionLens     "trimSpaceFromSelection"
                       [ ]
                       $ Just True
                ]

-- The Parser -----------------------------------------------------------------

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

spaceP :: Parser ()
spaceP = void $ condP $ \c -> c == ' ' || c == '\t'

spacesP :: Parser ()
spacesP = skipMany $ tryP spaceP

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
          alphasP = many $ tryP $ condP $ \c -> isPrint c && (c /= '"') 

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
pcmpP (PCMP parser lense name aliases _) = do
    foldl (\p alias -> p <|> tryP (textP alias)) (tryP $ textP name) aliases
    whitesP
    charP '='
    whitesP
    r <- parser
    return $ Endo $ \pc -> pc & lense ?~ r

pcmpPerm :: PConfigMemberParser -> Permutation Parser (Endo PConfig)
pcmpPerm pcmp@(PCMP _ lense _ _ mdef) =
    case mdef of
      Nothing  -> toPermutation $ tryP $ pcmpP pcmp
      Just def -> toPermutationWithDefault (mkDef def lense) $ tryP $ pcmpP pcmp
  where mkDef :: a -> Lens' PConfig (Maybe a) -> Endo PConfig
        mkDef df ls = Endo $ \pc -> pc & ls ?~ df

optionsP :: [PConfigMemberParser] -> Permutation Parser (Endo PConfig)
optionsP (hopt : topts) = foldl (\perm pcmp -> (<>) <$> pcmpPerm pcmp <*> perm)
                                (pcmpPerm hopt) topts
optionsP [] = toPermutation empty -- Won't happen

oldConfigP :: Parser (Endo PConfig)
oldConfigP = do
    textP "Config"
    whitesP
    result <- between (charP '{' >> whitesP) (whitesP >> charP '}')
                    $ intercalateEffect commaP $ optionsP configOptions
    whitesP
    eofP
    return result

