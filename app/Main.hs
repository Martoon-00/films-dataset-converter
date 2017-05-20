{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Conduit                      as CC
import           Control.Lens                 ((.=), (<<+=))
import           Control.Monad.Catch          (Exception, throwM)
import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Attoparsec.Text         as Atto
import           Data.Conduit                 (Conduit, Consumer, ($$), (=$=))
import qualified Data.Conduit                 as C
import qualified Data.Conduit.Binary          as CB
import qualified Data.Conduit.Lift            as CL
import qualified Data.Conduit.Text            as CT
import           Data.Foldable                (notElem)
import qualified Data.Map                     as M
import qualified Data.Set                     as S
import qualified Data.String.HT               as UHT
import qualified Data.Text                    as T
import qualified Data.Text.Buildable
import           Data.Text.Lazy.Builder       (Builder)
import           Formatting                   (bprint, build, sformat, stext, string, (%))
import           Prelude                      ()
import qualified System.Console.ANSI          as ANSI
import           Universum

main :: IO ()
main = getArgs >>= \a -> case a of
    []    -> putStrLn ("Specify path to dataset" :: String)
    (x:_) -> doMain x

doMain :: FilePath -> IO ()
doMain input = runResourceT $ do
    info <-
            CB.sourceFile input
        =$= CT.decode CT.utf8
        =$= CT.lines
         $$ readLines
    cInfo <- mapM convertFeatureInfo info
    CB.sourceFile input
        =$= CT.decode CT.utf8
        =$= CT.lines
        =$= convertFeatures cInfo
        =$= CC.unlinesC
        =$= CT.encode CT.utf8
         $$ CB.sinkFile (input <> ".out")

data LolException = LolException Text
    deriving (Show)

instance Exception LolException

explicitEmpty :: Text -> Text
explicitEmpty "" = "<empty>"
explicitEmpty t  = t

data FeatureInfo
    = Unset                 -- ^ way to process feature is not specified
    | Ignore                -- ^ forced ignore
    | Numeric               -- ^ value remains as is
    | Classes (S.Set Text)  -- ^ feature gets splitted to several binary features

data ConvertedFeatureInfo
    = CUnset           -- ^ way to process feature is not specified
    | CIgnore          -- ^ way to process feature is not specified
    | CNumeric         -- ^ don't modify
    | CClasses [Text]  -- ^ several features, each for some class from list

withColor' :: (ANSI.ColorIntensity, ANSI.Color) -> Builder -> Builder
withColor' (intensity, color) text =
    bprint (string%build%string)
    (ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground intensity color])
    text
    (ANSI.setSGRCode [ANSI.Reset])

withColor :: ANSI.Color -> Builder -> Builder
withColor color = withColor' (ANSI.Vivid, color)

instance Buildable ConvertedFeatureInfo where
    build CUnset   = withColor ANSI.Red "Not specified"
    build CIgnore  = withColor ANSI.Magenta "Dropped"
    build CNumeric = withColor ANSI.Blue "Leave as is"
    build (CClasses cls) =
        let limit = 20
        in  withColor' (ANSI.Dull, ANSI.White) $
            bprint (build%" classes: " %stext%stext)
                (length cls)
                (T.intercalate "," $ sformat build . explicitEmpty <$> take limit cls)
                (if length cls > limit then "..." else "")

(>:) :: a -> b -> (a, b)
(>:) = (,)

featuresTable :: M.Map Text FeatureInfo
featuresTable = M.fromList
    [ "color"                   >: Classes mempty
    , "director_name"           >: Ignore
    , "num_critic_for_reviews"  >: Numeric
    , "duration"                >: Ignore
    , "director_facebook_likes" >: Numeric
    ]

considerValue :: FeatureInfo -> Text -> FeatureInfo
considerValue Unset        _ = Unset
considerValue Ignore       _ = Ignore
considerValue (Classes cl) t = Classes $ foldr S.insert cl $ T.splitOn "|" t
considerValue Numeric      _ = Numeric

convertFeatureInfo :: MonadIO m => FeatureInfo -> m ConvertedFeatureInfo
convertFeatureInfo Unset        = return CUnset
convertFeatureInfo Ignore       = return CIgnore
convertFeatureInfo Numeric      = return CNumeric
convertFeatureInfo (Classes cl) = return (CClasses $ S.toList cl)

convertFeatureName :: ConvertedFeatureInfo -> Text -> [Text]
convertFeatureName CUnset         _ = []
convertFeatureName CIgnore        _ = []
convertFeatureName CNumeric       n = [n]
convertFeatureName (CClasses cls) n = cls <&> flip (sformat $ build%"_"%build) n

convertFeatureValue :: ConvertedFeatureInfo -> Text -> [Int]
convertFeatureValue CUnset   _  = []
convertFeatureValue CIgnore  _  = []
convertFeatureValue CNumeric "" = [0]
convertFeatureValue CNumeric t  =
    pure . fromMaybe (error $ "Bad number " <> t) $ readMaybe (toString t)
convertFeatureValue (CClasses cls) t =
    let cur = S.fromList $ T.splitOn "|" t
    in  cls <&> fromEnum . (`S.member` cur)

columns :: Text -> [Text]
columns t =
    either (error $ "Failed to split into columns: " <> t) identity $
    flip Atto.parseOnly t $
    (`Atto.sepBy` Atto.string ",") (toText . UHT.trim <$> item) <* Atto.endOfInput
  where
    item = Atto.char '"' *> many (Atto.satisfy (/= '"')) <* Atto.char '"'
       <|> many (Atto.satisfy (`notElem` [',', '"']))

uncolumns :: [Text] -> Text
uncolumns = T.intercalate ","

readLines :: (MonadIO m, MonadThrow m) => Consumer Text m [FeatureInfo]
readLines = do
    header <- C.await `whenNothingM` error "Empty table"
    let features = columns header <&> \name ->
          fromMaybe Unset $ M.lookup (T.toLower name) featuresTable
    let featuresNum = length $ columns header

    (_, res) <- CL.execStateC (2 :: Int, features) . C.awaitForever $ \line -> do
        lineNum <- _1 <<+= 1
        let entries = columns line
        featuresInfo <- gets snd
        when (featuresNum /= length entries) $
            throwM $ LolException $
            sformat ("Columns number mismatch (line "%build%"): \
                     \expected "%build%", got "%build)
                     lineNum featuresNum (length entries)
        lift $ _2 .= zipWith considerValue featuresInfo entries

    return res

convertFeatures :: (MonadIO m) => [ConvertedFeatureInfo] -> Conduit Text m Text
convertFeatures infos = do
    header <- C.await `whenNothingM` error "Empty table"
    convertLine convertFeatureName header

    putStrLn @Text "Process classes as following:"
    forM_ (zip (columns header) infos) $ \(hentry, info) ->
        putStrLn $ sformat (build%": "%build) hentry info

    C.awaitForever . convertLine $ fmap (map (sformat build)) . convertFeatureValue
      where
        convertLine converter =
            C.yield . uncolumns . concat . zipWith converter infos . columns

