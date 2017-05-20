{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}

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
    = Ignore Bool
    -- ^ don't include this feature into result,
    --   argument determines whether is was excluded manually
    | forall r. Read r => Modify Text (r -> Int)
    -- ^ value is to be modified
    | Classes (S.Set Text)
    -- ^ feature gets splitted to several binary features

data ConvertedFeatureInfo
    = CIgnore Bool
    | forall r. Read r => CModify Text (r -> Int)
    | CClasses [Text]

withColor' :: (ANSI.ColorIntensity, ANSI.Color) -> Builder -> Builder
withColor' (intensity, color) text =
    bprint (string%build%string)
    (ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground intensity color])
    text
    (ANSI.setSGRCode [ANSI.Reset])

withColor :: ANSI.Color -> Builder -> Builder
withColor color = withColor' (ANSI.Vivid, color)

instance Buildable ConvertedFeatureInfo where
    build (CIgnore byUser) =
        if byUser
        then withColor ANSI.Magenta "Discarded"
        else withColor ANSI.Red "Not specified"
    build (CModify desc _)  = withColor ANSI.Blue $ bprint stext desc
    build (CClasses cls) =
        let limit = 20
        in  withColor' (ANSI.Dull, ANSI.White) $
            bprint (build%" classes: " %stext%stext)
                (length cls)
                (T.intercalate ", " $ sformat build . explicitEmpty <$> take limit cls)
                (if length cls > limit then ", ..." else "")

(>:) :: a -> b -> (a, b)
(>:) = (,)

classes, unset, discard, numeric :: FeatureInfo
classes = Classes mempty
unset   = Ignore False
discard = Ignore True
numeric = Modify "Remain as is" identity

featuresTable :: M.Map Text FeatureInfo
featuresTable = M.fromList
    [ -- gemeral
      "color"                     >: classes
    , "genres"                    >: classes
    , "duration"                  >: discard
    , "movie_title"               >: discard

      -- statistics
    , "gross"                     >: numeric
    , "budget"                    >: numeric
    , "num_critic_for_reviews"    >: numeric
    , "num_user_for_reviews"      >: numeric
    , "num_voted_users"           >: numeric
    , "director_facebook_likes"   >: numeric
    , "actor_1_facebook_likes"    >: numeric
    , "actor_2_facebook_likes"    >: numeric
    , "actor_3_facebook_likes"    >: numeric
    , "cast_total_facebook_likes" >: numeric
    , "movie_facebook_likes"      >: numeric

      -- participants
    , "director_name"             >: discard
    , "actor_1_name"              >: discard
    , "actor_2_name"              >: discard
    , "actor_3_name"              >: discard

      -- misc
    , "plot_keywords"             >: discard
    , "facenumber_in_poster"      >: classes
    , "movie_imdb_link"           >: discard
    , "language"                  >: classes
    , "country"                   >: classes
    , "aspect_ratio"              >: classes
    , "imdb_score"                >: Modify "Multiply by 10"
                                     (\(k :: Double) -> round $ k * 10)
    , "content_rating"            >: classes
    ]

considerValue :: FeatureInfo -> Text -> FeatureInfo
considerValue i@Ignore{}   _ = i
considerValue (Classes cl) t = Classes $ foldr S.insert cl $ T.splitOn "|" t
considerValue (Modify d f) _ = Modify d f

convertFeatureInfo :: MonadIO m => FeatureInfo -> m ConvertedFeatureInfo
convertFeatureInfo (Ignore b)   = return (CIgnore b)
convertFeatureInfo (Modify d f) = return (CModify d f)
convertFeatureInfo (Classes cl) = return (CClasses $ S.toList cl)

convertFeatureName :: ConvertedFeatureInfo -> Text -> [Text]
convertFeatureName CIgnore{}      _ = []
convertFeatureName (CModify _ _)  n = [n]
convertFeatureName (CClasses cls) n = cls <&> flip (sformat $ build%" ("%build%")") n

convertFeatureValue :: ConvertedFeatureInfo -> Text -> [Int]
convertFeatureValue CIgnore{}     _  = []
convertFeatureValue (CModify _ _) "" = [0]
convertFeatureValue (CModify _ f) t  =
    pure . f . fromMaybe (error $ "Bad number " <> t) $ readMaybe (toString t)
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

readLines :: (MonadIO m, MonadThrow m) => Consumer Text m [FeatureInfo]
readLines = do
    header <- C.await `whenNothingM` error "Empty table"
    let features = columns header <&> \name ->
          fromMaybe unset $ M.lookup (T.toLower name) featuresTable
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
    -- convertLine convertFeatureName header

    putStrLn @Text "Process classes as following:"
    forM_ (zip (columns header) infos) $ \(hentry, info) ->
        putStrLn $ sformat (build%": "%build) hentry info
    
    putStrLn @Text "Converting..."
    C.awaitForever . convertLine $ fmap (map (sformat build)) . convertFeatureValue
    putStrLn @Text "Done"
      where
        convertLine converter =
            C.yield . T.intercalate " " . concat . zipWith converter infos . columns

