{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Conduit                      as CC
import           Control.Monad.Catch          (Exception, throwM)
import           Control.Monad.State          (get, put)
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Conduit                 (Conduit, Consumer, ($$), (=$=))
import qualified Data.Conduit                 as C
import qualified Data.Conduit.Binary          as CB
import qualified Data.Conduit.Lift            as CL
import qualified Data.Conduit.Text            as CT
import qualified Data.Map                     as M
import qualified Data.Set                     as S
import qualified Data.Text                    as T
import qualified Data.Text.Buildable
import           Formatting                   (bprint, build, sformat, stext, (%))
import           Prelude                      ()
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

data FeatureInfo
    = Unset                 -- ^ way to process feature is not specified
    | Numeric               -- ^ value remains as is
    | Classes (S.Set Text)  -- ^ feature gets splitted to several binary features


data ConvertedFeatureInfo
    = CUnset           -- ^ way to process feature is not specified
    | CLeave            -- ^ don't modify
    | CClasses [Text]  -- ^ several features, each for some class from list

instance Buildable ConvertedFeatureInfo where
    build CLeave = "Leave as is"
    build (CClasses cls) =
        bprint ("Classes: " %stext) $
        T.intercalate "," $ sformat build <$> cls
    build CUnset = "Not specified"

(>:) :: a -> b -> (a, b)
(>:) = (,)

featuresTable :: M.Map Text FeatureInfo
featuresTable = M.fromList
    [ "color"                  >: Classes mempty
    , "director_name"          >: Classes mempty
    , "num_critic_for_reviews" >: Numeric
    ]

considerValue :: FeatureInfo -> Text -> FeatureInfo
considerValue Unset        _ = Unset
considerValue (Classes cl) t = Classes $ foldr S.insert cl $ T.splitOn "|" t
considerValue Numeric      _ = Numeric

convertFeatureInfo :: MonadIO m => FeatureInfo -> m ConvertedFeatureInfo
convertFeatureInfo Unset        = return CUnset
convertFeatureInfo Numeric      = return CLeave
convertFeatureInfo (Classes cl) = return (CClasses $ S.toList cl)

convertFeatureName :: ConvertedFeatureInfo -> Text -> [Text]
convertFeatureName CUnset         _ = []
convertFeatureName CLeave         n = [n]
convertFeatureName (CClasses cls) n = cls <&> flip (sformat $ build%"_"%build) n

convertFeatureValue :: ConvertedFeatureInfo -> Text -> [Int]
convertFeatureValue CUnset _ = []
convertFeatureValue CLeave t =
    fromMaybe (error $ "Bad number " <> t) $ readMaybe (toString t)
convertFeatureValue (CClasses cls) t =
    let cur = S.fromList $ T.splitOn "|" t
    in  cls <&> fromEnum . (`S.member` cur)

columns :: Text -> [Text]
columns = T.splitOn ","

uncolumns :: [Text] -> Text
uncolumns = T.intercalate ","

readLines :: (MonadIO m, MonadThrow m) => Consumer Text m [FeatureInfo]
readLines = do
    header <- C.await `whenNothingM` error "Empty table"
    let features = columns header <&> \name ->
          fromMaybe Unset $ M.lookup (T.toLower name) featuresTable


    CL.execStateC features . C.awaitForever $ \line -> do
        let entries = columns line
        featuresInfo <- get
        when (length features /= length entries) $
            throwM $ LolException "Columns number mismatch"
        put $ zipWith considerValue featuresInfo entries

convertFeatures :: (MonadIO m) => [ConvertedFeatureInfo] -> Conduit Text m Text
convertFeatures infos = do
    header <- C.await `whenNothingM` error "Empty table"
    convertLine convertFeatureName header

    putStrLn @Text "Process classes as following:"
    forM_ (zip (columns header) infos) $ \(hentry, info) ->
        putStrLn $ sformat ("For "%build%": "%build) hentry info

    C.awaitForever . convertLine $ fmap (map (sformat build)) . convertFeatureValue
      where
        convertLine converter =
            C.yield . uncolumns . concat . zipWith converter infos . columns

