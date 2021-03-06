{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Conduit                      as CC
import           Control.Lens                 ((.=), (<<+=))
import           Control.Monad.Catch          (throwM)
import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Attoparsec.Text         as Atto
import           Data.Conduit                 (Conduit, Consumer, ($$), (=$=))
import qualified Data.Conduit                 as C
import qualified Data.Conduit.Binary          as CB
import qualified Data.Conduit.Lift            as CL
import qualified Data.Conduit.Text            as CT
import           Data.Foldable                (notElem)
import qualified Data.List                    as L
import qualified Data.Map                     as M
import qualified Data.String.HT               as UHT
import qualified Data.Text                    as T
import           Formatting                   (build, sformat, (%))
import           Prelude                      ()
import           Universum

import           Feature                      (FeatureInfo (..), considerValue,
                                               convertFeatureName, convertFeatureValue)
import qualified FeatureTable                 as FT
import           Util                         (LolException (..))

main :: IO ()
main = getArgs >>= \a -> case a of
    []    -> putStrLn ("Specify path to dataset" :: String)
    (x:_) -> doMain x

doMain :: FilePath -> IO ()
doMain input = runResourceT $ do
    info <- readLines $$ accountLines
    readLines =$= convertFeatures info $$ writeLines
  where
    readLines = CB.sourceFile input
            =$= CT.decode CT.utf8
            =$= CT.lines
    writeLines = CC.unlinesC
             =$= CT.encode CT.utf8
             =$= CB.sinkFile (input <> ".out")

columns :: Text -> [Text]
columns t =
    either (error $ "Failed to split into columns: " <> t) identity $
    flip Atto.parseOnly t $
    (`Atto.sepBy` Atto.string ",") (toText . UHT.trim <$> item) <* Atto.endOfInput
  where
    item = Atto.char '"' *> many (Atto.satisfy (/= '"')) <* Atto.char '"'
       <|> many (Atto.satisfy (`notElem` [',', '"']))

accountLines :: (MonadIO m, MonadThrow m) => Consumer Text m [FeatureInfo]
accountLines = do
    header <- C.await `whenNothingM` error "Empty table"
    let features = columns header <&> \name ->
          fromMaybe FT.unset $ M.lookup (T.toLower name) FT.featuresTable
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

convertFeatures :: MonadIO m => [FeatureInfo] -> Conduit Text m Text
convertFeatures infos = do
    header <- C.await `whenNothingM` error "Empty table"

    -- place required feature at end
    lIdx <- L.findIndex (FT.lastFeature == ) (columns header)
                `whenNothing` error "Last feature not found"
    let permute = take lIdx <> drop (lIdx + 1) <> take 1 . drop lIdx

    convertLine permute convertFeatureName header

    putStrLn @Text "Process classes as following:"
    forM_ (zip (columns header) infos) $ \(hentry, info) ->
        putStrLn $ sformat (build%": "%build) hentry info

    putStrLn @Text "Converting..."
    C.awaitForever . convertLine permute $ map (sformat build) ... convertFeatureValue
    putStrLn @Text "Done"
      where
        convertLine permute converter =
            C.yield . T.intercalate " " . concat . permute . zipWith converter infos . columns
