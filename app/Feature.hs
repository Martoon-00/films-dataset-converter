{-# LANGUAGE ExistentialQuantification #-}

module Feature where

import           Control.Lens        (at, ix, non, (+~))
import           Data.List           (span)
import qualified Data.List           as L
import qualified Data.Map            as M
import qualified Data.Set            as S
import qualified Data.Text           as T
import qualified Data.Text.Buildable
import           Formatting          (bprint, build, sformat, stext, (%))
import           Universum

import qualified Color
import           Util                (explicitEmpty)

data FeatureInfo
    = Ignore Bool
    -- ^ don't include this feature into result,
    --   argument determines whether is was excluded manually
    | forall r. (Read r, Num r) => Modify Text (r -> Int)
    -- ^ value is to be modified
    | Classes (M.Map Text Int) Int
    -- ^ feature gets splitted to several binary features.
    --   values which have lesser value than first argument are merged
    | Ranges [Int]
    -- ^ feature is splitted into several binary features according to whether value
    --   fits into range. Argument keep range delimiters

instance Buildable FeatureInfo where
    build (Ignore byUser) =
        if byUser
        then Color.withColor Color.Magenta "Discarded"
        else Color.withColor Color.Red "Not specified"
    build (Modify desc _) = Color.withColor Color.Blue $ bprint stext desc
    build (Classes cls' mb) =
        let cls = mergeClasses mb cls'
            limit = 100
        in  Color.withColor' (Color.Dull, Color.White) $
            bprint (build%" classes (merge bound="%build%"): " %stext%stext)
                (length cls)
                mb
                (T.intercalate ", " $
                     uncurry (sformat (stext%" ("%build%")")) .
                     first explicitEmpty <$>
                     L.sortOn (Down . snd)
                     (take limit $ M.toList cls))
                (if length cls > limit then ", ..." else "")
    build (Ranges rng) =
        Color.withColor Color.Cyan $
        bprint ("..."%stext%"...") $ T.intercalate ".." $ show <$> rng

considerValue :: FeatureInfo -> Text -> FeatureInfo
considerValue (Classes cls mb) t = do
    let cls' = foldr (\x -> at x . non 0 +~ 1) cls $ T.splitOn "|" t
    Classes cls' mb
considerValue other           _ = other

mergeClasses :: Int -> M.Map Text Int -> M.Map Text Int
mergeClasses mb cls = do
    let (_, main) = L.break ((> mb) . snd) $ L.sortOn snd $ M.toList cls
    M.fromList main

convertFeatureName :: FeatureInfo -> Text -> [Text]
convertFeatureName Ignore{}        _ = []
convertFeatureName (Modify _ _)    n = [n]
convertFeatureName (Classes mb cls) n =
    M.keys (mergeClasses cls mb) <&> flip (sformat $ build%"("%build%")") n
convertFeatureName (Ranges _)      n = [n]

convertFeatureValue :: FeatureInfo -> Text -> [Int]
convertFeatureValue Ignore{}     _  = []
convertFeatureValue (Modify _ f) t  = pure . f . fromMaybe 0 $ readNumber t
convertFeatureValue (Classes cls _) t = do
    let cur = S.fromList $ T.splitOn "|" t
    M.keys cls <&> fromEnum . (`S.member` cur)
convertFeatureValue (Ranges rng) t =
    let value   = readNumber t
        rangeNo = fromMaybe (-1) $ value <&> \v -> length . fst $ span ( < v) rng
    in  (0 : rng $> 0) & ix rangeNo .~ 1

readNumber :: Read a => Text -> Maybe a
readNumber "" = Nothing
readNumber t  = Just . fromMaybe (error $ "Bad number " <> t) $
                readMaybe (toString t)
