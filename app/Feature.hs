{-# LANGUAGE ExistentialQuantification #-}

module Feature where

import           Control.Lens        (ix)
import           Data.List           (span)
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
    | Classes (S.Set Text)
    -- ^ feature gets splitted to several binary features
    | Ranges [Int]
    -- ^ feature is splitted into several binary features according to whether value
    --   fits into range. Argument keep range delimiters

instance Buildable FeatureInfo where
    build (Ignore byUser) =
        if byUser
        then Color.withColor Color.Magenta "Discarded"
        else Color.withColor Color.Red "Not specified"
    build (Modify desc _) = Color.withColor Color.Blue $ bprint stext desc
    build (Classes cls) =
        let limit = 20
        in  Color.withColor' (Color.Dull, Color.White) $
            bprint (build%" classes: " %stext%stext)
                (length cls)
                (T.intercalate ", " $ sformat build . explicitEmpty <$>
                     (take limit $ S.toList cls))
                (if length cls > limit then ", ..." else "")
    build (Ranges rng) =
        Color.withColor Color.Cyan $
        bprint ("..."%stext%"...") $ T.intercalate ".." $ show <$> rng

considerValue :: FeatureInfo -> Text -> FeatureInfo
considerValue (Classes cl) t = Classes $ foldr S.insert cl $ T.splitOn "|" t
considerValue other        _ = other

convertFeatureName :: FeatureInfo -> Text -> [Text]
convertFeatureName Ignore{}      _ = []
convertFeatureName (Modify _ _)  n = [n]
convertFeatureName (Classes cls) n = S.toList cls <&> flip (sformat $ build%" ("%build%")") n
convertFeatureName (Ranges _)    n = [n]

convertFeatureValue :: FeatureInfo -> Text -> [Int]
convertFeatureValue Ignore{}     _  = []
convertFeatureValue (Modify _ f) t  = pure . f . fromMaybe 0 $ readNumber t
convertFeatureValue (Classes cls) t =
    let cur = S.fromList $ T.splitOn "|" t
    in  S.toList cls <&> fromEnum . (`S.member` cur)
convertFeatureValue (Ranges rng) t =
    let value   = readNumber t
        rangeNo = fromMaybe (-1) $ value <&> \v -> length . fst $ span ( < v) rng
    in  (0 : rng $> 0) & ix rangeNo .~ 1

readNumber :: Read a => Text -> Maybe a
readNumber "" = Nothing
readNumber t  = Just . fromMaybe (error $ "Bad number " <> t) $
                readMaybe (toString t)
