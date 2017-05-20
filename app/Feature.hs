{-# LANGUAGE ExistentialQuantification #-}

module Feature where

import qualified Color
import qualified Data.Set            as S
import qualified Data.Text           as T
import qualified Data.Text.Buildable
import           Formatting          (bprint, build, sformat, stext, (%))
import           Universum

import           Util                (explicitEmpty)

data FeatureInfo
    = Ignore Bool
    -- ^ don't include this feature into result,
    --   argument determines whether is was excluded manually
    | forall r. Read r => Modify Text (r -> Int)
    -- ^ value is to be modified
    | Classes (S.Set Text)
    -- ^ feature gets splitted to several binary features

instance Buildable FeatureInfo where
    build (Ignore byUser) =
        if byUser
        then Color.withColor Color.Magenta "Discarded"
        else Color.withColor Color.Red "Not specified"
    build (Modify desc _)  = Color.withColor Color.Blue $ bprint stext desc
    build (Classes cls) =
        let limit = 20
        in  Color.withColor' (Color.Dull, Color.White) $
            bprint (build%" classes: " %stext%stext)
                (length cls)
                (T.intercalate ", " $ sformat build . explicitEmpty <$>
                     (take limit $ S.toList cls))
                (if length cls > limit then ", ..." else "")

considerValue :: FeatureInfo -> Text -> FeatureInfo
considerValue i@Ignore{}   _ = i
considerValue (Classes cl) t = Classes $ foldr S.insert cl $ T.splitOn "|" t
considerValue (Modify d f) _ = Modify d f

convertFeatureName :: FeatureInfo -> Text -> [Text]
convertFeatureName Ignore{}      _ = []
convertFeatureName (Modify _ _)  n = [n]
convertFeatureName (Classes cls) n = S.toList cls <&> flip (sformat $ build%" ("%build%")") n

convertFeatureValue :: FeatureInfo -> Text -> [Int]
convertFeatureValue Ignore{}     _  = []
convertFeatureValue (Modify _ _) "" = [0]
convertFeatureValue (Modify _ f) t  =
    pure . f . fromMaybe (error $ "Bad number " <> t) $ readMaybe (toString t)
convertFeatureValue (Classes cls) t =
    let cur = S.fromList $ T.splitOn "|" t
    in  S.toList cls <&> fromEnum . (`S.member` cur)

