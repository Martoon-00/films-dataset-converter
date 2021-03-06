{-# LANGUAGE ScopedTypeVariables #-}

module FeatureTable where

import qualified Data.Map  as M
import           Universum

import qualified Color
import           Feature   (FeatureInfo (..))


classes, unset, discard, numeric :: FeatureInfo
classes = Classes mempty 0
unset   = Ignore False
discard = Ignore True
numeric = Modify (Color.withColor' (Color.Dull, Color.Green) "Remain as is")
                  identity

mainClasses :: Int -> FeatureInfo
mainClasses = Classes mempty

(>:) :: a -> b -> (a, b)
(>:) = (,)

featuresTable :: M.Map Text FeatureInfo
featuresTable = M.fromList
    [ -- general
      "color"                     >: classes
    , "genres"                    >: mainClasses 96
    , "duration"                  >: unset
    , "movie_title"               >: discard

      -- statistics
    , "gross"                     >: numeric
    , "budget"                    >: numeric
    , "num_critic_for_reviews"    >: discard
    , "num_user_for_reviews"      >: discard
    , "num_voted_users"           >: numeric
    , "director_facebook_likes"   >: numeric
    , "actor_1_facebook_likes"    >: discard
    , "actor_2_facebook_likes"    >: discard
    , "actor_3_facebook_likes"    >: discard
    , "cast_total_facebook_likes" >: discard
    , "movie_facebook_likes"      >: discard

      -- participants
    , "director_name"             >: mainClasses 10
    , "actor_1_name"              >: mainClasses 15
    , "actor_2_name"              >: mainClasses 10
    , "actor_3_name"              >: mainClasses 5

      -- misc
    , "plot_keywords"             >: mainClasses 10
    , "facenumber_in_poster"      >: discard
    , "movie_imdb_link"           >: discard
    , "language"                  >: mainClasses 10
    , "country"                   >: mainClasses 100
    , "aspect_ratio"              >: discard
    , "imdb_score"                >: Modify "Multiply by 10"
                                     (\(k :: Double) -> round $ k * 10)
    , "content_rating"            >: mainClasses 50
    , "title_year"                >: Ranges [2000, 2010]
    ]

-- | Feature which represents 'Y' set and should be placed at last position
lastFeature :: Text
lastFeature = "imdb_score"
