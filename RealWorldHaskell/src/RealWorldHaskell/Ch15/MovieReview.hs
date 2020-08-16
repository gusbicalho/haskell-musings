module RealWorldHaskell.Ch15.MovieReview where

import Control.Applicative(liftA3)

type Params = [(String, Maybe String)]

getParam :: Params -> String -> Maybe String
getParam params key = case lookup key params of
                        Just s@(Just _) -> s
                        _ -> Nothing

data MovieReview = MovieReview {
      revTitle :: String
    , revUser :: String
    , revReview :: String
    } deriving (Eq, Show)

toReview :: Params -> Maybe MovieReview
toReview params = liftA3 MovieReview (p "title")
                                     (p "user")
                                     (p "review")
  where p = getParam params

-- >>> toReview [("title", Just "Borg"), ("user", Just "Picard"), ("review", Just "Nope.")]
-- Just (MovieReview {revTitle = "Borg", revUser = "Picard", revReview = "Nope."})
--
