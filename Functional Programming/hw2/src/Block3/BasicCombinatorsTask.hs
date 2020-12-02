{-# LANGUAGE LambdaCase #-}

-- | Contains basic parser combinators.
module Block3.BasicCombinatorsTask (ok, eof, satisfy, element, stream) where

import Block3.ParserCombinatorTask (Parser (..))

-- | Accepts any input without reading it.
ok :: Parser s ()
ok = Parser $ \s -> Just ((), s)

-- | Accepts end of file.
eof :: Parser s ()
eof = Parser $ \s ->
  if null s
    then Just ((), s)
    else Nothing

-- | Accepts a stream element, if it satisfies the provided predicate.
satisfy :: (s -> Bool) -> Parser s s
satisfy pr = Parser $ \case
                        [] -> Nothing
                        (x : xs) -> if pr x then Just (x, xs) else Nothing

-- | Accepts a stream element, if it is equal to the provided one.
element :: Eq s => s -> Parser s s
element c = satisfy (== c)

-- | Accepts several stream elements, if they are equal to the provided stream.
stream :: Eq s => [s] -> Parser s [s]
stream = foldr (\ x -> (<*>) ((:) <$> element x)) (pure [])
