module Utils where

interlace :: [a] -> [a] -> [a]
interlace [] [] = []
interlace (a:as) (b:bs) = a:b:(interlace as bs)

interlace3 as bs cs = interlaceN [as, bs, cs]

interlaceN [] = []
interlaceN ([]:_) = []
interlaceN ((a:as):xs) = a:(interlaceN $ xs ++ [as])

map2 :: (a -> c) -> (b -> d) -> [(a, b)] -> [(c, d)]
map2 f g = map (\(a, b) -> (f a, g b))


map2M :: Monad m => (a -> m c) -> (b -> m d) -> [(a, b)] -> m [(c, d)]
map2M f g = mapM (\(a, b) -> do c <- f a
                                d <- g b
                                return (c, d)) 

listWithout n list = let (before, after) = splitAt n list
                      in before ++ (tail after)


