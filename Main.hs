{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Main where

main :: IO ()
main = putStrLn $ show $ 
     ( polymorphicAddition 1 :: Int
     , polymorphicAddition 1 2 3 :: Int
      , polymorphicListCreation 1 2 3 :: [Int]
     ,  polymorphicListCreation [1,2] [2] [3] :: [[Int]]
     ,  polymorphicConcatenation "Hello" "World" :: String
    )
class ArgumentSummation a where 
       polyAdd' :: [Integer] -> a

instance (Integral a, ArgumentSummation r) => ArgumentSummation (a -> r) where 
    polyAdd' is i =  polyAdd' ((toInteger i):is) 

instance ArgumentSummation Int where 
     polyAdd' = fromInteger . sum

polymorphicAddition :: ArgumentSummation args => args 
polymorphicAddition = polyAdd' [] 
-- 
class ArgumentStringConcatenation r where
  polymorphicConcatenation' :: String -> r
  
instance ArgumentStringConcatenation String where
  polymorphicConcatenation' = id

instance (ArgumentStringConcatenation r) => ArgumentStringConcatenation (String -> r) where
  polymorphicConcatenation' s c = if length s == 0 then  polymorphicConcatenation' (s ++ c) else polymorphicConcatenation' (s ++ " " ++ c)

polymorphicConcatenation :: ArgumentStringConcatenation r => r
polymorphicConcatenation = polymorphicConcatenation' ""
-- 
class ArgumentListCreation a r | r -> a  where 
  polymorphicListCreation' :: ([a] -> [a]) -> r

instance ArgumentListCreation a [a] where 
  polymorphicListCreation' f = f [] 
  
instance (ArgumentListCreation a r) => ArgumentListCreation a (a -> r) where 
  polymorphicListCreation' f x = polymorphicListCreation' (f . (:) x)

polymorphicListCreation :: (ArgumentListCreation a r) => r
polymorphicListCreation = polymorphicListCreation' id 
