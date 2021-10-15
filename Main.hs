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
    
-- store a list of integers and then sum them at the end. Return Int Type
class ArgumentSummation a where 
       polyAdd' :: [Integer] -> a

instance (Integral a, ArgumentSummation r) => ArgumentSummation (a -> r) where 
    polyAdd' is i =  polyAdd' ((toInteger i):is) 

instance ArgumentSummation Int where 
     polyAdd' = fromInteger . sum -- this is where the summation of [Integer] happens. Int type is returned

polymorphicAddition :: ArgumentSummation args => args 
polymorphicAddition = polyAdd' [] -- Needed for `polymorphicAddition` call to return 0 when called with 0 args

-- Creating one unified string 
class ArgumentStringConcatenation r where
  polymorphicConcatenation' :: String -> r
  
instance ArgumentStringConcatenation String where
  polymorphicConcatenation' = id

instance (ArgumentStringConcatenation r) => ArgumentStringConcatenation (String -> r) where
  polymorphicConcatenation' s c = s ++ c

polymorphicConcatenation :: ArgumentStringConcatenation r => r
polymorphicConcatenation = polymorphicConcatenation' "" -- Needed for `polymorphicConcatenation` call to return "" when called with 0 args

-- creating one unified list  
class ArgumentListCreation a r | r -> a  where 
  polymorphicListCreation' :: ([a] -> [a]) -> r

instance ArgumentListCreation a [a] where 
  polymorphicListCreation' f = f [] 
  
instance (ArgumentListCreation a r) => ArgumentListCreation a (a -> r) where 
  polymorphicListCreation' f x = polymorphicListCreation' (f . (:) x)

polymorphicListCreation :: (ArgumentListCreation a r) => r
polymorphicListCreation = polymorphicListCreation' id -- when called with zero args this function will be run 
