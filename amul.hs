{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
import Control.Comonad
import Control.Monad
import Data.Stream ( Stream(..) )
import qualified Data.Stream as S

import Prelude hiding ( iterate, take, Right, Left )
import Data.List hiding ( iterate )
import Data.Bifunctor
import Data.Maybe

data Tape a =
    Tape {
        viewL :: Stream a,
        cursor :: a,
        viewR :: Stream a
    } deriving (Functor)

unfold :: (c -> (a, c))
        -> (c -> a)
        -> (c -> (a, c))
        -> c
        -> Tape a
unfold prev center next =
    Tape <$> S.unfold prev <*> center <*> S.unfold next

iterate :: (a -> a) -> (a -> a) -> a -> Tape a
iterate prev next =
    unfold (dup . prev) id (dup . next)
    where dup a = (a, a)

instance Comonad Tape where
    extract = cursor
    duplicate = iterate moveL moveR

moveL, moveR :: Tape a -> Tape a
moveL (Tape (Cons l ls) c rs) = Tape ls l (Cons c rs)
moveR (Tape ls c (Cons r rs)) = Tape (Cons c ls) r rs

overwrite :: a -> Tape a -> Tape a
overwrite a (Tape l _ r) = Tape l a r

data Move = Left | Right

newtype TMStep a = TMStep (a -> (a, Maybe Move, Maybe (TMStep a)))

halt :: TMStep a
halt = TMStep (, Nothing, Nothing)

ifEqElse :: (Eq a) => a -> TMStep a -> TMStep a -> TMStep a
ifEqElse test (TMStep thenStep) (TMStep elseStep) =
    TMStep $ \char ->
        if char == test
            then thenStep char
            else elseStep char

proceed :: TMStep a -> TMStep a
proceed step = TMStep (, Nothing, Just step)

move :: Move -> TMStep a -> TMStep a
move Left step = TMStep (, return Left, Just step)
move Right step = TMStep (, return Right, Just step)

replace :: (a -> a) -> TMStep a -> TMStep a
replace f (TMStep step) = TMStep $ step . f

initTape :: [a] -> Tape (Maybe a)
initTape =
    unfold
        (const (Nothing, Nothing))
        (fmap fst)
        (maybe (Nothing, Nothing) (helper . snd))
        . uncons
    where
        helper [] = (Nothing, Nothing )
        helper (x:xs) = (Just x, Just (x, xs))

goTil :: (Eq a) => Move -> a -> TMStep a -> TMStep a
goTil dir test step =
    ifEqElse test
        step
        (move dir $ goTil dir test step)

goTilNot :: Eq t => Move -> t -> TMStep t -> TMStep t
goTilNot dir test step =
    ifEqElse test
        (move dir $ goTilNot dir test step)
        step

mark :: Move -> (a -> Maybe a) -> TMStep a -> TMStep a
mark dir fn next =
    TMStep $ \char ->
        case fn char of
            Nothing -> (char, Nothing, Just next)
            Just r -> (r, Just dir, Just $ mark dir fn next)

replacing :: (Eq a) => a -> a -> (a -> Maybe a)
replacing x x' i
    | i == x = return x'
    | otherwise = Nothing

allExcept :: (Eq a) => a -> a -> (a -> Maybe a)
allExcept t r i
    | t /= i = return r
    | otherwise = Nothing

clearAll :: Eq a => TMStep (Maybe a) -> TMStep (Maybe a)
clearAll next =
    ifEqElse Nothing
        (proceed next)
        $ replace (const Nothing) (move Left $ clearAll next)

step :: Tape a -> TMStep a -> (Tape a, Maybe (TMStep a))
step tape (TMStep st) =
    case st (cursor tape) of
        (c, Nothing, next) -> (overwrite c tape, next)
        (c, Just Right, next) -> (moveR $ overwrite c tape, next)
        (c, Just Left, next) -> (moveL $ overwrite c tape, next)

instance Show (Tape (Maybe Char)) where
    show (Tape l c r) =
        reverse (map (fromMaybe ' ') $ S.takeWhile (not . null) l)
        ++ ">" ++ maybe " " (:[]) c
        ++ map (fromMaybe ' ') (S.takeWhile (not . null) r)

run :: Tape (Maybe Char) -> TMStep (Maybe Char) -> IO ()
run tape start = do
    print tape
    getLine
    let (tape', step') = step tape start
    case step' of
        Nothing -> print tape'
        Just s -> run tape' s

mul1 :: TMStep (Maybe Char)
mul1 = goTil Right Nothing mul2

mul2 :: TMStep (Maybe Char)
mul2 = move Left mul3

mul3 ::TMStep (Maybe Char)
mul3 =
    ifEqElse (Just '2')
        (replace (const Nothing) $ move Left mul4)
        $ clearAll halt

mul4 :: TMStep (Maybe Char)
mul4 = 
    ifEqElse (Just '1') halt
        $ ifEqElse (Just '5') mul9
            $ ifEqElse Nothing halt
                $ replace (const Nothing)
                    $ move Left mul41

mul41 :: TMStep (Maybe Char)
mul41 = 
    ifEqElse Nothing 
        (move Right mul42)
        $ ifEqElse (Just '1') 
            mul5
            $ move Left mul41

mul42 :: TMStep (Maybe Char)
mul42 =
    ifEqElse (Just '3')
        (replace (const $ Just '1') $ move Right mul42)
        $ goTil Right Nothing $ move Left mul4

mul5 :: TMStep (Maybe Char)
mul5 = replace (const $ Just '3') $ move Right mul6

mul6 :: TMStep (Maybe Char)
mul6 = goTilNot Right (Just '3') mul61

mul61 :: TMStep (Maybe Char)
mul61 = goTilNot Right (Just '5') mul7

mul7 :: TMStep (Maybe Char)
mul7 =
    ifEqElse Nothing 
        (replace (const $ Just '5') $ move Left mul41)
        $ replace (const $ Just '5') $ move Right mul8

mul8 :: TMStep (Maybe Char)
mul8 = goTil Right Nothing 
    $ replace (const $ Just '2') 
        $ move Left mul41

mul9 :: TMStep (Maybe Char)
mul9 = mark Left (replacing (Just '5') (Just '1')) halt