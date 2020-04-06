https://stackoverflow.com/questions/39438176/when-is-it-useful-to-use-strict-wildcard-in-haskell-and-what-does-it-do?rq=1
The Bang is there to make `unsafeTake` strict in its first argument. Not for the semantics of `unsafeTake undefined []`, but for optimization. Without the bang, the first pattern does not require the first argument, even if later patterns require it.

The strictness analyzer shows the difference

    >ghc StrictnessAnalysis -O2 -ddump-stranal
    ...
    unsafeTake [Occ=LoopBreaker]
      :: forall a_amS. Int -> [a_amS] -> [a_amS]
    [LclId,
     Arity=2,
     CallArity=2,
     Str=DmdType <S,1*U(1*U)><S,1*U>,

whereas without the bang, the DemandType of the first argument changes from strict (`S`) to lazy (`L`).

    unsafeTake [Occ=LoopBreaker]
      :: forall a_an1. Int -> [a_an1] -> [a_an1]
    [LclId,
     Arity=2,
     CallArity=2,
     Str=DmdType <L,1*U(1*U)><S,1*U>,

{-# LANGUAGE BangPatterns #-}

unsafeTake :: Int -> [a] -> [a]
unsafeTake  _  []     = []
unsafeTake 1   (x: _) = [x]
unsafeTake m   (x:xs) = x : unsafeTake (m - 1) xs

main = print $ unsafeTake 42 ""
