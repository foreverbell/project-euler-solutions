{-# LANGUAGE TemplateHaskell #-}

module Common.NumMod.MkNumMod (
  mkNumMod
) where

import Language.Haskell.TH
import Data.Vector.Unboxed.Deriving

apprem :: Int -> Bool -> Exp -> Exp
apprem n needMod e = UInfixE (ParensE e) remE (LitE (IntegerL $ fromIntegral n))
  where 
    remE = if needMod then VarE 'mod else VarE 'rem

inline :: Name -> [Dec] -> [Dec]
inline name = (:) pragma
  where
    pragma = PragmaD (InlineP name Inline FunLike AllPhases)

wrapper :: Int -> Name
wrapper n = mkName $ "Int" ++ show n

unwrapper :: Int -> Name
unwrapper n = mkName $ "fromInt" ++ show n

wrap :: Int -> Exp -> Exp
wrap n e = RecConE (wrapper n) [(unwrapper n, e)]

unwrap :: Int -> Exp -> Exp
unwrap n e = VarE (unwrapper n) `AppE` e

mkVar :: Int -> Name -> Exp
mkVar n = unwrap n . VarE

mkShow :: Int -> [Dec]
mkShow n = inline m [ FunD m [ Clause [VarP x] (NormalB $ VarE 'show `AppE` apprem n True (mkVar n x)) [] ] ]
  where
    m = mkName "show"
    x = mkName "x"

mkBOp :: Int -> Bool -> Exp -> Name -> [Dec]
mkBOp n canOverflow0 op opName = inline opName [ FunD opName [ Clause [VarP a, VarP b] (NormalB $ wrap n . dropInteger . apprem n False $ UInfixE var1 op var2) [] ] ]
  where
    a = mkName "a"
    b = mkName "b"
    canOverflow = n >= 2^31 && canOverflow0
    liftInteger | canOverflow = AppE (VarE 'toInteger)
                | otherwise = id
    dropInteger | canOverflow = AppE (VarE 'fromInteger)
                | otherwise = id
    var1 = liftInteger $ mkVar n a
    var2 = liftInteger $ mkVar n b

mkAddition :: Int -> [Dec]
mkAddition n = mkBOp n False (VarE '(+)) (mkName "+")

mkSubtract :: Int -> [Dec]
mkSubtract n = mkBOp n False (VarE '(-)) (mkName "-")

mkMultiply :: Int -> [Dec]
mkMultiply n = mkBOp n True (VarE '(*)) (mkName "*")

mkFromInteger :: Int -> [Dec]
mkFromInteger n = inline m [ FunD m [ Clause [VarP a] (NormalB $ wrap n e) [] ] ]
  where
    m = mkName "fromInteger"
    a = mkName "a"
    e = VarE 'fromIntegral `AppE` apprem n False (VarE a)

mkUndefined :: Name -> [Dec]
mkUndefined m = [ FunD m [ Clause [WildP] (NormalB $ VarE 'undefined) [] ] ]

mkAbs :: Int -> [Dec]
mkAbs _ = mkUndefined m
  where
    m = mkName "abs"

mkSignum :: Int -> [Dec]
mkSignum _ = mkUndefined m
  where
    m = mkName "signum"

mkNumMod :: Bool -> Int -> DecsQ
mkNumMod enableUnbox n = do
  let typeName = wrapper n
  let typeNum = DataD [] typeName [] [RecC typeName [(unwrapper n, Unpacked, ConT ''Int)]] []
  let instanceShow = InstanceD [] (ConT ''Show `AppT` ConT typeName) (mkShow n)
  let instanceNum = InstanceD [] (ConT ''Num `AppT` ConT typeName) $ 
        concatMap ($ n) [mkAddition, mkSubtract, mkMultiply, mkFromInteger, mkAbs, mkSignum]
  instanceUnbox <- do
    let var = mkName "x"
    if enableUnbox
       then derivingUnbox (show typeName) 
              (return $ ArrowT `AppT` ConT typeName `AppT` ConT ''Int)
              (return $ LamE [VarP var] (unwrap n (VarE var)))
              (return $ LamE [VarP var] (wrap n (VarE var)))
       else return []
  return $ [typeNum, instanceShow, instanceNum] ++ instanceUnbox

{- mkNumMod True 42 
     ======>
   data Int42 = Int42 { fromInt42 :: {-# UNPACK #-} !Int }
   instance Show Int42
   instance Num Int42 (@abs@ and @signum@ are set to undefined)
   instances for Vector Unbox (optional)
-}
