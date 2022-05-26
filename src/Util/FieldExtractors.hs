{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Util.FieldExtractors where


import Generated.AbsMacchiato



class HasId a where
    getId :: a -> String


instance HasId FnDef where
    getId (FunDef _ _ (UIdent id) _ _) = id

instance HasId Arg where
    getId (ArgVal _ _ (UIdent id)) = id
    getId (ArgRef _ _ (UIdent id)) = id

