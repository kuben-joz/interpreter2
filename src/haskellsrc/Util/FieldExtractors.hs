{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Util.FieldExtractors where


import Parsing.AbsMacchiato



class HasId a where
    getId :: a -> String


instance HasId FnDef where
    getId (FunDef _ _ (Ident id) _ _) = id

instance HasId Arg where
    getId (ArgVal _ _ (Ident id)) = id

