{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-- |

module Language.SimpleGo.Monad (
  TranslateT, Msg(..),
  runTranslateT, unsupported, declare, popContext, newContext, notDefined,
  lookup, lookup', fresh
  ) where

import           Control.Monad.Except                 (ExceptT (..), runExceptT,
                                                       throwError)
import           Control.Monad.State                  (StateT, evalStateT, gets,
                                                       modify')
import           Data.Monoid                          ((<>))
import qualified Data.Text                            as T
import           Language.SimpleGo.AST.Name           (Name (..))
import qualified Language.SimpleGo.Balsa.Declarations as Declarations
import qualified Language.SimpleGo.Balsa.Types        as Types
import           Language.SimpleGo.Balsa.Unique       (UniqueSupply (..))
import qualified Language.SimpleGo.Env                as Env
import           Prelude                              hiding (lookup)

data Msg = Unsupported String
         | EnvironmentError Env.Error
         | TypeError Types.TypeError
         | Undefined String
         deriving (Show, Eq)


data TranslationState decl = TranslationState {
  env    :: Env.Env decl,
  idents :: Integer
} deriving (Show, Eq)

modifyEnv :: (Env.Env a -> Env.Env a) -> TranslationState a -> TranslationState a
modifyEnv f t = t{env=f $ env t}

def :: TranslationState decl
def = TranslationState {
  env = Env.new,
  idents = 0
  }

type TranslateT m decl = ExceptT Msg (StateT (TranslationState decl) m)

instance (Monad m) => Types.TypeNamespace (TranslateT m Declarations.Decl) where
  lookup (Name t) = do
    decl <- lookup t
    case decl of
      Nothing -> Types.typeError $ Types.Unfound (Name t)
      Just (Declarations.Type a) -> return a
      Just _ -> throwError $ Unsupported $ T.unpack t ++ " is not a type"
  declare (Name i) t = declare i (Declarations.Type t)
  typeError = throwError . TypeError




runTranslateT :: (Monad m) => TranslateT m decl a -> m (Either Msg a)
runTranslateT m =  evalStateT (runExceptT m) def

unsupported :: (Monad m, Show a) => String -> a -> TranslateT m decl b
unsupported construct a = throwError $ Unsupported $ "unsupported " ++ construct ++ " :" ++ show a

notDefined :: (Monad m) => String -> TranslateT m decl a
notDefined id' = throwError $ Undefined id'

declare :: (Monad m) => T.Text -> decl -> TranslateT m decl ()
declare t d = do
  env' <- gets env
  case Env.insert env' t d of
    Left err -> throwError $ EnvironmentError err
    Right e -> modify' $ modifyEnv (const e)

newContext :: (Monad m) => TranslateT m decl ()
newContext = modify' (modifyEnv Env.fresh)

lookup :: (Monad m) => T.Text -> TranslateT m decl (Maybe decl)
lookup t = do
  env' <- gets env
  return $ Env.lookup env' t

lookup' :: (Monad m) => T.Text -> TranslateT m decl decl
lookup' t = do
  i <- lookup t
  case i of
    Just a -> return a
    Nothing -> notDefined (T.unpack t)

popContext :: (Monad m) => TranslateT m decl [(T.Text, decl)]
popContext = do
  env' <- gets env
  case Env.pop env' of
    Left err -> throwError $ EnvironmentError err
    Right (context, e)  -> do
      modify' $ modifyEnv (const e)
      return context

fresh :: (Monad m) => TranslateT m decl T.Text
fresh = do
  i <- gets idents
  modify' $ \t -> t{idents = succ i}
  return $ "$:" <> T.pack (show i)

instance (Monad m) => UniqueSupply (TranslateT m decl) T.Text where
  unique = fresh
