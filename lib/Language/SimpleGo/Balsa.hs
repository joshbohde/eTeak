{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |

module Language.SimpleGo.Balsa  (
  synthesizeFile,
  typeDecl,
  TranslateM, M.runTranslateT, pos, primExp, simpleExp, runExprCmd, ExprCmd(..)
  ) where

import           Control.Monad                        (forM_)
import           Control.Monad.Except                 (ExceptT (..), runExceptT,
                                                       withExceptT)
import           Control.Monad.Trans                  (lift, liftIO)
import           Data.Functor.Compose                 (Compose (..))
import           Data.Maybe                           (fromMaybe)
import           Data.Text                            (pack, unpack)
import qualified Data.Vector                          as U

import qualified Context                              as C
import           Control.Monad.Trans.Cont             (ContT (..), callCC,
                                                       mapContT, runContT)
import           Language.Helpers                     (bind, eval, finish, teak,
                                                       writeGates, writeTeak)
import           Language.SimpleGo.AST
import qualified Language.SimpleGo.AST.Operators      as Operators
import           Language.SimpleGo.Balsa.Builtins     (bool, byte, string)
import qualified Language.SimpleGo.Balsa.Builtins     as Builtins
import           Language.SimpleGo.Balsa.Declarations (Context, Decl)
import qualified Language.SimpleGo.Balsa.Declarations as D
import qualified Language.SimpleGo.Eval               as Eval
import           Language.SimpleGo.Monad              (declare, unsupported)
import qualified Language.SimpleGo.Monad              as M
import           Language.SimpleGo.Process            (compileFile)
import qualified ParseTree                            as PT
import           Print                                (showTree)
import qualified Report                               as R

type TranslateM = M.TranslateT IO Decl

-- ContT specialized for TranslateM
-- For reference: runContT :: (a -> m r) -> m r

-- This is necessary because some expressions need to set up the
-- appropriate cmd context before the next command can run. For
-- instance, calling a function literal requires a new block, with
-- definitions setup for the literal, before the literal can be called.
type Cont a b = ContT b TranslateM a

runExprCmd :: Cont a ExprCmd -> (a -> Maybe PT.Expr) -> TranslateM ExprCmd
runExprCmd ma f = runContT ma (return . ExprCmd Nothing . f)

getCmd :: ExprCmd -> PT.Cmd
getCmd (ExprCmd c _) = fromMaybe PT.NoCmd c

-- If we don't need the expression, we can just throw it away.
runSideEffects :: Cont a ExprCmd -> TranslateM PT.Cmd
runSideEffects ma = getCmd <$> runExprCmd ma (const Nothing)

-- Error if we require a command, otherwise return the expression
simpleExpression :: Cont (Maybe PT.Expr) ExprCmd -> TranslateM PT.Expr
simpleExpression ma = runExprCmd ma id >>= extract
  where
    extract e@(ExprCmd (Just c) _) = M.unsupported "expression requires commands to be run" e
    extract (ExprCmd _ (Just e)) = return e
    extract e = M.unsupported "ExprCmd" e



-- A data type representing the commands necessary to run before the
-- result of some expression is available. This is necessary because
-- Go's Statements can sometimes be either a Balsa expression or statement, and
-- Go's expressions can sometiems be either a Balsa expression or statement.
-- This is visible in function application, where a byte(..) is an expression (has a return type of byte),
data ExprCmd = ExprCmd
               (Maybe PT.Cmd) -- The commands necessary to run before the expression is available
               (Maybe PT.Expr) -- The expression
               deriving (Show, Eq)

pos :: R.Pos
pos = D.pos

synthesizeFile :: FilePath -> IO ()
synthesizeFile f = do
  e <- runExceptT go
  case e of
    Left s -> putStrLn s
    Right a -> return a
  where
    write s t = liftIO $ do
      putStrLn $ "running " ++ s
      writeFile (f ++ ".tree-" ++ s) $ showTree t
    go = do
      goSource <- ExceptT $ compileFile f
      balsa <- withExceptT show $ ExceptT $ M.runTranslateT $ asBalsa goSource
      write "parse" balsa
      bound <- bind balsa
      write "bind" bound
      evaled <- eval bound
      write "eval" evaled
      finished <- finish evaled
      write "finish" finished
      teak' <- teak [] finished
      writeTeak (f ++ ".teak") teak'
      writeGates f teak'

asBalsa :: Program -> TranslateM Context
asBalsa program = do
  -- Implementation bug: "String" must be defined
  declare "String" $ D.alias string
  root' program
  D.declContext <$> M.popContext

root' :: Program -> TranslateM ()
root' program = do
  forM_ Builtins.types $ \(n,t) -> declare (pack n) $ D.alias t
  forM_ (declarations program) declareTopLevel


balsaType :: Type -> TranslateM PT.Type
balsaType (TypeName id') = return $ PT.NameType pos (unId id')
balsaType (SliceType typ) = PT.ArrayType (PT.Interval (0,0) bool) <$> balsaType typ
balsaType (FunctionType _) = return PT.NoType
balsaType t = M.unsupported "type" t

typeDecl :: Type -> TranslateM D.Decl
typeDecl t@(TypeName _) = D.alias <$> balsaType t
typeDecl (Struct fields) = D.Type <$> record
  where
    record = PT.RecordType D.pos <$> traverse fieldDecl fields <*> pure PT.NoType
    fieldDecl :: (Id, Type) -> TranslateM PT.RecordElem
    fieldDecl (id', typ) = PT.RecordElem D.pos (unId id') <$> balsaType typ
typeDecl t = unsupported "type declaration" t

declareTopLevel :: Declaration -> TranslateM ()
declareTopLevel (Const (Id id') typ e) = do
  e' <- simpleExpression $ exprExp e
  declare id' $ D.Const e'
-- err, this should be type checked, but need to refactor decls
declareTopLevel (Var i typ' (Prim f@(LitFunc sig' block))) = declareTopLevel (Func i sig' block)
declareTopLevel (Var (Id id') _ (Prim (Make (Channel Bidirectional typ') []))) = do
  t' <- balsaType typ'
  declare id' $ D.Chan t'
declareTopLevel (Var (Id id') typ e) = do
  t <- balsaType typ
  case e of
    Zero -> do
      declare id' $ D.Var t Nothing
    _  -> do
      e' <- simpleExpression $ exprExp e
      declare id' $ D.Var t (Just e')
declareTopLevel (Type (Id id') typ) = do
  t <- typeDecl typ
  declare id' t
--declareTopLevel f = M.unsupported "top level binding" f
declareTopLevel f@(Func (Id id') sig block) = declare id' =<< decl
  where

    decl = if isProc sig
           then procedure
           else M.unsupported "function" f
    isProc (Signature i o) = True
    procedure = do
      M.newContext
      declareSig sig
      b <- blockCmd block
      sigDecl' <- M.popContext
      return $ D.Proc (D.declContext sigDecl') b

true :: PT.Value
true = PT.IntValue 1

binOp :: Binary -> TranslateM PT.BinOp
binOp Multiply = return PT.BinMul
binOp Quotient = return PT.BinDiv
binOp Remainder = return PT.BinMod
binOp Add = return PT.BinAdd
binOp Subtract = return PT.BinSub
binOp BitwiseAnd = return PT.BinAnd
binOp BitwiseOr  = return PT.BinOr
binOp BitwiseXor = return PT.BinXor
binOp LessThan = return PT.BinLT
binOp GreaterThan = return PT.BinGT
binOp LessThanEqual = return PT.BinLE
binOp GreaterThanEqual = return PT.BinGE
binOp NotEqual = return PT.BinNE
binOp Equal  = return PT.BinEQ
binOp o = M.unsupported "operator" o

unId :: Id -> String
unId (Id id') = unpack id'

sigDecl :: Type -> TranslateM D.Decl
sigDecl (Channel Input typ) = D.In <$> balsaType typ
sigDecl (Channel Output typ) = D.Out <$> balsaType typ
--sigDecl (Channel Bidirectional typ) = PT.ChanDecl pos <$> balsaType typ
sigDecl t@(TypeName _) = D.Param <$> balsaType t
sigDecl t = unsupported "signature type" t

declareParam :: Param -> TranslateM ()
declareParam (Param (Just (Id id')) t) = do
  t' <- sigDecl t
  declare id' t'
declareParam (Param Nothing t) = do
  t' <- sigDecl t
  declare "_" t'

declareSig :: Signature -> TranslateM ()
declareSig (Signature inputs _) = U.forM_ inputs declareParam

blockCmd :: Block -> TranslateM PT.Cmd
blockCmd (Block statements) = do
  M.newContext
  cmd' <- seqCmd $ U.toList statements
  c <- D.declContext <$> M.popContext
  return $ PT.BlockCmd pos c cmd'

seqCmd :: [Statement] -> TranslateM PT.Cmd
seqCmd ss = collapsePars <$> traverse parCmd ss


caseCmds :: [Case Expr]
           -> Cont ([PT.CaseCmdGuard], PT.Cmd) ExprCmd
caseCmds cs = (,) <$> explicits <*> lift def
  where
    isDefault (Default _) = True
    isDefault _ = False
    def = case filter isDefault cs of
      [] -> return PT.NoCmd
      (Default ss : _ ) -> seqCmd ss
      _ -> error "refactor `caseCmds` to be total"
    explicits = mapM cmdGuard $ filter (not . isDefault) cs
    cmdGuard (Case es ss) = PT.CaseCmdGuard pos <$> mapM match es <*> lift (seqCmd ss)
    cmdGuard _ = error "refactor `caseCmds` to be total"
    match e = PT.ExprCaseMatch pos <$> forceExp e




data Par a = Par a | Seq a

parCmd :: Statement -> TranslateM (Par PT.Cmd)
parCmd c@(Go _) = Par <$> cmd c
parCmd c = Seq <$> cmd c

collapsePars :: [Par PT.Cmd] -> PT.Cmd
collapsePars = go
  where
    isPar :: Par a -> Bool
    isPar (Par _) = True
    isPar _ = False

    seqs :: [Par a] -> ([Par a], [Par a])
    seqs = break isPar

    pars :: [Par a] -> ([Par a], [Par a])
    pars = span isPar

    undo :: Par a -> a
    undo (Par a) = a
    undo (Seq a) = a


    justCmds = filter (not . isNoCmd)
      where
        isNoCmd PT.NoCmd = True
        isNoCmd _ = False

    go  [] = PT.NoCmd
    go [Par c] = c
    go [Seq c] = c
    go (Par c : next) = p
      where
        (ps, ss) = pars next
        p = PT.ParCmd pos $ justCmds (c:(map undo ps ++ [s]))
        s = collapsePars ss
    go (Seq c : next) = s
      where
        (ss, ps) = seqs next
        s = PT.SeqCmd pos $ justCmds (c:(map undo ss ++ [p]))
        p = collapsePars ps

cmd :: Statement -> TranslateM PT.Cmd
cmd (Go p) = runSideEffects $ exprExp p
cmd (Simple s) = getCmd <$> simpleExp s
cmd (StmtBlock block) = blockCmd block
-- for { } construct is identical to loop ... end
cmd (ForWhile Nothing block) = PT.LoopCmd pos <$> blockCmd block
cmd (ForWhile (Just e) block) = runSideEffects $
  PT.WhileCmd pos PT.NoCmd <$> forceExp e <*> lift (blockCmd block)
-- for i := <start>; i < <end>; i++ { } is equivalent to a range
cmd f@(ForThree
       (SimpVar id' start)
       (Just (BinOp LessThan (Prim (Qual Nothing id'')) end))
       (Inc (Prim (Qual Nothing id''')))
       block)
  | id' == id'' && id' == id''' = b interval
  where
    b (Just i@(PT.Interval _ typ')) = do
       M.newContext
       declare (idText id') (D.Param typ')
       c <- D.declContext <$> M.popContext
       PT.ForCmd pos PT.Seq i c <$> blockCmd block
    b _ = unsupported "ForLoop" f
    interval = do
      (start', end') <- (,) <$> Eval.eval start <*> Eval.eval end
      case (start', end') of
        (Eval.IntegralR t s, Eval.IntegralR _ e) ->
          return $ PT.Interval (s, pred e) $ lookupType t
        _ -> Nothing
    lookupType Eval.Int8 = Builtins.int8
    lookupType Eval.Int16 = Builtins.int16
    lookupType Eval.Int32 = Builtins.int32
    lookupType Eval.Int64 = Builtins.int64
    lookupType Eval.Uint8 = Builtins.uint8
    lookupType Eval.Uint16 = Builtins.uint16
    lookupType Eval.Uint32 = Builtins.uint32
    lookupType Eval.Uint64 = Builtins.uint64
    lookupType Eval.GoInt = Builtins.int
    lookupType Eval.GoUint = Builtins.uint
cmd (If (Cond Nothing (Just expr)) block s) = runSideEffects $
  PT.CaseCmdE pos <$> forceExp expr <*> lift (fmap return trueBlock) <*> lift s'
  where
    s' = maybe (return PT.NoCmd) cmd s
    trueBlock = PT.CaseCmdGuard pos [PT.ExprCaseMatch pos (PT.ValueExpr pos bool true)] <$> blockCmd block
cmd (Switch (Cond Nothing (Just expr)) cases) = runSideEffects $ do
  (cs, def) <- caseCmds cases
  PT.CaseCmdE pos <$> forceExp expr <*> pure cs <*> pure def
cmd (StmtDecl decl) = do
  declareTopLevel decl
  return PT.NoCmd
cmd (StmtSelect cases) = PT.SelectCmd pos False <$> traverse chanCase cases
  where
    chanCase (Case as stmnts) = PT.ChanGuard pos <$> traverse chanGuard as <*> pure C.EmptyContext <*> seqCmd stmnts
      where
        chanGuard (ChanRecv Nothing (UnOp Receive (Prim (Qual Nothing id')))) = return $ PT.NameChan pos (unId id')
        chanGuard c = unsupported "guard" c
    chanCase d@(Default _) = unsupported "select" d
cmd s = unsupported "statment" s

type ExprT r = Cont (Maybe PT.Expr) r
type ExprFT r = Cont PT.Expr r

andThen :: PT.Cmd -> PT.Cmd -> PT.Cmd
a `andThen` (PT.SeqCmd p cmds) = PT.SeqCmd p (a : cmds)
a `andThen` b = PT.SeqCmd pos [a,b]

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

(<**>) :: (Applicative f, Applicative g) =>
         f (g (a -> b)) -> f (g a) -> f (g b)
a <**> b = getCompose $ Compose a <*> Compose b

mkExpr :: PT.Expr -> ExprT ExprCmd
mkExpr = return . Just

exprExp :: Expr -> ExprT ExprCmd
exprExp (Prim prim) = primExp prim
exprExp (UnOp Plus e) = PT.BinExpr pos PT.NoType PT.BinAdd (PT.ValueExpr pos PT.NoType (PT.IntValue 0)) <$$> exprExp e
exprExp (UnOp Minus e) = PT.BinExpr pos PT.NoType PT.BinSub (PT.ValueExpr pos PT.NoType (PT.IntValue 0)) <$$> exprExp e
exprExp (UnOp Not e) = PT.UnExpr pos PT.NoType PT.UnNot <$$> exprExp e
exprExp (BinOp op e e') = do
  op' <- lift (binOp op)
  PT.BinExpr pos PT.NoType op' <$$> exprExp e <**> exprExp e'
exprExp e = lift $ M.unsupported "expr" e

forceExp :: Expr -> ExprFT ExprCmd
forceExp e = do
  a <- exprExp e
  case a of
    Nothing -> lift $ M.unsupported "expr" e
    Just a' -> return a'

mapCmd :: (PT.Cmd -> PT.Cmd) -> ExprCmd -> ExprCmd
mapCmd f (ExprCmd mc e) = ExprCmd (Just (f (fromMaybe PT.NoCmd mc))) e

alterCmd :: (PT.Cmd -> PT.Cmd)
           -> Cont a ExprCmd -> Cont a ExprCmd
alterCmd f = mapContT (fmap (mapCmd f))

primExp :: Prim -> ExprT ExprCmd
primExp (LitInt i) = mkExpr $ PT.ValueExpr pos byte (PT.IntValue i)
primExp (LitStr s) = mkExpr $ PT.ValueExpr pos string (PT.StringValue (unpack s))
primExp (Qual (Just struct) id) = mkExpr $ PT.RecElemExpr pos (PT.NameExpr pos (unId struct)) (unId id)
primExp (Qual Nothing id') = mkExpr $ PT.NameExpr pos (unId id')
--primExp c@(Call (Qual Nothing (Id "append")) [arg] (Just f)) = callCC $ PT.AppendExpr pos PT.NoType <$> toExpr arg <*> toExpr f
---- special case type coercions and builtins
--primExp c@(Call (Qual Nothing id') [arg] Nothing) = case lookup (unId id') Builtins.types of
--  Just typ -> callCC $ PT.CastExpr pos typ <$> toExpr arg
--  Nothing -> case lookup (unId id') Builtins.functions of
--    Just f -> callCC $ f <$> toExpr arg
--    Nothing -> M.unsupported "call expression" c
primExp (LitFunc sig block) = do
  (c, i) <- lift $ do
    M.newContext
    id' <- M.fresh
    declareTopLevel $ Func (Id id') sig block
    c' <- D.declContext <$> M.popContext
    return (c', id')
  alterCmd (PT.BlockCmd pos c) (mkExpr $ PT.NameExpr pos (unpack i))
primExp (Call p es me) = do
  callable <- primExp p
  case callable of
    (Just (PT.NameExpr _ id')) -> do
      exprs <- mapM forceExp es
      let
        call = PT.CallCmd pos (PT.NameCallable pos id') C.EmptyContext $ map PT.ExprProcActual exprs
      alterCmd (call `andThen`) (return Nothing)
    _ -> lift $ M.unsupported "callable" callable
primExp s@(Slice p me (Just end)) = slice
  where
    slice = PT.SliceExpr pos <$$> primExp p <**> exprExp start <**> exprExp end
    start = fromMaybe (Prim (LitInt 0)) me
primExp s@(Slice _ _ Nothing) = lift $ M.unsupported "slice expression with no end" s
primExp (Index p e) = PT.IndexExpr pos <$$> primExp p <**> exprExp e
primExp (Paren e) = exprExp e
primExp s = lift $ M.unsupported  "primitive" s

simpleExp :: Simp -> TranslateM ExprCmd
simpleExp Empty = return $ ExprCmd Nothing Nothing
simpleExp (Inc e) = simpleExp $ Assign e $ BinOp Operators.Add e (Prim (LitInt 1))
simpleExp (Dec e) = simpleExp $ Assign e $ BinOp Operators.Subtract e (Prim (LitInt 1))
simpleExp (Send chan e) = runContT ma return
  where
    ma = do
      chan <- exprExp chan
      case chan of
        (Just (PT.NameExpr p id)) -> do
          c <- PT.OutputCmd pos (PT.NameChan pos id) <$> forceExp e
          return $ ExprCmd (Just c) Nothing
        _ -> lift $ unsupported "chan expression" chan
--simpleExp (SimpVar (Id id') (UnOp Receive (Prim (Qual Nothing (Id chan))))) = do
--  chan' <- M.lookup chan
--  case chan' of
--    Nothing -> M.notDefined (unpack chan)
--    Just t -> do
--      r <- receiveType t
--      declare id' (D.Var r Nothing)
--      return $ PT.InputCmd pos (PT.NameChan pos (unpack chan)) (PT.NameLvalue pos (unpack id'))
--    where
--      receiveType :: D.Decl -> TranslateM PT.Type
--      receiveType (D.In t) = return t
--      receiveType (D.Chan t) = return t
--      receiveType s = M.typeError "chan or <-chan" (show s)
simpleExp (SimpleExpr e) = runExprCmd (exprExp e) id
simpleExp s = unsupported "simple expression " s


data Args = Args [Expr] (Maybe Expr)

--print :: S.Args -> TranslateM PT.Cmd
--print (S.Args es (Just e)) = runSideEffects $ do
--  args <- mapM forceExp (es ++ [e])
--  return $ PT.PrintCmd pos args
--print (S.Args es Nothing) = runSideEffects $ do
--  args <- mapM forceExp es
--  return $ PT.PrintCmd pos args
