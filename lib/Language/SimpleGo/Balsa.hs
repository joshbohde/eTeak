{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |

module Language.SimpleGo.Balsa  (
  synthesizeFile,
  TranslateM, M.runTranslateT, pos, primExp, simpleExp, runExprCmd, ExprCmd(..), TypedExpr(..)
  ) where

import           Control.Monad                        (forM, forM_, (>=>))
import           Control.Monad.Except                 (ExceptT (..), runExceptT,
                                                       withExceptT)
import           Control.Monad.Trans                  (lift, liftIO)
import           Control.Monad.Trans.Cont             (ContT (..), mapContT,
                                                       runContT)
import           Data.Functor.Compose                 (Compose (..))
import           Data.Maybe                           (fromMaybe)
import           Data.Text                            (pack, unpack)
import           Data.Vector                          ((!?))
import qualified Data.Vector                          as U

import qualified Context                              as C
import           Language.Helpers                     (bind, eval, finish, teak,
                                                       writeGates, writeTeak)
import           Language.SimpleGo.AST
import           Language.SimpleGo.AST.Name           (Name (..), name)
import qualified Language.SimpleGo.AST.Operators      as Operators
import           Language.SimpleGo.Balsa.Builtins     (bool, byte, string)
import qualified Language.SimpleGo.Balsa.Builtins     as Builtins
import           Language.SimpleGo.Balsa.Declarations (Context)
import qualified Language.SimpleGo.Balsa.Declarations as D
import qualified Language.SimpleGo.Balsa.Functions    as Func
import           Language.SimpleGo.Balsa.Types        (canonicalType, (=?))
import qualified Language.SimpleGo.Balsa.Types        as BT
import qualified Language.SimpleGo.Eval               as Eval
import           Language.SimpleGo.Monad              (declare, unsupported)
import qualified Language.SimpleGo.Monad              as M
import           Language.SimpleGo.Process            (compileFile)
import qualified Language.SimpleGo.Types              as Types
import qualified ParseTree                            as PT
import           Print                                (showTree)
import qualified Report                               as R

newtype Call = FuncCall { call :: [TypedExpr] -> Maybe TypedExpr -> (Maybe PT.Cmd, Maybe TypedExpr) }

instance Show Call where
  show _ = ""

type Decl = D.Decl Call

type TranslateM = M.TranslateT IO Decl

-- ContT specialized for TranslateM
-- For reference: runContT :: (a -> m r) -> m r

-- This is necessary because some expressions need to set up the
-- appropriate cmd context before the next command can run. For
-- instance, calling a function literal requires a new block, with
-- definitions setup for the literal, before the literal can be called.
type Cont a b = ContT b TranslateM a

getCmd :: ExprCmd -> PT.Cmd
getCmd (ExprCmd c _) = fromMaybe PT.NoCmd c

runExprCmd :: Cont (Maybe TypedExpr) ExprCmd -> TranslateM ExprCmd
runExprCmd ma = runContT ma (return . ExprCmd Nothing)

-- If we don't need the expression, we can just throw it away.
runSideEffects :: Cont PT.Cmd ExprCmd -> TranslateM PT.Cmd
runSideEffects ma = getCmd <$> runContT mb return
  where
    mb = do
      c <- ma
      return $ ExprCmd (Just c) Nothing

-- Error if we require a command, otherwise return the expression
simpleExpression :: Cont (Maybe TypedExpr) ExprCmd -> TranslateM TypedExpr
simpleExpression ma = runExprCmd ma >>= extract
  where
    extract e@(ExprCmd (Just _) _) = M.unsupported "expression requires commands to be run" e
    extract (ExprCmd _ (Just e)) = return e
    extract e = M.unsupported "ExprCmd" e


data TypedExpr = TypedExpr {
  goType    :: Either Types.UnTyped Types.Type,
  balsaExpr ::  PT.Expr
  } deriving (Show, Eq)

explicit :: Types.Type -> PT.Expr -> TypedExpr
explicit t = TypedExpr (Right t)

implicit :: Types.UnTyped -> PT.Expr -> TypedExpr
implicit u = TypedExpr (Left u)

explicitType = either Types.defaultType id

-- A data type representing the commands necessary to run before the
-- result of some expression is available. This is necessary because
-- Go's Statements can sometimes be either a Balsa expression or statement, and
-- Go's expressions can sometiems be either a Balsa expression or statement.
-- This is visible in function application, where a byte(..) is an expression (has a return type of byte),
data ExprCmd = ExprCmd
               (Maybe PT.Cmd) -- The commands necessary to run before the expression is available
               (Maybe TypedExpr) -- The expression
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
  root' program
  D.topLevelContext <$> M.popContext

root' :: Program -> TranslateM ()
root' program = do
  BT.declareBuiltins
  declare "print" $ D.Proc (Types.Func (Types.Signature U.empty (Just Types.Any) U.empty)) C.EmptyContext PT.NoCmd printBuiltin
  forM_ (declarations program) declareTopLevel


--balsaType :: Type -> TranslateM PT.Type
--balsaType (TypeName id') = return $ PT.NameType pos (unId id')
--balsaType (SliceType typ) = PT.ArrayType (PT.Interval (0,0) bool) <$> balsaType typ
--balsaType (FunctionType _) = return PT.NoType
--balsaType t = M.unsupported "type" t

canonicalBalsaType :: Types.Type -> TranslateM PT.Type
canonicalBalsaType (Types.Named (Name n) _) = return $ PT.NameType pos (unpack n)
canonicalBalsaType Types.Bool = return Builtins.bool
canonicalBalsaType t@(Types.Numeric n) = case n of
       Types.Int8 -> return Builtins.int8
       Types.Int16 -> return Builtins.int16
       Types.Int32 -> return Builtins.int32
       Types.Int64 -> return Builtins.int64
       Types.Uint8 -> return Builtins.uint8
       Types.Uint16 -> return Builtins.uint16
       Types.Uint32 -> return Builtins.uint32
       Types.Uint64 -> return Builtins.uint64
       Types.GoInt -> return Builtins.int
       Types.GoUint -> return Builtins.uint
       _ -> M.unsupported "type" t
canonicalBalsaType Types.String = return Builtins.string
canonicalBalsaType (Types.Array i t) = PT.ArrayType (PT.Interval (0, i) Builtins.int) <$> canonicalBalsaType t
canonicalBalsaType t@(Types.Chan _ _) = M.unsupported "type: chan types must be statically declared: " t
canonicalBalsaType t@(Types.Struct _) = M.unsupported "type: anonymous structs are not supported: " t
canonicalBalsaType (Types.Func _) = return PT.NoType
canonicalBalsaType t = M.unsupported "type" t


declareType :: Id -> Types.Type -> TranslateM ()
declareType id' t = do
  balsaType <- mkType t
  BT.declareType (name (unId id')) t balsaType
  where
   mkType (Types.Struct fields) = PT.RecordType D.pos <$> traverse fieldDecl fields <*> pure PT.NoType
     where
       fieldDecl (id', typ) = PT.RecordElem D.pos (unId id') <$> canonicalBalsaType typ
   mkType t = PT.AliasType pos <$> canonicalBalsaType t


declareTopLevel :: Declaration -> TranslateM ()
declareTopLevel (Const (Id id') typ e) = do
  declaredType <- traverse canonicalType typ
  (TypedExpr typ' e') <- simpleExpression $ exprExp e
  case declaredType of
    Just t -> do
      t =? typ'
      declare id' $ D.Const (Right t) e'
    Nothing -> declare id' $ D.Const typ' e'
-- err, this should be type checked, but need to refactor decls
declareTopLevel (Var i _ (Prim (LitFunc sig' block))) = declareTopLevel (Func i sig' block)
declareTopLevel (Var (Id id') t (Prim (Make (Channel Bidirectional typ') []))) = do
  declaredType <- canonicalType t
  goType <- canonicalType typ'
  declaredType =? Right (Types.Chan Bidirectional goType)
  t' <- canonicalBalsaType goType
  declare id' $ D.Chan goType t'
declareTopLevel (Var (Id id') typ e) = do
  declaredType <- canonicalType typ
  t <- canonicalBalsaType declaredType
  case e of
    Zero -> declare id' $ D.Var declaredType t Nothing
    _  -> do
      (TypedExpr t' e') <- simpleExpression $ exprExp e
      declaredType =? t'
      declare id' $ D.Var declaredType t (Just e')
declareTopLevel (Type id' typ) = do
  t' <- canonicalType typ
  declareType id' t'
declareTopLevel (Func (Id id') sig block) = declare id' =<< mkProcedure (unpack id') sig block

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

blockCmd :: Block -> TranslateM PT.Cmd
blockCmd (Block statements) = do
  M.newContext
  cmd' <- seqCmd $ U.toList statements
  c <- D.subContext <$> M.popContext
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
    match e = PT.ExprCaseMatch pos . balsaExpr <$> forceExp e


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
cmd (Go p) = getCmd <$> runExprCmd (exprExp p)
cmd (Simple s) = getCmd <$> simpleExp s
cmd (StmtBlock block) = blockCmd block
-- for { } construct is identical to loop ... end
cmd (ForWhile Nothing block) = PT.LoopCmd pos <$> blockCmd block
cmd (ForWhile (Just e) block) = runSideEffects $
  PT.WhileCmd pos PT.NoCmd <$> (balsaExpr <$> forceExp e) <*> lift (blockCmd block)
-- for i := <start>; i < <end>; i++ { } is equivalent to a range
cmd f@(ForThree
       (SimpVar id' start)
       (Just (BinOp LessThan (Prim (Qual Nothing id'')) end))
       (Inc (Prim (Qual Nothing id''')))
       block)
  | id' == id'' && id' == id''' = b interval
  where
    b (Just (goType, i@(PT.Interval _ typ'))) = do
       M.newContext
       declare (idText id') (D.Param goType typ')
       cmd' <- blockCmd block
       c <- D.subContext <$> M.popContext
       return $ PT.ForCmd pos PT.Seq i c cmd'
    b _ = unsupported "ForLoop" f

    interval :: Maybe (Types.Type, PT.Interval)
    interval = do
      (start', end') <- (,) <$> Eval.eval start <*> Eval.eval end
      case (start', end') of
        (Eval.IntegralR t s, Eval.IntegralR _ e) ->
          return $ (Types.Numeric t, PT.Interval (s, pred e) $ lookupType t)
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
  PT.CaseCmdE pos <$> (balsaExpr <$> forceExp expr) <*> lift (fmap return trueBlock) <*> lift s'
  where
    s' = maybe (return PT.NoCmd) cmd s
    trueBlock = PT.CaseCmdGuard pos [PT.ExprCaseMatch pos (PT.ValueExpr pos bool true)] <$> blockCmd block
cmd (Switch (Cond Nothing (Just expr)) cases) = runSideEffects $ do
  (cs, def) <- caseCmds cases
  PT.CaseCmdE pos <$> (balsaExpr <$> forceExp expr) <*> pure cs <*> pure def
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

type ExprT r = Cont (Maybe TypedExpr) r
type ExprFT r = Cont TypedExpr r

andThen :: PT.Cmd -> PT.Cmd -> PT.Cmd
a `andThen` (PT.SeqCmd p cmds) = PT.SeqCmd p (a : cmds)
a `andThen` b = PT.SeqCmd pos [a,b]

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

(<**>) :: (Applicative f, Applicative g) =>
         f (g (a -> b)) -> f (g a) -> f (g b)
a <**> b = getCompose $ Compose a <*> Compose b

mkExpr :: TypedExpr -> ExprT ExprCmd
mkExpr = return . Just

unOpExpr :: Expr -> (PT.Type -> PT.Expr -> PT.Expr) -> ContT ExprCmd TranslateM (Maybe TypedExpr)
unOpExpr e f = do
  mayExpr <- exprExp e
  case mayExpr of
    Just (TypedExpr t e') -> do
      bType <- lift $ canonicalBalsaType $ explicitType t
      return $ Just $ TypedExpr t $ f bType e'
    Nothing -> return Nothing


exprExp :: Expr -> ExprT ExprCmd
exprExp (Prim prim) = primExp prim
exprExp (UnOp Plus e) = unOpExpr e $ \bType -> PT.BinExpr pos bType PT.BinAdd (PT.ValueExpr pos bType (PT.IntValue 0))
exprExp (UnOp Minus e) = unOpExpr e $ \bType -> PT.BinExpr pos bType PT.BinSub (PT.ValueExpr pos bType (PT.IntValue 0))
exprExp (UnOp Not e) = unOpExpr e $ \bType -> PT.UnExpr pos bType PT.UnNot
-- We only allow channel operations on names
exprExp (UnOp Receive (Prim (Qual Nothing (Id chan)))) = do
  (input, expr) <- lift f
  alterCmd (input `andThen`) $ return expr
  where
    f = do
     chan' <- M.lookup chan
     case chan' of
       Nothing -> M.notDefined (unpack chan)
       Just t -> do
         (goType, balsaType) <- receiveType t
         id' <- M.fresh
         declare id' $ D.Var goType balsaType Nothing
         let
           name = (unpack id')
           input = PT.InputCmd pos (PT.NameChan pos (unpack chan)) (PT.NameLvalue pos name)
           expr = PT.NameExpr pos name
         return (input, Just (explicit goType expr))
       where
         receiveType (D.In goType balsaType) = return (goType, balsaType)
         receiveType (D.Chan goType balsaType) = return (goType, balsaType)
         receiveType _ = M.unsupported "chan or <-chan" ""
exprExp b@(BinOp op e e') = do
  op' <- lift (binOp op)
  (a, b) <- (,) <$> forceExp e <*> forceExp e'
  goType' <- lift $ binType op (goType a) (goType b)
  bType <- lift $ canonicalBalsaType $ explicitType goType'
  return $ Just $ TypedExpr goType' $ PT.BinExpr pos bType op' (balsaExpr a) (balsaExpr b)
  where
    binType _ a b = do
      let
        a' :: Types.Type
        a' = either Types.defaultType id a
      a' =? b
      return a
exprExp e = lift $ M.unsupported "expr" e

forceExp :: Expr -> ExprFT ExprCmd
forceExp e = do
  a <- exprExp e
  case a of
    Nothing -> lift $ M.unsupported "expr" e
    Just a -> return a

mapCmd :: (PT.Cmd -> PT.Cmd) -> ExprCmd -> ExprCmd
mapCmd f (ExprCmd mc e) = ExprCmd (Just (f (fromMaybe PT.NoCmd mc))) e

alterCmd :: (PT.Cmd -> PT.Cmd)
           -> Cont a ExprCmd -> Cont a ExprCmd
alterCmd f = mapContT (fmap (mapCmd f))

primExp :: Prim -> ExprT ExprCmd
primExp (LitInt i) = mkExpr $ implicit Types.DefaultInt $ PT.ValueExpr pos byte (PT.IntValue i)
primExp (LitStr s) = mkExpr $ implicit Types.DefaultString $ PT.ValueExpr pos string (PT.StringValue (unpack s))
-- todo add type checking here
primExp p@(Qual (Just (Id struct)) id') = do
  s <- lift $ M.lookup' struct
  case s of
    (D.Var t _ _) -> go t
    (D.Const (Right t) _) -> go t
    _ -> lift $ M.unsupported "accessor" p
  where
    go (Types.Struct fields) = do
      let field = lookup id' fields
      case field of
        Nothing -> lift $ M.notDefined (unId id')
        (Just t) -> mkExpr $ explicit t $ PT.RecElemExpr pos (PT.NameExpr pos (unpack struct)) (unId id')
primExp (Qual Nothing (Id id')) = do
  typ' <- lift $ do
    var <- M.lookup' id'
    case var of
      (D.Var t _ _) -> return $ Right t
      (D.Chan t _) -> return $ Right $ Types.Chan Bidirectional t
      (D.In t _) -> return $ Right $ Types.Chan Input t
      (D.Out t _) -> return $ Right $ Types.Chan Output t
      (D.Const t _) -> return t
      (D.Param t _) -> return $ Right t
      (D.Proc t _ _ _) -> return $ Right t
  mkExpr $ TypedExpr typ' $ PT.NameExpr pos (unpack id')
-- We emulate function literals by defining a named function in the
-- current scope. This is not a valid Go identifier so is guaranteed
-- to not be allowed by the user.
primExp (LitFunc sig block) = do
  i <- lift $ do
    id' <- M.fresh
    declareTopLevel $ Func (Id id') sig block
    return id'
  t' <- lift $ canonicalType (FunctionType sig)
  mkExpr $ explicit t' $ PT.NameExpr pos (unpack i)
primExp (Call p es me) = do
  callable <- primExp p
  case callable of
    (Just (TypedExpr _ (PT.NameExpr _ id'))) -> do
      callable <- lift $ M.lookup' (pack id')
      case callable of
         (D.Proc _ _ _ funcCall) -> do
           exprs <- mapM forceExp es
           mayExpr <- traverse forceExp me
           let
             (cmd', expr) = call funcCall exprs mayExpr
           case cmd' of
             Just a -> alterCmd (a `andThen`) (return expr)
             Nothing -> return expr
    _ -> lift $ M.unsupported "callable" callable
primExp s@(Slice p me (Just end)) = do
  args <- (,,) <$$> primExp p <**> exprExp start <**> exprExp end
  forM args slice
  where
    slice ((TypedExpr (Right (Types.Array _ itemType)) p), (TypedExpr _ se@(PT.ValueExpr _ _ (PT.IntValue s))), (TypedExpr _ ee@(PT.ValueExpr _ _ (PT.IntValue e)))) = return $ explicit (Types.Array (e - s) itemType) $ PT.SliceExpr pos p se ee
    slice _ = lift $ M.unsupported "slice statement" s
    start = fromMaybe (Prim (LitInt 0)) me
primExp s@(Slice _ _ Nothing) = lift $ M.unsupported "slice expression with no end" s
primExp (Index p e) = do
  m <- (,) <$$> primExp p <**> exprExp e
  forM m $ uncurry index
  where
    index (TypedExpr (Right (Types.Array _ t)) arr) (TypedExpr _ i) = return $ explicit t $ PT.IndexExpr pos arr i
primExp (Paren e) = exprExp e
primExp s = lift $ M.unsupported  "primitive" s

simpleExp :: Simp -> TranslateM ExprCmd
simpleExp Empty = return $ ExprCmd Nothing Nothing
simpleExp (Inc e) = simpleExp $ Assign e $ BinOp Operators.Add e (Prim (LitInt 1))
simpleExp (Dec e) = simpleExp $ Assign e $ BinOp Operators.Subtract e (Prim (LitInt 1))
simpleExp s@(Send chan e) = runContT ma return
  where
    checkSend (Types.Chan Input _) _ = M.unsupported "sending on input channel" e
    checkSend (Types.Chan _ t) t' = t =? t'
    checkSend t _ = M.unsupported "sending on non channel" t
    ma = do
      chan' <- exprExp chan
      case chan' of
        (Just (TypedExpr (Right chanType) (PT.NameExpr _ id'))) -> do
          (TypedExpr t e') <- forceExp e
          lift $ checkSend chanType t
          let
            c = PT.OutputCmd pos (PT.NameChan pos id') e'
          return $ ExprCmd (Just c) Nothing
        _ -> lift $ unsupported "chan expression" chan
simpleExp s@(SimpVar (Id id') e) = runContT ma return
  where
    ma = do
      (TypedExpr t ex) <- forceExp e
      let
        goType = explicitType t
      lift $ do
        bType <- canonicalBalsaType goType
        declare id' $ D.Var goType bType Nothing
      return $ ExprCmd (Just $ PT.AssignCmd pos (PT.NameLvalue pos (unpack id')) ex) Nothing
simpleExp (SimpleExpr e) = runExprCmd (exprExp e)
simpleExp s = unsupported "simple expression " s


-- Functions for declaring and calling go functions & balsa procedures

printBuiltin :: Call
printBuiltin = FuncCall f
  where
    f es (Just e) = (Just $ PT.PrintCmd pos (map balsaExpr (es ++ [e])), Nothing)
    f es Nothing = (Just $ PT.PrintCmd pos (map balsaExpr es), Nothing)


mkProcedure :: String -> Signature Type -> Block -> TranslateM Decl
mkProcedure id' sig block = do
  canonicalSig <- traverse canonicalType sig
  let
    args = Func.signatureArgs canonicalSig

  (inConvention, outConvention) <- Func.conventions (fmap Id M.fresh) args
  declareNewType inConvention
  declareNewType outConvention

  M.newContext
  newBlock <- declareSig canonicalSig inConvention outConvention block
  b <- blockCmd newBlock
  sigDecl' <- M.popContext
  funcType <- canonicalType (FunctionType sig)
  return $ D.Proc funcType (D.subContext sigDecl') b (defaultCall id')

  where
    declareNewType conv = case Func.newType conv of
      (Just (i, t)) -> declareType i t
      _ -> return ()

    declareSig :: Signature Types.Type -> Func.ArgConvention -> Func.ArgConvention -> Block -> TranslateM Block
    declareSig s@(Signature _ (Just _)  _) _ _ _ = M.unsupported "function declaration with variadic" s
    declareSig s@(Signature inputs _ _) inConv outConv b = do
      forM_ kept decl
      let
        inType = Types.Chan Input <$> Func.argType inConv
        outType = Types.Chan Output <$> Func.argType outConv
      inParam <- traverse newParam inType
      outParam <- traverse newParam outType
      (maybe return addBlockPrelude inParam) b
      where
        decl = traverse sigDecl >=> declareParam

        recStatement (Just id') e = Simple $ SimpVar id' (UnOp Receive e)
        recStatement Nothing e = Simple $ SimpleExpr (UnOp Receive e)

        prependBlock s (Block stmts) = Block (U.cons s stmts)
        concatBlock s' (Block stmts) = Block ((U.++) (U.fromList s') stmts)


        addBlockPrelude (Param Nothing _) block = M.internalError "need a named parameter in generated parameter"
        addBlockPrelude (Param (Just inputName) _) block = case inConv of
          Func.Empty -> return block
          (Func.Single _ (Param varName _)) ->
              return $ prependBlock (recStatement varName (Prim (Qual Nothing inputName))) block
          (Func.Bundled _ fields) -> do
            tmp <- Id <$> M.fresh
            let
              tmp' = recStatement (Just tmp) (Prim (Qual Nothing inputName))
            stmts <- forM fields $ \(fieldName, _, (Param varName _)) -> do
              return $ recStatement varName (Prim (Qual (Just tmp) fieldName))
            return $ concatBlock (tmp':stmts) block


        newParam t = do
            name <- M.fresh
            let p = Param (Just (Id name)) t
            decl p
            return p

        declareParam (Param (Just (Id id')) d) = declare id' d
        declareParam (Param Nothing d) = declare "_" d

        sigDecl :: Types.Type -> TranslateM Decl
        sigDecl (Types.Chan Input typ) = D.In typ <$> canonicalBalsaType typ
        sigDecl (Types.Chan Output typ) = D.Out typ <$> canonicalBalsaType typ
        sigDecl t = M.unsupported "signature type" t

        kept = passthrough inputs

        passthrough = case inConv of
          Func.Empty -> id
          (Func.Single i _) -> f i
            where
              f i a = U.take (pred i) s U.++ e
                where
                  (s, e) = U.splitAt i a
          (Func.Bundled _ ps) -> U.ifilter (\i _ -> elem i ixs)
            where
              ixs = map ix ps
              ix (_, i, _) = i

    defaultCall :: String -> Call
    defaultCall id' = FuncCall f where
      f es _ = (Just $ PT.CallCmd pos (PT.NameCallable pos id') C.EmptyContext $ map (PT.ExprProcActual . balsaExpr) es, Nothing)
