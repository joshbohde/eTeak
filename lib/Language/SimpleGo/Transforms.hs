{-# LANGUAGE OverloadedStrings #-}
-- |

module Language.SimpleGo.Transforms (
  exprM, expr, replaceIota
  ) where

import           Control.Monad.Identity (runIdentity)
import           Language.SimpleGo.AST


-- Change expressions
exprM :: Monad m => (Expr -> m Expr)
        -> Expr -> m Expr
exprM ma e = f e >>= ma
  where
    decl (Const i t expr') = Const i t <$> go expr'
    decl (Var i t expr') = Var i t <$> go expr'
    decl (Func i sig block) = Func i sig <$> blk block
    decl t = pure t

    blk (Block stmnts) = Block <$> traverse stmnt stmnts

    simp (Send expr' expr'') = Send <$> go expr' <*> go expr''
    simp (SimpleExpr expr') = SimpleExpr <$> go expr'
    simp (Inc expr') = Inc <$> go expr'
    simp (Dec expr') = Dec <$> go expr'
    simp (SimpVar i expr') = SimpVar i <$> go expr'
    simp (Assign expr' expr'') = Assign <$> go expr' <*> go expr''
    simp s = pure s

    cond (Cond ms me) = Cond <$> traverse simp ms <*> traverse go me

    stmnt (Return es) = Return <$> traverse go es
    stmnt (StmtDecl d) = StmtDecl <$> decl d
    stmnt (Simple s) = Simple <$> simp s
    stmnt (Go expr') = Go <$> go expr'
    stmnt (StmtBlock block) = StmtBlock <$> blk block
    stmnt (If cnd block mayStmnt) = If <$> cond cnd <*> blk block <*> traverse stmnt mayStmnt
    stmnt (Switch cnd cases) = Switch <$> cond cnd <*> traverse (traverse go) cases
    stmnt (ForWhile expr' block) = ForWhile <$> traverse go expr' <*> blk block
    stmnt (ForThree simp' mayExpr simp'' block) = ForThree <$> simp simp' <*> traverse go mayExpr <*> simp simp'' <*> blk block
    stmnt (ForRange es expr' assign block) = ForRange <$> traverse go es <*> go expr' <*> pure assign <*> blk block
    stmnt s = pure s


    go = exprM ma
    f Zero = ma Zero
    f (UnOp o ex) = UnOp o <$> go ex
    f (BinOp o ex ex') = BinOp o <$> go ex <*> go ex'
    f (Prim prim) = Prim <$> prim' prim
      where
        prim' (LitComp t es) = LitComp t <$> traverse go es
        prim' (LitFunc sig block) = LitFunc sig <$> blk block
        prim' (Paren ex) = Paren <$> go ex
        prim' (Cast t ex) = Cast t <$> go ex
        prim' (Make t exs) = Make t <$> traverse go exs
        prim' (Index p ex) = Index <$> prim' p <*> go ex
        prim' (Slice p me me') = Slice <$> prim' p <*> traverse go me <*> traverse go me'
        prim' (TA p t) = TA <$> prim' p <*> pure t
        prim' (Call p exs final) = Call <$> prim' p <*> traverse go exs <*> traverse go final
        prim' p = pure p

expr :: (Expr -> Expr) -> Expr -> Expr
expr f ex = runIdentity (exprM (return . f) ex)

replaceIota :: Integer -> Expr -> Expr
replaceIota i = expr f
  where
    f (Prim (Qual Nothing (Id "iota"))) = Prim (LitInt i)
    f x = x
