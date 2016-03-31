 -- |

 module Language.SimpleGo.AST where

 import qualified Data.Text   as T
 import qualified Data.Vector as U


 newtype PackageName = PackageName { unPackage :: T.Text } deriving (Eq, Show)

 -- | Go Language source start
 data Program = Program {
       declarations :: U.Vector Declaration
       } deriving (Eq, Show, Read)

 newtype Id = Id T.Text deriving (Eq, Show, Read)

 data Declaration = Const Id Type Expr
                  | Var Id Type Expr
                  | Type Id Type
                  | Func Id Signature Block
                 deriving (Eq, Read, Show)

 data UnOp = Plus      -- '+' (0 + x)
           | Minus     -- '-' (0 - x)
           | Complement -- '^' (is m ^ x  with m = "all bits set to 1" for unsigned x and  m = -1 for signed x)
           | Not   -- '!'
           | Address  -- '*'
           | Reference       -- '&'
           | Receive     -- '<-'
           deriving (Eq, Read, Show)

 data BinOp = LogicalOr -- '||'
            | LogicalAnd -- '&&'
            | Equal -- '=='
            | NotEqual -- '!='
            | LessThan -- '<'
            | LessThanEqual  -- '<='
            | GreaterThan        -- '>'
            | GreaterThanEqual        -- '>='
            | BitwiseXor       -- '^'
            | BitwiseOr       -- '|'
            | Add      -- '+'
            | Subtract     -- '-'
            | Multiply  -- '*'
            | Quotient   -- '/'
            | Remainder   -- '%'
            | LeftShift       -- '<<'
            | RightShift       -- '>>'
            | BitwiseAnd       -- '&'
            | AndNot       -- '&^'
            deriving (Eq, Read, Show)

 -- Rec (= 'Receiver' = 'ReceiverType')
 data Rec = Rec Bool (Maybe Id) Type
                 deriving (Eq, Read, Show)

 data Signature = Signature {
   input  :: U.Vector Param,
   output :: U.Vector Param
   } deriving (Eq, Read, Show)

 data Param = Param Id Type
            deriving (Eq, Read, Show)

 -- Type (= 'Type' = 'TypeLit' = 'LiteralType')
 data Type = TypeName Id
           | ArrayType Expr Type
           | Channel ChanKind Type
           | FunctionType Signature
           | MapType Type Type
           | PointerType Type
           | SliceType Type
           | StructType [FieldType]
           | EllipsisType Type  -- only in Literals
           | VariadicType Type  -- only in Funcs
           deriving (Eq, Read, Show)


 data ChanKind = Input  -- <-chan
               | Output  -- chan<-
               | Bidirectional -- chan
               deriving (Eq, Read, Show)

 -- FieldType
 data FieldType = FieldType {
       getFieldTag  :: Maybe String,
       getFieldId   :: [Id],
       getFieldType :: Type }
                  | FieldAnon {
       getFieldTag  :: Maybe String,
       getFieldPtr  :: Bool,
       getFieldType :: Type } -- MUST be typename
                 deriving (Eq, Read, Show)

 data Expr = Zero
           | Prim Prim
           | UnOp UnOp Expr
           | BinOp BinOp Expr Expr
           deriving (Eq, Read, Show)

 -- PrimExpr (= 'PrimaryExpr')
 data Prim = LitInt  Integer
           | LitReal Float
           | LitImag Float
           | LitChar Char
           | LitStr  String
           | LitFunc Signature Block
           | Qual Id -- 'PrimaryExpr/Operand/QualifiedIdent'
           | Method Rec Id     -- 'PrimaryExpr/Operand/MethodExpr'
           | Paren Expr          -- 'PrimaryExpr/Operand/MethodExpr'
           | Cast Type Expr    -- 'PrimaryExpr/Conversion'
           | New  Type           -- 'PrimaryExpr/BuiltinCall/new'
           | Make Type [Expr]  -- 'PrimaryExpr/BuiltinCall/make'
 --          | BI Id Type [Expr]  -- 'PrimaryExpr/BuiltinCall'
           | Select Prim Id    -- 'PrimaryExpr/Selector'
           | Index Prim Expr   -- 'PrimaryExpr/Index'
           | Slice Prim (Maybe Expr) (Maybe Expr) -- 'PrimaryExpr/Slice'
           | TA    Prim Type   -- 'PrimaryExpr/TypeAssertion'
           | Call  Prim [Expr]
               deriving (Eq, Read, Show)

 -- For defining record literals, etc.
 data Comp = Comp [Element]
           deriving (Eq, Read, Show)

 data Element = Element Key Value
              deriving (Eq, Read, Show)

 data Key = KeyNone
          | KeyField Id
          | KeyIndex Expr
          deriving (Eq, Read, Show)

 data Value = ValueExpr Expr -- 'Value/Expression'
            | ValueComp Comp -- 'Value/LiteralValue'
            deriving (Eq, Read, Show)

 newtype Block = Block (U.Vector Statement)
               deriving (Eq, Read, Show)

 data ForClause = ForWhile (Maybe Expr)
                | ForThree Simp (Maybe Expr) Simp
                | ForRange [Expr] Expr Bool -- True if AssignDecl
                deriving (Eq, Read, Show)

 data Statement = StmtDecl Declaration -- 'Statement/Declaration'
                | Simple Simp
                | Go Expr
                | Return [Expr]
                | Break    (Maybe Id)
                | Continue (Maybe Id)
                | Fallthrough
                | StmtBlock Block
                | If Cond Block (Maybe Statement)
                  -- TODO rewrite this to a more static case structure, with default
                | StmtSelect  [Case Chan]
                | Switch Cond [Case Expr]
                | For ForClause Block
                deriving (Eq, Read, Show)

 data Simp = Empty
           | Send Expr Expr
           | SimpleExpr Expr
           | Inc  Expr
           | Dec  Expr
           | SimpVar Id Expr
           deriving (Eq, Read, Show)

 data Chan = ChanRecv (Maybe (Expr, Maybe Expr, UnOp)) Expr
           | ChanSend Expr Expr
           deriving (Eq, Read, Show)

 data Cond = Cond (Maybe Simp) (Maybe Expr)
           deriving (Eq, Read, Show)


 data Case a = Case [a] [Statement]
             | Default  [Statement]
             deriving (Eq, Read, Show)