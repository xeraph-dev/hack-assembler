module HackAssembler.Eval (eval) where

import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.Map                   as M
import           HackAssembler.AST          (AInstr (..), Addresses,
                                             EnvCtx (..), Exp (..))
import           HackAssembler.Lexer        (Range)
import           HackAssembler.Parser       (parse)

addresses :: Addresses
addresses
  = M.fromList
    [("SP",         0)
    ,("LCL",        1)
    ,("ARG",        2)
    ,("THIS",       3)
    ,("THAT",       4)
    ,("R0",         0)
    ,("R1",         1)
    ,("R2",         2)
    ,("R3",         3)
    ,("R4",         4)
    ,("R5",         5)
    ,("R6",         6)
    ,("R7",         7)
    ,("R8",         8)
    ,("R9",         9)
    ,("R10",       10)
    ,("R11",       11)
    ,("R12",       12)
    ,("R13",       13)
    ,("R14",       14)
    ,("R15",       15)
    ,("SCREEN", 16384)
    ,("KBD",    24576)]

data EvalEnv a
  = EvalEnv EnvCtx [Exp a]
  deriving (Eq, Show)


evalLabel :: Exp a -> (EnvCtx, [Exp a]) -> (EnvCtx, [Exp a])
evalLabel (ELabel _ label) (ctx@EnvCtx {line=line', env=env'}, exps) = (ctx {env=M.insert label line' env'}, exps)
evalLabel exp' (ctx@EnvCtx {line=line'}, exps) = (ctx {line=line'+1}, exp':exps)

evalSymbol :: Exp a -> (EnvCtx, [Exp a]) -> (EnvCtx, [Exp a])
evalSymbol (EAInstr r (Symbol sym)) (ctx@EnvCtx {env=env', mem=mem'}, exps)
  = case M.lookup sym env' of
      Just sym' -> (ctx, EAInstr r (Const sym'):exps)
      Nothing   -> (ctx {mem=mem'+1, env=M.insert sym mem' env'}, EAInstr r (Const mem'):exps)
evalSymbol exp' (ctx, exps) = (ctx, exp':exps)

eval :: ByteString -> Either String [Exp Range]
eval code = case parse code of
              Left e -> Left e
              Right p ->
                let (ctx, exps) = foldl (flip evalLabel) (EnvCtx {line=0, env=addresses, mem=16}, []) p
                    (_, exps')  = foldl (flip evalSymbol) (ctx, []) $ reverse exps
                in Right $ reverse exps'



