{
module HackAssembler.Parser (parse) where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (fromJust)
import Data.Monoid (First (..))

import qualified HackAssembler.Lexer as L
import qualified HackAssembler.AST as A
import HackAssembler.Lexer (runAlex)
}

%name parseHasm exps
%tokentype { L.RangedToken }
%error { parseError }
%monad { L.Alex } { >>= } { pure }
%lexer { lexer } { L.RangedToken L.EOF _ }

%token
  M           { L.RangedToken (L.Identifier "M") _ }
  D           { L.RangedToken (L.Identifier "D") _ }
  DM          { L.RangedToken (L.Identifier "DM") _ }
  A           { L.RangedToken (L.Identifier "A") _ }
  AM          { L.RangedToken (L.Identifier "AM") _ }
  AD          { L.RangedToken (L.Identifier "AD") _ }
  ADM         { L.RangedToken (L.Identifier "ADM") _ }

  JGT         { L.RangedToken (L.Identifier "JGT") _ }
  JEQ         { L.RangedToken (L.Identifier "JEQ") _ }
  JGE         { L.RangedToken (L.Identifier "JGE") _ }
  JLT         { L.RangedToken (L.Identifier "JLT") _ }
  JNE         { L.RangedToken (L.Identifier "JNE") _ }
  JLE         { L.RangedToken (L.Identifier "JLE") _ }
  JMP         { L.RangedToken (L.Identifier "JMP") _ }

  '0'         { L.RangedToken (L.Integer 0) _ }
  '1'         { L.RangedToken (L.Integer 1) _ }

  identifier  { L.RangedToken (L.Identifier _) _ }
  integer     { L.RangedToken (L.Integer _) _ }

  '@'         { L.RangedToken (L.Symbol "@") _ }
  '('         { L.RangedToken (L.Symbol "(") _ }
  ')'         { L.RangedToken (L.Symbol ")") _ }
  '='         { L.RangedToken (L.Symbol "=") _ }
  ';'         { L.RangedToken (L.Symbol ";") _ }
  '-'         { L.RangedToken (L.Symbol "-") _ }
  '+'         { L.RangedToken (L.Symbol "+") _ }
  '!'         { L.RangedToken (L.Symbol "!") _ }
  '&'         { L.RangedToken (L.Symbol "&") _ }
  '|'         { L.RangedToken (L.Symbol "|") _ }

%%

aInstr :: { A.Exp L.Range }
  : '@' integer     { A.EAInstr (L.rtRange $1 <-> L.rtRange $2) (unTok $2 (\_ (L.Integer int)   -> A.Const int)) }
  | '@' '0'         { A.EAInstr (L.rtRange $1 <-> L.rtRange $2) (unTok $2 (\_ (L.Integer int)   -> A.Const int)) }
  | '@' '1'         { A.EAInstr (L.rtRange $1 <-> L.rtRange $2) (unTok $2 (\_ (L.Integer int)   -> A.Const int)) }
  | '@' identifier  { A.EAInstr (L.rtRange $1 <-> L.rtRange $2) (unTok $2 (\_ (L.Identifier id) -> A.Symbol id)) }

  | '@' M           { A.EAInstr (L.rtRange $1 <-> L.rtRange $2) (unTok $2 (\_ (L.Identifier id) -> A.Symbol id)) }
  | '@' D           { A.EAInstr (L.rtRange $1 <-> L.rtRange $2) (unTok $2 (\_ (L.Identifier id) -> A.Symbol id)) }
  | '@' DM          { A.EAInstr (L.rtRange $1 <-> L.rtRange $2) (unTok $2 (\_ (L.Identifier id) -> A.Symbol id)) }
  | '@' A           { A.EAInstr (L.rtRange $1 <-> L.rtRange $2) (unTok $2 (\_ (L.Identifier id) -> A.Symbol id)) }
  | '@' AM          { A.EAInstr (L.rtRange $1 <-> L.rtRange $2) (unTok $2 (\_ (L.Identifier id) -> A.Symbol id)) }
  | '@' AD          { A.EAInstr (L.rtRange $1 <-> L.rtRange $2) (unTok $2 (\_ (L.Identifier id) -> A.Symbol id)) }
  | '@' ADM         { A.EAInstr (L.rtRange $1 <-> L.rtRange $2) (unTok $2 (\_ (L.Identifier id) -> A.Symbol id)) }

  | '@' JGT         { A.EAInstr (L.rtRange $1 <-> L.rtRange $2) (unTok $2 (\_ (L.Identifier id) -> A.Symbol id)) }
  | '@' JEQ         { A.EAInstr (L.rtRange $1 <-> L.rtRange $2) (unTok $2 (\_ (L.Identifier id) -> A.Symbol id)) }
  | '@' JGE         { A.EAInstr (L.rtRange $1 <-> L.rtRange $2) (unTok $2 (\_ (L.Identifier id) -> A.Symbol id)) }
  | '@' JLT         { A.EAInstr (L.rtRange $1 <-> L.rtRange $2) (unTok $2 (\_ (L.Identifier id) -> A.Symbol id)) }
  | '@' JNE         { A.EAInstr (L.rtRange $1 <-> L.rtRange $2) (unTok $2 (\_ (L.Identifier id) -> A.Symbol id)) }
  | '@' JLE         { A.EAInstr (L.rtRange $1 <-> L.rtRange $2) (unTok $2 (\_ (L.Identifier id) -> A.Symbol id)) }
  | '@' JMP         { A.EAInstr (L.rtRange $1 <-> L.rtRange $2) (unTok $2 (\_ (L.Identifier id) -> A.Symbol id)) }

dest
  : M '='   { $1 }
  | D '='   { $1 }
  | DM '='  { $1 }
  | A '='   { $1 }
  | AM '='  { $1 }
  | AD '='  { $1 }
  | ADM '=' { $1 }

comp0
  : '0'       { $1 }
  | '1'       { $1 }
  | '-' '1'   { $1 <-<>->> [$2] }
  | D         { $1 }
  | A         { $1 }
  | '!' D     { $1 <-<>->> [$2] }
  | '!' A     { $1 <-<>->> [$2] }
  | '-' D     { $1 <-<>->> [$2] }
  | '-' A     { $1 <-<>->> [$2] }
  | D '+' '1' { $1 <-<>->> [$2,$3] }
  | A '+' '1' { $1 <-<>->> [$2,$3] }
  | D '-' '1' { $1 <-<>->> [$2,$3] }
  | A '-' '1' { $1 <-<>->> [$2,$3] }
  | D '+' A   { $1 <-<>->> [$2,$3] }
  | D '-' A   { $1 <-<>->> [$2,$3] }
  | A '-' D   { $1 <-<>->> [$2,$3] }
  | D '&' A   { $1 <-<>->> [$2,$3] }
  | D '|' A   { $1 <-<>->> [$2,$3] }

comp1
  : M         { $1 }
  | '!' M     { $1 <-<>->> [$2] }
  | '-' M     { $1 <-<>->> [$2] }
  | M '+' '1' { $1 <-<>->> [$2,$3] }
  | M '-' '1' { $1 <-<>->> [$2,$3] }
  | D '+' M   { $1 <-<>->> [$2,$3] }
  | D '-' M   { $1 <-<>->> [$2,$3] }
  | M '-' D   { $1 <-<>->> [$2,$3] }
  | D '&' M   { $1 <-<>->> [$2,$3] }
  | D '|' M   { $1 <-<>->> [$2,$3] }

comp :: { (Bool, L.RangedToken) }
  : comp0 { (False,$1) }
  | comp1 { (True ,$1) }

jump
  : ';' JGT { $2 }
  | ';' JEQ { $2 }
  | ';' JGE { $2 }
  | ';' JLT { $2 }
  | ';' JNE { $2 }
  | ';' JLE { $2 }
  | ';' JMP { $2 }

cInstr :: { A.Exp L.Range }
  : comp            { A.ECInstr (L.rtRange (snd $1)) (A.CInstr (fst $1) Nothing (unTok (snd $1) extractValue) Nothing) } 
  | dest comp       { A.ECInstr (L.rtRange $1 <-> L.rtRange (snd $2)) (A.CInstr (fst $2) (Just (unTok $1 extractValue)) (unTok (snd $2) extractValue) Nothing) } 
  | comp jump       { A.ECInstr (L.rtRange (snd $1) <-> L.rtRange $2) (A.CInstr (fst $1) Nothing (unTok (snd $1) extractValue) (Just (unTok $2 extractValue))) } 
  | dest comp jump  { A.ECInstr (L.rtRange $1 <-> L.rtRange $3) (A.CInstr (fst $2) (Just (unTok $1 extractValue)) (unTok (snd $2) extractValue) (Just (unTok $3 extractValue))) } 

label :: { A.Exp L.Range }
  : '(' identifier ')'  { A.ELabel (L.rtRange $1 <-> L.rtRange $3) (unTok $2 extractValue) }
  | '(' M ')'           { A.ELabel (L.rtRange $1 <-> L.rtRange $3) (unTok $2 extractValue) }
  | '(' D ')'           { A.ELabel (L.rtRange $1 <-> L.rtRange $3) (unTok $2 extractValue) }
  | '(' DM ')'          { A.ELabel (L.rtRange $1 <-> L.rtRange $3) (unTok $2 extractValue) }
  | '(' A ')'           { A.ELabel (L.rtRange $1 <-> L.rtRange $3) (unTok $2 extractValue) }
  | '(' AM ')'          { A.ELabel (L.rtRange $1 <-> L.rtRange $3) (unTok $2 extractValue) }
  | '(' AD ')'          { A.ELabel (L.rtRange $1 <-> L.rtRange $3) (unTok $2 extractValue) }
  | '(' ADM ')'         { A.ELabel (L.rtRange $1 <-> L.rtRange $3) (unTok $2 extractValue) }

exp :: { A.Exp L.Range }
  : aInstr  { $1 }
  | cInstr { $1 }
  | label   { $1 }

exps
  : many(exp) { $1 }

many(p)
  :           { [] }
  | p many(p) { $1 <-:-> $2 }

{
parseError :: L.RangedToken -> L.Alex a
parseError _ = do
  (L.AlexPn _ line column, _, _, _) <- L.alexGetInput
  L.alexError $ "Parse error at line " <> show line <> ", column " <> show column

lexer :: (L.RangedToken -> L.Alex a) -> L.Alex a
lexer = (=<< L.alexMonadScan)

unTok :: L.RangedToken -> (L.Range -> L.Token -> a) -> a
unTok (L.RangedToken tok range) ctor = ctor range tok


extractValue :: L.Range -> L.Token -> ByteString
extractValue _ (L.Symbol sym) = sym
extractValue _ (L.Identifier id) = id
extractValue _ (L.Integer int) = BS.pack $ show int

info :: Foldable f => f a -> a
info = fromJust . getFirst . foldMap pure

(<-<>->>) :: L.RangedToken -> [L.RangedToken] -> L.RangedToken
rt1 <-<>->> [] = rt1
rt1 <-<>->> rt2 
  = L.RangedToken 
    { L.rtRange = L.rtRange rt1 <-> (L.rtRange $ last rt2)
    , L.rtToken = L.Identifier $ unTok rt1 extractValue <> foldr (\rt acc -> unTok rt extractValue <> acc) "" rt2
    }

(<->) :: L.Range -> L.Range -> L.Range
L.Range a1 _ <-> L.Range _ b2 = L.Range a1 b2

(<>:<>) :: (A.Exp L.Range, L.Range) -> ([A.Exp L.Range], L.Range) -> [A.Exp L.Range]
(e1, r1) <>:<> (e2, r2) = do
  let (L.AlexPn _ line1 _) = L.stop r1
  let (L.AlexPn _ line2 column) = L.start r2
  if line2 > line1 
    then e1 : e2
    else error $ "Parser error: Missing line break at line " <> show line2 <> ", column " <> show column

(<-:->) :: A.Exp L.Range -> [A.Exp L.Range] -> [A.Exp L.Range]
e1 <-:-> [] = [e1]
e1@(A.EAInstr r1 _) <-:-> e2@((A.EAInstr r2 _):_) = (e1, r1) <>:<> (e2, r2)
e1@(A.ELabel r1 _) <-:-> e2@((A.ELabel r2 _):_) = (e1, r1) <>:<> (e2, r2)

parse :: ByteString -> Either String [A.Exp L.Range]
parse input = runAlex input parseHasm
}