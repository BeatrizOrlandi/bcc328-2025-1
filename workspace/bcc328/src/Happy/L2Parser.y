{
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Happy.L2Parser (parseL2) where

import L.L2.Frontend.Lexer   (Token(..), Lexeme(..), lexer)
import L.L2.Frontend.Syntax
import Utils.Var   (Var(..))
import Utils.Value (Value(..))
}

%name       parseTokens
%tokentype  { Token }
%error      { parseErr }

%token
  NUM     { Token _ (TNumber $$) }
  STR     { Token _ (TString $$) }
  ID      { Token _ (TIdent  $$) }

  ASSIGN  { Token _ TAssign }
  READ    { Token _ TRead   }
  PRINT   { Token _ TPrint  }

  LPAREN  { Token _ TLParen }
  RPAREN  { Token _ TRParen }
  COMMA   { Token _ TComma  }
  SEMI    { Token _ TSemi   }

  PLUS    { Token _ TPlus   }
  MINUS   { Token _ TMinus  }
  TIMES   { Token _ TTimes  }
  DIVIDE  { Token _ TDiv    }

  DEF     { Token _ TDef    }
  INKW    { Token _ TIn     }
  ENDKW   { Token _ TEnd    }

%left  PLUS MINUS
%left  TIMES DIVIDE

%%          

Prog   :                         { L2 [] }
       | Stmt Prog               { let (L2 xs) = $2 in L2 ($1 : xs) }

Stmt   : ID ASSIGN Expr SEMI                    { LAssign (Var $1) $3 }
       | READ  LPAREN STR COMMA ID RPAREN SEMI  { LRead  $3 (Var $5) }
       | PRINT LPAREN Expr RPAREN SEMI          { LPrint $3 }
       | DEF ID ASSIGN Expr INKW Prog ENDKW     { let (L2 b) = $6 in Def (Var $2) $4 b }

Expr   : Expr PLUS  Term   { LAdd   $1 $3 }
       | Expr MINUS Term   { LMinus $1 $3 }
       | Term              { $1 }

Term   : Term TIMES Fact   { LMul $1 $3 }
       | Term DIVIDE Fact  { LDiv $1 $3 }
       | Fact              { $1 }

Fact   : NUM               { LVal (VInt $1) }
       | STR               { LVal (VStr $1) }
       | ID                { LVar (Var $1) }
       | LPAREN Expr RPAREN{ $2 }

{
parseL2 :: String -> Either String L2
parseL2 src = Right (parseTokens (lexer src))

parseErr :: [Token] -> a
parseErr []                    = error "erro de sintaxe (fim do arquivo)"
parseErr (Token (l,c) lx : _)  =
  error $ "erro de sintaxe na linha "
        ++ show l ++ ", coluna " ++ show c ++ " perto de " ++ show lx
}