{
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Happy.L1Parser (parseL1) where

import L.L1.Frontend.Syntax
import L.L1.Frontend.Lexer (Token(..), Lexeme(..), lexer)
import Utils.Var   (Var(..))
import Utils.Value (Value(..))
}

%name   parseTokens
%tokentype { Token }
%error  { parseError }

%left  PLUS MINUS
%left  TIMES DIVIDE

%token
  IDENT   { Token _ (TIdent  $$) }
  NUM     { Token _ (TNumber $$) }
  STR     { Token _ (TString $$) }

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
%%

Program  : StmtList                     { L1 $1 }

StmtList :                               { [] }
         | StmtList Stmt                 { $1 ++ [$2] }

Stmt     : IDENT ASSIGN Expr SEMI        { LAssign (Var $1) $3 }
         | READ  LPAREN STR COMMA IDENT RPAREN SEMI
                                           { LRead  $3 (Var $5) }
         | PRINT LPAREN Expr RPAREN SEMI  { LPrint $3 }

Expr     : Expr PLUS  Term               { LAdd   $1 $3 }
         | Expr MINUS Term               { LMinus $1 $3 }
         | Term                          { $1 }

Term     : Term TIMES Factor             { LMul $1 $3 }
         | Term DIVIDE Factor            { LDiv $1 $3 }
         | Factor                        { $1 }

Factor   : NUM                           { LVal (VInt $1) }
         | STR                           { LVal (VStr $1) }
         | IDENT                         { LVar (Var $1) }
         | LPAREN Expr RPAREN            { $2 }


{
-- executa lexer + parser
parseL1 :: String -> Either String L1
parseL1 src = Right (parseTokens (lexer src))

-- mensagem de erro
parseError :: [Token] -> a
parseError [] = error "Erro de sintaxe: fim inesperado do arquivo."
parseError (Token (l,c) lx : _) =
  error $ "Erro de sintaxe na linha " ++ show l
       ++ ", coluna " ++ show c
       ++ " perto de " ++ show lx
}