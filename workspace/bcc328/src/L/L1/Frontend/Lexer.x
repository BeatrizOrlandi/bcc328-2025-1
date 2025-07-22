{
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module L.L1.Frontend.Lexer
  ( Token(..)
  , Lexeme(..)
  , lexer
  ) where
}

%wrapper "posn"

--------------------------------------------------------------------------
-- Macros (expressões regulares reutilizáveis)
--------------------------------------------------------------------------
$digit      = 0-9
$alpha      = [A-Za-z]
$alphanum   = [A-Za-z0-9]
@number     = $digit+
@ident      = $alpha$alphanum*
@string     = \"[^\"]*\"


--------------------------------------------------------------------------
-- Regras (a ordem importa). NÃO coloque comentários aqui!
--------------------------------------------------------------------------
tokens :-

  $white+            ;

  "//".*             ;

  "read"             { simpleToken TRead  }
  "print"            { simpleToken TPrint }

  @number            { mkNumber }
  @ident             { mkIdent  }
  @string            { mkString }

  ":="               { simpleToken TAssign }
  ";"                { simpleToken TSemi   }
  ","                { simpleToken TComma  }
  "("                { simpleToken TLParen }
  ")"                { simpleToken TRParen }
  "+"                { simpleToken TPlus   }
  "-"                { simpleToken TMinus  }
  "*"                { simpleToken TTimes  }
  "/"                { simpleToken TDiv    }

{
--------------------------------------------------------------------------
-- Tipos de token / lexema
--------------------------------------------------------------------------
data Token = Token
  { pos    :: (Int, Int)   -- (linha, coluna)
  , lexeme :: Lexeme
  } deriving (Eq, Ord, Show)

data Lexeme
  = TNumber Int
  | TIdent  String
  | TString String
  | TAssign | TSemi  | TComma
  | TLParen | TRParen
  | TPlus   | TMinus | TTimes | TDiv
  | TRead   | TPrint
  | TEOF
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------
-- Construtores auxiliares
--------------------------------------------------------------------------
position :: AlexPosn -> (Int, Int)
position (AlexPn _ l c) = (l, c)

mkNumber :: AlexPosn -> String -> Token
mkNumber p s = Token (position p) (TNumber (read s))

mkIdent  :: AlexPosn -> String -> Token
mkIdent  p s = Token (position p) (TIdent  s)

mkString :: AlexPosn -> String -> Token
mkString p s = Token (position p) (TString (init (tail s))) -- retira as aspas

simpleToken :: Lexeme -> AlexPosn -> String -> Token
simpleToken lx p _ = Token (position p) lx

--------------------------------------------------------------------------
-- Função principal exportada
--------------------------------------------------------------------------
lexer :: String -> [Token]
lexer = alexScanTokens
}
