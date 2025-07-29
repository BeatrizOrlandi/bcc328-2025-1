{
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module L.L2.Frontend.Lexer
  ( Token(..)
  , Lexeme(..)
  , lexer
  ) where
}

%wrapper "posn"

$digit     = 0-9
$alpha     = [A-Za-z]
$alphanum  = [A-Za-z0-9]
@number    = $digit+
@ident     = $alpha $alphanum*
@string     = \"[^\"]*\"

tokens :-

    $white+      ;
    "//".*       ;

    "def"        { simple TDef   }
    "in"         { simple TIn    }
    "end"        { simple TEnd   }
    "read"       { simple TRead  }
    "print"      { simple TPrint }

    @number      { mkNum }
    @ident       { mkId  }
    @string      { mkStr }

    ":="         { simple TAssign }
    ";"          { simple TSemi   }
    ","          { simple TComma  }
    "("          { simple TLParen }
    ")"          { simple TRParen }
    "+"          { simple TPlus   }
    "-"          { simple TMinus  }
    "*"          { simple TTimes  }
    "/"          { simple TDiv    }

{
data Token = Token { pos :: (Int,Int), lexeme :: Lexeme } deriving (Eq,Ord,Show)

data Lexeme
  = TNumber Int | TString String | TIdent String
  | TAssign | TSemi | TComma
  | TLParen | TRParen
  | TPlus | TMinus | TTimes | TDiv
  | TRead | TPrint
  | TDef | TIn | TEnd
  | TEOF
  deriving (Eq,Ord,Show)

position :: AlexPosn -> (Int,Int)
position (AlexPn _ l c) = (l,c)

mkNum :: AlexPosn -> String -> Token
mkNum p s = Token (position p) (TNumber (read s))

mkId :: AlexPosn -> String -> Token
mkId p s = Token (position p) (TIdent s)

mkStr :: AlexPosn -> String -> Token
mkStr p s = Token (position p) (TString (init (tail s)))

simple :: Lexeme -> AlexPosn -> String -> Token
simple lx p _ = Token (position p) lx

lexer :: String -> [Token]
lexer src = alexScanTokens src ++ [Token (0,0) TEOF]
}
