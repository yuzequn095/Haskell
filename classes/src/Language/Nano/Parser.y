{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Language.Nano.Parser (
    parseExpr
  , parseDefs
  , parseTokens
  ) where

import Language.Nano.Lexer
import Language.Nano.Types hiding (Nano (..))
import Control.Monad.Except
import Control.Exception

}

-- Entry point
%name top

-- Lexer structure
%tokentype { Token }

-- Parser monad
%monad { Except String } { (>>=) } { return }
%error { parseError }

-- Token Names
%token
    let   { LET _    }
    true  { TRUE _   }
    false { FALSE _  }
    in    { IN _     }
    if    { IF _     }
    then  { THEN _   }
    else  { ELSE _   }
    TNUM  { NUM _ $$ }
    ID    { ID _ $$  }
    '\\'  { LAM _    }
    '->'  { ARROW _  }
    '='   { EQB _    }
    '+'   { PLUS _   }
    '-'   { MINUS _  }
    '*'   { MUL _    }
    '&&'  { AND _    }
    '||'  { OR  _    }
    '=='  { EQL _    }
    '/='  { NEQ _    }
    '<'   { LESS _   }
    '<='  { LEQ _    }
    ':'   { COLON _  }
    '('   { LPAREN _ }
    ')'   { RPAREN _ }
    '['   { LBRAC _  }
    ']'   { RBRAC _  }
    ','   { COMMA _  }


-- Operators
%right in
%nonassoc '=' '==' '/=' '<' '<=' if then else
%right ':' '->'
%left '||' '&&'
%left '+' '-'
%left '*'
%%

Top  : Def			   { [$1] }
     | Def ',' Top		   { $1 : $3 }
     | Expr			   { [("", $1) ]}

Def  : ID '=' Expr		   { ($1, $3) }

Expr : let ID '=' Expr in Expr     { ELet $2 $4 $6 }
     | let ID IDS '=' Expr in Expr { ELet $2 (mkLam $3 $5) $7 }
     | '\\' ID '->' Expr           { ELam $2 $4 }
     | if Expr then Expr else Expr { EIf $2 $4 $6 }
     | Expr ':' Expr               { EBin Cons $1 $3 }
     | Expr ',' Expr               { EBin Cons $1 $3 }
     | '[' Expr                    { $2 }
     | Expr ']'                    { EBin Cons $1 ENil }
     | Cm                          { $1 }

Cm   : Cm '<' Cm		   { EBin Lt $1 $3 }
     | Cm '<=' Cm		   { EBin Le $1 $3 }
     | Cm '==' Cm		   { EBin Eq $1 $3 }
     | Cm '/=' Cm		   { EBin Ne $1 $3 }
     | Cm '||' Cm 		   { EBin Or $1 $3 }
     | Cm '&&' Cm		   { EBin And $1 $3 }
     | Op			   { $1 }

Op   : Op '+' Op		   { EBin Plus $1 $3 }
     | Op '-' Op		   { EBin Minus $1 $3 }
     | Op '*' Op		   { EBin Mul $1 $3 }
     | Func 			   { $1 }

Func : Func Para		   { EApp $1 $2 }
     | Para			   { $1 }

Para : '(' Expr ')' 		   { $2 }
     | '[' ']'			   { ENil }
     | TNUM			   { EInt $1 }
     | true			   { EBool True }
     | false			   { EBool False }
     | ID		           { EVar $1 }

IDS  : ID		           { [$1] }
     | ID IDS			   { $1:$2 }

{
mkLam :: [Id] -> Expr -> Expr
mkLam []     e = e
mkLam (x:xs) e = ELam x (mkLam xs e)

parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError []     = throwError "Unexpected end of Input"

parseExpr :: String -> Expr
parseExpr s = case parseDefs' s of
                Left msg         -> throw (Error ("parse error:" ++ msg))
                Right ((_,e):_)  -> e

parseDefs :: String -> [(Id, Expr)]
parseDefs s = case parseDefs' s of 
                Left msg -> throw (Error ("parse error:" ++ msg))
                Right e  -> e
                
parseDefs' input = runExcept $ do
   tokenStream <- scanTokens input
   top tokenStream

parseTokens :: String -> Either String [Token]
parseTokens = runExcept . scanTokens


}
