{
module Lexical.Tokens (getTokens, Token(..)) where

import System.IO
import System.IO.Unsafe
}

%wrapper "basic"

$digit = [0-9]
$alpha = [a-zA-Z]   -- alphabetic characters

tokens :-

  -- THINGS THAT ARE IGNORED ------------------------------

  $white+                          ; -- white spaces
  "#".*                            ; -- single-line comment
  "#"!   [.\n]*  !"#"              ; -- multi-line comment

  -- BLOCKS -----------------------------------------------

  \{                               { \s -> LBrace }
  \}                               { \s -> RBrace }
  \(                               { \s -> LParen }
  \)                               { \s -> RParen }
  \[                               { \s -> LBracket }
  \]                               { \s -> RBracket }
 
  -- SECTIONS ---------------------------------------------

  \@imports                        { \s -> Imports } 
  \@globals                        { \s -> Globals }
  \@subprograms                    { \s -> Subprograms }
  \@main                           { \s -> Main }
  
  -- SEPARATORS -------------------------------------------

  -- statement separator
  ";"                              { \s -> SemiColon }

  -- LITERALS  ---------------------------------------------

  $digit+                          { \s -> Int (read s) }
 
  -- OPERATORS  --------------------------------------------

  =                                { \s -> Assign }
  \+                               { \s -> Plus }
  \-                               { \s -> Minus }
  \*                               { \s -> Asterisk }
  \/                               { \s -> Bar}
  "+="                             { \s -> PlusEquals }
  "-="                             { \s -> MinusEqual }
  "*="                             { \s -> AsteriskEquals }
  "/="                             { \s -> BarEquals }
  "++"                             { \s -> PlusPlus }
  "--"                             { \s -> MinusMinus }
  "<"                              { \s -> LessThan }
  ">"                              { \s -> GreaterThan }
  "<="                             { \s -> LessEquals }
  ">="                             { \s -> GreaterEquals }
  "!"                              { \s -> Negation }
  "&&"                             { \s -> AndShortCircuit }
  "and"                            { \s -> And }
  "||"                             { \s -> OrShortCircuit }
  "or"                             { \s -> Or }

  -- CONDITIONALS  -----------------------------------------

  if                               { \s -> If }
  else                             { \s -> Else }
  "else if"                        { \s -> ElseIf } 

  -- TYPES  ------------------------------------------------

  int                              { \s -> TypeInt }
  -- float                            { \s -> Type s }
  -- set                              { \s -> Type s }
  -- double                           { \s -> Type s } 
  -- bool                             { \s -> Type s }
  -- tuple                            { \s -> Type s }
  -- byte                             { \s -> Type s }
  string                           { \s -> TypeString } 

  -- LOOPS  ------------------------------------------------

  while                            { \s -> Loop s }
  for                              { \s -> Loop s }

  -- NAMES ------------------------------------------------

  -- filename
  [$alpha\_$digit]+   \.  [a-z]+   { \s -> Filename s }

  -- identifier
  $alpha [$alpha $digit \_ \']*    { \s -> Id s }

{
-- The token type:
data Token =

  -- BLOCKS -----------------------------------------------

  LBrace           |
  RBrace           |
  LParen           |
  RParen           |
  LBracket         |
  RBracket         |

  -- SECTIONS ---------------------------------------------
  
  Imports          |
  Globals          |
  Subprograms      |
  Main             |

  -- SEPARATORS -------------------------------------------

  -- statement separator
  SemiColon        |
  
  -- OPERATORS  --------------------------------------------

  Assign           |
  Plus             |
  Minus            |
  Asterisk         |
  Bar              |
  PlusEquals       |
  MinusEqual       |
  AsteriskEquals   |
  BarEquals        |
  PlusPlus         |
  MinusMinus       |
  LessThan         |
  GreaterThan      |
  LessEquals       |
  GreaterEquals    |
  Negation         |
  AndShortCircuit  |
  And              |
  OrShortCircuit   |
  Or               |
  
  -- CONDITIONALS  -----------------------------------------

  If               |
  Else             |
  ElseIf           |

  -- TYPES  ------------------------------------------------

  TypeString      |
  TypeInt         |
  
  -- LOOPS  ------------------------------------------------

  Loop String      |

  -- NAMES ------------------------------------------------

  -- identifier
  Id String        |

  -- file name
  Filename String  |

  -- LITERALS  ---------------------------------------------

  Int Int          

  ----------------------------------------------------------

  deriving (Eq,Show)

getTokens fn = unsafePerformIO (getTokensAux fn)

getTokensAux fn = do {fh <- openFile fn ReadMode;
                      s <- hGetContents fh;
                      return (alexScanTokens s)}
}