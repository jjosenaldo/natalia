{
module Lexical.Lexemes (getTokens, Token(..), typeName, get_pos, get_id_name) where

import System.IO
import System.IO.Unsafe
}

%wrapper "posn"

$digit = [0-9]
$alpha = [a-zA-Z]   -- alphabetic characters

tokens :-

  -- TYPES  ------------------------------------------------

  int                              { \p s -> Type s (getLC p)}
  long                             { \p s -> Type s (getLC p)}
  float                            { \p s -> Type s (getLC p)}
  double                           { \p s -> Type s (getLC p)} 
  string                           { \p s -> Type s (getLC p)}
  bool                             { \p s -> Type s (getLC p)} 

  -- THINGS THAT ARE IGNORED ------------------------------

  $white+                          ; -- white spaces
  "#".*                            ; -- single-line comment
  "#"!   [.\n]*  !"#"              ; -- multi-line comment

  -- BLOCKS -----------------------------------------------

  \{                               { \p s -> LBrace (getLC p)}
  \}                               { \p s -> RBrace (getLC p)}
  \(                               { \p s -> LParen (getLC p)}
  \)                               { \p s -> RParen (getLC p)}
  \[                               { \p s -> LBracket (getLC p)}
  \]                               { \p s -> RBracket (getLC p)}
 
  -- SECTIONS ---------------------------------------------

  \@imports                        { \p s -> Import (getLC p)}
  \@globals                        { \p s -> Globals (getLC p)}
  \@subprograms                    { \p s -> Subprograms (getLC p)}
  \@main                           { \p s -> Main (getLC p)}
  
  -- SEPARATORS -------------------------------------------

  -- statement separator
  ";"                              { \p s -> SemiColon (getLC p)}

  -- LITERALS  ---------------------------------------------
  
  $digit+ \. $digit+               { \p s -> Double (read s) (getLC p)}
  $digit+                          { \p s -> Int (read s) (getLC p)}
  "True"                           { \p s -> Bool (read s) (getLC p) }
  "False"                          { \p s -> Bool (read s) (getLC p)}
 
  -- OPERATORS  --------------------------------------------

  =                                { \p s -> Assign (getLC p)}
  \%                               { \p s -> Mod (getLC p)}
  \^                               { \p s -> Expo (getLC p)}
  \+                               { \p s -> Plus (getLC p)}
  \-                               { \p s -> Minus (getLC p)}
  \*                               { \p s -> Times (getLC p)}
  \/                               { \p s -> Div (getLC p)}
  "+="                             { \p s -> PlusEquals (getLC p)}
  "-="                             { \p s -> MinusEqual (getLC p)}
  "*="                             { \p s -> AsteriskEquals (getLC p)}
  "/="                             { \p s -> BarEquals (getLC p)}
  "++"                             { \p s -> PlusPlus (getLC p)}
  "--"                             { \p s -> MinusMinus (getLC p)}
  "<"                              { \p s -> LessThan (getLC p)}
  ">"                              { \p s -> GreaterThan (getLC p)}
  "<="                             { \p s -> LessEquals (getLC p)}
  ">="                             { \p s -> GreaterEquals (getLC p)}
  "=="                             { \p s -> Equals (getLC p)}
  "!="                             { \p s -> Difference (getLC p)}
  "!"                              { \p s -> Negation (getLC p)}
  "&&"                             { \p s -> And (getLC p)}
  "||"                             { \p s -> Or (getLC p)}

  -- CONDITIONALS  -----------------------------------------

  if                               { \p s -> If (getLC p)}
  else                             { \p s -> Else (getLC p)}
  "else if"                        { \p s -> ElseIf (getLC p)} 

  -- LOOPS  ------------------------------------------------

  while                            { \p s -> Loop s (getLC p)}
  for                              { \p s -> Loop s (getLC p)}

  -- NAMES ------------------------------------------------

  -- filename
  [$alpha\_$digit]+   \.  [a-z]+   { \p s -> Filename s (getLC p)}

  -- identifier
  $alpha [$alpha $digit \_ \']*    { \p s -> Id s (getLC p)}

{
-- The token type:
data Token =

  -- BLOCKS -----------------------------------------------

  LBrace (Int, Int)          |
  RBrace (Int, Int)          |
  LParen (Int, Int)          |
  RParen (Int, Int)          |
  LBracket (Int, Int)        |
  RBracket (Int, Int)        |

  -- SECTIONS ---------------------------------------------
  
  Import (Int, Int)         |
  Globals (Int, Int)         |
  Subprograms (Int, Int)     |
  Main (Int, Int)            |

  -- SEPARATORS -------------------------------------------

  -- statement separator
  SemiColon (Int, Int)       |
  
  -- OPERATORS  --------------------------------------------

  Assign (Int, Int)          |
  Mod (Int, Int)             |
  Expo (Int, Int)            |
  Plus (Int, Int)            |
  Minus (Int, Int)           |
  Times (Int, Int)           |
  Div (Int, Int)             |
  PlusEquals (Int, Int)      |
  MinusEqual (Int, Int)      |
  AsteriskEquals (Int, Int)  |
  BarEquals (Int, Int)       |
  PlusPlus (Int, Int)        |
  MinusMinus (Int, Int)      |
  LessThan (Int, Int)        |
  GreaterThan (Int, Int)     |
  LessEquals (Int, Int)      |
  GreaterEquals (Int, Int)   |
  Equals (Int, Int)          |
  Negation (Int, Int)        |
  And (Int, Int)             |
  Or (Int, Int)              |
  Difference (Int, Int)      |
  
  -- CONDITIONALS  -----------------------------------------

  If (Int, Int)              |
  Else (Int, Int)            |
  ElseIf (Int, Int)          |

  -- TYPES  ------------------------------------------------

  Type String (Int, Int)     |  
  
  -- LOOPS  ------------------------------------------------

  Loop String (Int, Int)     |

  -- NAMES ------------------------------------------------

  -- identifier
  Id String (Int, Int)       |

  -- file name
  Filename String (Int, Int) |

  -- LITERALS  ---------------------------------------------

  Int Integer (Int, Int)         |
  Double Double (Int, Int)       |
  Bool Bool (Int, Int)        

  ----------------------------------------------------------

  deriving (Eq,Show)

typeName :: Token -> String
typeName (Type x p) = x
typeName _ = error "typeName error"

get_id_name :: Token -> String
get_id_name (Id s _) = s

get_pos :: Token -> (Int, Int)
get_pos (LBrace p) = p
get_pos (RBrace p) = p
get_pos (LParen p) = p
get_pos (RParen p) = p
get_pos (LBracket p) = p
get_pos (RBracket p) = p
get_pos (Import p) = p
get_pos (Globals p) = p
get_pos (Subprograms p) = p
get_pos (Main p) = p
get_pos (SemiColon p) = p
get_pos (Assign p) = p
get_pos (Mod p) = p
get_pos (Expo p) = p
get_pos (Plus p) = p
get_pos (Minus p) = p
get_pos (Times p) = p
get_pos (Div p) = p
get_pos (PlusEquals p) = p
get_pos (MinusEqual p) = p
get_pos (AsteriskEquals p) = p
get_pos (BarEquals p) = p
get_pos (PlusPlus p) = p
get_pos (MinusMinus p) = p
get_pos (LessThan p) = p
get_pos (GreaterThan p) = p
get_pos (LessEquals p) = p
get_pos (GreaterEquals p) = p
get_pos (Equals p) = p
get_pos (Negation p) = p
get_pos (And p) = p
get_pos (Or p) = p
get_pos (Difference p) = p
get_pos (If p) = p
get_pos (Else p) = p
get_pos (ElseIf p) = p
get_pos (Type _ p) = p
get_pos (Loop _ p) = p
get_pos (Id _ p) = p
get_pos (Filename _ p) = p
get_pos (Int _ p) = p
get_pos (Double _ p) = p
get_pos (Bool _ p) = p
 
getLC (AlexPn _ l c) = (l, c) 

getTokens fn = unsafePerformIO (getTokensAux fn)

getTokensAux fn = do {fh <- openFile fn ReadMode;
                      s <- hGetContents fh;
                      return (alexScanTokens s)}
}