{
module Main (main) where
}

%wrapper "basic"

$digit = [0-9]
$alpha = [a-zA-Z]   -- alphabetic characters

tokens :-

  $white+                          ; -- white spaces
  "#".*                            ; -- single-line comment
  "#"!   [.\n]*  !"#"              ; -- multi-line comment
  \{                               {\s -> LBrace}
  \}                               {\s -> RBrace}
  \@imports                        {\s -> Imports} 
  \@globals                        {\s -> Globals}
  \@subprograms                    {\s -> Subprograms}
  \@main                           {\s -> Main}
  [$alpha\_$digit]+   \.  [a-z]+   {\s -> Filename s}
  ";"                              {\s -> SemiColon}
  $digit+                          { \s -> Int (read s) }
  =                                { \s -> Assign}
  int                              { \s -> Type s}
  $alpha [$alpha $digit \_ \']*    { \s -> Id s }


{
-- The token type:
data Token =
  LBrace |
  RBrace     |
  Imports   |
  Globals     |
  Subprograms   |
  Filename String |
  SemiColon |
  Assign |
  Type String |
  Id String |
  Int Int |
  Main
  deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}