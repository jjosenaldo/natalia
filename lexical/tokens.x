{
module Main (main) where
}

%wrapper "basic"

$digit = 0-9      -- digits
$alpha = [a-zA-Z]   -- alphabetic characters

tokens :-

  $white+ ;
  \{ {\s -> LBrace}
  \} {\s -> RBrace}
  imports {\s -> Imports} 
  globais {\s -> Globals}
  subprogramas {\s -> Subprograms}
  \"\([^\0 !$`&*()+]\|\\\(\ |\!|\$|\`|\&|\*|\(|\)|\+\)\)\+\" {\s -> Filename}
  ";" { \s -> SemiColon}


imports
{
  "lib1.cpp";
  "lib2.cpp";

}






  "--".*                               ;
  program                              { \s -> Program }
  var                                  { \s -> Var }
  begin                                { \s -> Begin}
  end                                  { \s -> End}
  :                                    { \s -> Colon}
  ";"                                  { \s -> SemiColon}
  int                                  { \s -> Type s}
  :=                                   { \s -> Assign}
  if                                   { \s -> If}
  then                                 { \s -> Then}
  write                                { \s -> Write}
  >                                    { \s -> Greater}
  $digit+                              { \s -> Int (read s) }
  $alpha [$alpha $digit \_ \']*        { \s -> Id s }
  \" $alpha [$alpha $digit \\ ! \_ \']* \"  { \s -> String s}

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
  Program |
  Var     |
  Begin   |
  End     |
  Colon   |
  SemiColon |
  Assign    | 
  If  |
  Then |
  Write |
  Greater |
  Type String |
  Id String |
  Int Int |
  String String
  deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}