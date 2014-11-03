module Parser where

import Text.ParserCombinators.Parsec 
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language (emptyDef)
import Term
import Formula
import Print

type FormulaS = Formula String String String

toFormula' = parse formula ""

toFormula  = either (\ x -> error ("Syntax error in formula\n  "++ show x)) id .toFormula'

toTerm' = parse term ""

toTerm  = either (\ x -> error ("Syntax error in term\n  "++show x)) id .toTerm'

formulaTable = [[Prefix 
                 (do
                  reserved "not"
                  return $ \f -> Conn Not [f] 
                 )],
                [relOp "&" And AssocLeft],
                [relOp "|" Or AssocLeft],
                [relOp "-->" Imp AssocRight]]
                
  where relOp s op a = Infix (reservedOp s >> return ( \a b -> Conn op [a,b])) a

formula = buildExpressionParser formulaTable atomicFormula

tryChoice = choice . map try

atomicFormula = tryChoice [parens formula,
                           numEq,
                           prop,
                           truth,
                           falsity,
                           forall,
                           ex]

truth = reserved "True" >> return (Conn T [])
falsity = reserved "False" >> return (Conn F [])

numEq = do
  t <- term
  reservedOp "="
  t' <- term
  return $ Rel "=" [t, t']

prop :: Parser FormulaS
prop = do
  s <- propName
  xs <- option [] $ do 
    parens $ commaSep term
  return $ Rel s xs

forall = do
  reserved "forall"
  v <- var
  dot  
  f <- formula 
  return $ Quant All v f

ex = do
  reserved "exists"
  v <- var
  dot  
  f <- formula
  return $ Quant Exist v f

term = buildExpressionParser table atom 
  
                   

table = [
  [Prefix (do
          reservedOp "-"
          return $ \a -> Fun False "negate" [a]
          )],
  [op "*"],
  [op "+", op "-"]]
  where op s = 
          Infix ( do
            reservedOp s
            return $ \a b -> Fun False s [a,b]) AssocLeft

atom = parens term <|> try prefix <|> lit <|> tVar
  where prefix = do f <- var
                    xs <- parens(commaSep term)
                    return (Fun False f xs)

tVar = do
       v <- var
       return $ Var v

lit = do
  n <- natural
  return $ peano n

peano 0 = Fun False "0" []
peano m = Fun False "S" [peano (m-1)]
  
firstOrderDef = emptyDef{P.reservedNames = ["forall","exists","not","True","False"],
                         P.reservedOpNames = ["+","-","~","&","|","-->","*"],
                         P.identStart = letter <|> char '?' <|> char '_'}
                
lexer = P.makeTokenParser firstOrderDef

parens = P.parens lexer
var = P.identifier lexer
propName = P.identifier lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer
dot = P.dot lexer
natural = P.natural lexer
symbol = P.symbol lexer
integer = P.integer lexer
whitespace = P.whiteSpace lexer
commaSep = P.commaSep lexer