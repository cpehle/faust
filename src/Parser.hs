-- | Parser of the platon language.
module Parser where
import           Control.Applicative ((<|>),(<$>),(<*>),(*>),(<**>),(<$),pure)
import           Data.Char (generalCategory, GeneralCategory(MathSymbol), isLetter, isAlphaNum, isDigit, digitToInt)
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.List (foldl')
import           Data.Ratio
import qualified Syntax
import           Text.Parser.Char (CharParsing, oneOf, satisfy, char)
import           Text.Parser.Combinators
import           Text.Parser.Token
import           Text.Parser.Token.Highlight
import           Text.Parser.Token.Style
import           Text.Trifecta.Parser

platonCommentStyle :: CommentStyle
platonCommentStyle = CommentStyle "" "" "%" False

platonReservedIdents :: [String]
platonReservedIdents = ["let", "case", "merge", "receive", "module", "fn", "in", "forall"]

aplSymbols :: String
aplSymbols = ['⌶'..'⍺']
  
isMathSymbol :: Char -> Bool
isMathSymbol c = case generalCategory c of
                   MathSymbol -> True
                   _ -> False

set :: [String] -> HashSet String
set = HashSet.fromList             

operators :: TokenParsing m => IdentifierStyle m
operators = IdentifierStyle { _styleName = "operator" 
                            , _styleStart = satisfy isMathSymbol <|> oneOf (":!#$%&*+./<=>?@\\^|-~" ++ aplSymbols) 
                            , _styleLetter = satisfy isMathSymbol <|> oneOf (":!#$%&*+./<=>?@\\^|-~" ++ aplSymbols)
                            , _styleReserved = set [".", ":", "=", "\\","|", "~", "⟹", "←", "→", "->", "<-"]
                            , _styleHighlight = Operator
                            , _styleReservedHighlight = ReservedOperator
                            }

identifiers :: TokenParsing m => IdentifierStyle m
identifiers = IdentifierStyle { _styleName = "identifier"
                              , _styleStart = satisfy isLetter <|> char '_'
                              , _styleLetter   = satisfy isAlphaNum <|> oneOf "_'"
                              , _styleReserved = set platonReservedIdents
                              , _styleHighlight = Identifier
                              , _styleReservedHighlight = ReservedIdentifier                     
                              }

integerOrRational :: TokenParsing m => m (Either Integer Rational)
integerOrRational = token (highlight Number ior <?> "number")
  where ior = mneg <$> optional (oneOf "+-¯") <*> naturalRational
        mneg (Just '-') nr = either (Left . negate) (Right . negate) nr
        mneg (Just '¯') nr = either (Left . negate) (Right . negate) nr
        mneg _ nr = nr

naturalRational :: TokenParsing m => m (Either Integer Rational)
naturalRational = char '0' *> zeroNumRational
               <|> decimalRational
zeroNumRational :: TokenParsing m => m (Either Integer Rational)               
zeroNumRational = Left <$> (hexadecimal <|> octal) 
                  <|> decimalRational 
                  <|> pure 0 <**> try fractRational 
                  <|> pure (Left 0)
decimalRational :: TokenParsing m => m (Either Integer Rational)                  
decimalRational = decimal <**> option Left (try fractRational)
fractRational :: TokenParsing m => m (Integer -> Either Integer Rational)
fractRational = (Right .) <$> fractExponent
{-# INLINE fractRational #-} 

sign :: TokenParsing m => m (Integer -> Integer)
sign = highlight Operator
     $ negate <$ char '-'
   <|> id <$ char '+'
   <|> pure id
           
number :: TokenParsing m => Integer -> m Char -> m Integer
number base baseDigit =
  foldl' (\x d -> base*x + toInteger (digitToInt d)) 0 <$> some baseDigit                                                                                                    
fractExponent :: TokenParsing m => m (Integer -> Rational)
fractExponent = (\fract expo n -> (fromInteger n + fract) * expo) <$> fraction <*> option 1 exponent'
            <|> (\expo n -> fromInteger n * expo) <$> exponent' where
  fraction :: CharParsing m => m Rational
  fraction = Prelude.foldr op (0::Rational) <$> (char '.' *> (some (satisfy isDigit) <?> "fraction"))
  op :: Char -> Rational -> Rational
  op d f = (f + fromIntegral (digitToInt d)) / 10
  exponent' = ((\f e -> power (f e)) <$ oneOf "eE" <*> sign <*> (decimal <?> "exponent")) <?> "exponent"
  power e
    | e < 0     = 1 % (10 ^ (-e))
    | otherwise = 10^e

{-
  Grammar for expressions:
  ========================

  term ::= literal
         | var
         | term1 term2
         | let var = term1 in term2
         | term : sig
         | ( term )
       
  literal ::= integer
            | rational
            | string
-}    

parseTerm :: Parser Syntax.Term
parseTerm = do { whiteSpace
               ; t <- term
               ; eof
               ; return t } 

variable :: Parser Syntax.Term
variable = Syntax.Variable <$> ident identifiers
         
literal :: Parser Syntax.Term
literal = Syntax.Lit <$> (ratOrIntLit <|> stringLit)
  where ratOrIntLit = either Syntax.Integer Syntax.Rational <$> integerOrRational
        stringLit = Syntax.String <$> stringLiteral

atom :: Parser Syntax.Term
atom = choice [parens term, variable, literal]

term :: Parser Syntax.Term
term = choice [try ann, nonAnn] where
  ann :: Parser Syntax.Term
  ann = do tm <- nonAnn
           reserve operators ":"
           ty <- sigma
           return $ Syntax.Ann tm ty
  nonAnn = choice [lambda, rlet, application]

lambda :: Parser Syntax.Term
lambda = reserve operators "\\" *> choice [try annLambda, ordlambda] where                      
  annLambda = do 
    (v,ty) <- parens (do v <- ident identifiers
                         reserve operators ":"
                         ty <- sigma
                         return (v,ty))
    reserve operators "."
    body <- term
    return (Syntax.ALam v ty body)
  
  ordlambda = do 
    v <- ident identifiers
    reserve operators "."
    body <- term
    return (Syntax.Lam v body)

rlet :: Parser Syntax.Term
rlet = do 
  reserve identifiers "let"
  v <- ident identifiers
  reserve operators "="
  rhs <- term
  reserve identifiers "in"
  body <- term
  return (Syntax.Let v rhs body)

application :: Parser Syntax.Term
application = foldl Syntax.App <$> atom <*> many atom
{- 

   Grammar for types:
   ==================
   sig = rho | forall tv1 .. tvn . rho
   rho = tv  | base | sig -> rho
   base = Integer | Bool | Rational

-}            

sigma :: Parser Syntax.Type
sigma = choice [try (parens sigma), sigma', rho]
atomSigma :: Parser Syntax.Type
atomSigma = choice [try (parens sigma), atomRho]
sigma' :: Parser Syntax.Type
sigma' = do 
  tvs <- reserve identifiers "forall" *> (map Syntax.BoundTv <$> many (ident identifiers))
  reserve operators "."
  ty <- rho
  return $ Syntax.ForAll tvs ty                      
            
rho :: Parser Syntax.Type
rho = choice [try rfun, atomRho]
rfun :: Parser Syntax.Type
rfun = do 
  arg <- atomSigma
  reserve operators "→" <|> reserve operators "->"
  res <- rho
  return $ Syntax.Fun arg res                                   

atomRho :: Parser Syntax.Type
atomRho = choice [try tyvariable, tyconstant, parens rho]

tau :: Parser Syntax.Type
tau = choice [try tfun, atomTau]
atomTau :: Parser Syntax.Type
atomTau = choice [try tyvariable, tyconstant, parens tau]             
-- TODO: Use Haskell convention of separating typevariables from constants / constructors
tyvariable :: Parser Syntax.Type
tyvariable = Syntax.TyVar . Syntax.BoundTv <$> ident identifiers -- TODO: Annotate origin, otherwise predefined Typeconstants could be shadowed..."
-- This is a Hack.
tyconstant :: Parser Syntax.Type
tyconstant =  choice [ try $ do { "Integer"  <- ident identifiers ; return $ Syntax.TyCon Syntax.IntegerT }
                     , do { "Bool" <- ident identifiers ; return $ Syntax.TyCon Syntax.BooleanT }]
tfun :: Parser Syntax.Type
tfun = do 
  arg <- atomTau
  reserve operators "→" <|> reserve operators "->"
  res <- tau
  return $ Syntax.Fun arg res                                   

{- 
  Grammar for modules
  ===================


-}
