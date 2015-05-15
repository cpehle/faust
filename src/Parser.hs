module Parser where

import           Control.Applicative ((<|>),(<$>),(<*>),(*>),(<**>), (<*),(<$),pure)
import           Data.Char (generalCategory, GeneralCategory(MathSymbol), isLetter, isAlphaNum, isDigit, digitToInt)
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.List (foldl')
import           Data.Ratio
import qualified Syntax

import           Control.Monad
import           Control.Monad.State.Strict


import           Text.Parser.Char (CharParsing, oneOf, satisfy, char)
import           Text.Parser.Combinators
import           Text.Parser.Token
import           Text.Parser.Token.Highlight
import           Text.Parser.Token.Style
import           Text.Trifecta.Parser


type FaustParser = StateT FParser FaustInnerParser
newtype FaustInnerParser a = FaustInnerParser { runInnerParser :: Parser a }
        deriving (Monad, Functor, MonadPlus, Applicative, Alternative, CharParsing, LookAheadParsing, DeltaParsing, MarkParsing Delta, Monoid, TokenParsing)

deriving instance Parsing FaustInnerParser
type MonadicParsing m = (DeltaParsing m, LookAheadParsing m, TokenParsing m, Monad m)

data FState = FState { faust_pos :: (Int, Int)}
instance Show FState where
         show = const "{internal state}"

faustCommentStyle :: CommentStyle
faustCommentStyle = CommentStyle "" "" "%" False

faustReservedIdents :: [String]
faustReservedIdents
  = [ "let"
    , "match"
    , "merge"
    , "receive"
    , "module"
    , "fn"
    , "iota"
    , "forall"
    ]

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

monadicoperators :: TokenParsing m => IdentifierStyle m
monadicoperators = IdentifierStyle { _styleName = "monadic"
                                   , _styleStart = oneOf "|⍟*⌈⌊○!+−×÷~⍳⌽↑⍴⍕"
                                   , _styleLetter = oneOf ""
                                   , _styleReserved = set []
                                   , _styleHighlight = Operator
                                   , _styleReservedHighlight = ReservedOperator
                                   }

identifiers :: TokenParsing m => IdentifierStyle m
identifiers = IdentifierStyle { _styleName = "identifier"
                              , _styleStart = satisfy isLetter <|> char '_'
                              , _styleLetter   = satisfy isAlphaNum <|> oneOf "_'"
                              , _styleReserved = set faustReservedIdents
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

{-| expressionList:
	'\n'
	statementList '\n'
-}

{-|
statementList:
      statement
      statement ';' statement
-}
statementList = choice [try sepBy ';' statement, statement]
{-|
   statement:
      var '=' expression
      expression
-}

{-| operand
	number
	vector
	operand [ expr ]...
	unop expr
-}

{-| expr
	operand
	operand binop expr
-}
expression = do
    op <- operand
    try binop

{-|
 index
	expr
	expr [ expr ]
	expr [ expr ] [ expr ] ....
-}


{-| number
	integer
	rational
	variable
	'(' expr ')'
-}
number = literal <|> variable <|> parens expression

variable :: Parser Syntax.Term
variable = Syntax.Variable <$> ident identifiers

literal :: Parser Syntax.Term
literal = Syntax.Literal <$> (ratOrIntLit <|> stringLit)
  where ratOrIntLit = either Syntax.Integer Syntax.Rational <$> integerOrRational
        stringLit = Syntax.String <$> stringLiteral
