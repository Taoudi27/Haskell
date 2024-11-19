-- TP-1  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}
--
-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Pretty printer
-- - Implantation du langage

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------

import Text.ParserCombinators.Parsec -- Bibliothèque d'analyse syntaxique.
import Data.Char                -- Conversion de Chars de/vers Int et autres.
import System.IO                -- Pour stdout, hPutStr   pour les fichiers en gros

---------------------------------------------------------------------------
-- La représentation interne des expressions de notre language           --
---------------------------------------------------------------------------
data Sexp = Snil                        -- La liste vide
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
          | Snode Sexp [Sexp]           -- Une liste non vide
          -- Génère automatiquement un pretty-printer et une fonction de
          -- comparaison structurelle.
          deriving (Show, Eq)

-- Exemples:
-- (+ 2 3) ==> Snode (Ssym "+")
--                   [Snum 2, Snum 3]
--
-- (/ (* (- 68 32) 5) 9)
--     ==>
-- Snode (Ssym "/")
--       [Snode (Ssym "*")
--              [Snode (Ssym "-")
--                     [Snum 68, Snum 32],
--               Snum 5],
--        Snum 9]

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()  -- reconnait char: c
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()  -- ignore ce qui start par ;
pComment = do { pChar ';'; _ <- many (satisfy (\c -> not (c == '\n')));
                (pChar '\n' <|> eof); return ()
              }
-- N'importe quelle combinaison d'espaces et de commentaires est considérée
-- comme du blanc.
pSpaces :: Parser ()
pSpaces = do { _ <- many (do { _ <- space ; return () } <|> pComment);
               return () }

-- Un nombre entier est composé de chiffres.
integer     :: Parser Int  -- parser reconnait entier pos et neg
integer = do c <- digit
             integer' (digitToInt c)
          <|> do _ <- satisfy (\c -> (c == '-'))
                 n <- integer
                 return (- n)
    where integer' :: Int -> Parser Int
          integer' n = do c <- digit
                          integer' (10 * n + (digitToInt c))
                       <|> return n

-- Les symboles sont constitués de caractères alphanumériques et de signes
-- de ponctuations.
pSymchar :: Parser Char
pSymchar    = alphaNum <|> satisfy (\c -> c `elem` "!@$%^&*_+-=:|/?<>")
pSymbol :: Parser Sexp -- reconnait symb ou nbre et le converti en sexp
pSymbol= do { s <- many1 (pSymchar);
              return (case parse integer "" s of
                        Right n -> Snum n
                        _ -> Ssym s)
            }

---------------------------------------------------------------------------
-- Analyseur syntaxique                                                  --
---------------------------------------------------------------------------

-- La notation "'E" est équivalente à "(quote E)"
pQuote :: Parser Sexp   --empeche evaluation de cqui suit
pQuote = do { pChar '\''; pSpaces; e <- pSexp;
              return (Snode (Ssym "quote") [e]) }

-- Une liste est de la forme:  ( {e} [. e] )
pList :: Parser Sexp   -- reconnait list de sexp
pList  = do { pChar '('; pSpaces;
              ses <- pTail;
                    return (case ses of [] -> Snil
                                        se : ses' -> Snode se ses')
            }
pTail :: Parser [Sexp] --analyse element liste jusqua cquelle se femre par )
pTail  = do { pChar ')'; return [] }
     -- <|> do { pChar '.'; pSpaces; e <- pSexp; pSpaces;
     --          pChar ')' <|> error ("Missing ')' after: " ++ show e);
     --          return e }
     <|> do { e <- pSexp; pSpaces; es <- pTail; return (e : es) }

-- Accepte n'importe quel caractère: utilisé en cas d'erreur.
pAny :: Parser (Maybe Char)
pAny = do { c <- anyChar ; return (Just c) } <|> return Nothing -- si aucune entree est dispo

-- Une Sexp peut-être une liste, un symbol ou un entier.
pSexpTop :: Parser Sexp  -- analyse sexp principale(listes, expressions, symboles, nbres)
pSexpTop = do { pSpaces;
                pList <|> pQuote <|> pSymbol
                <|> do { x <- pAny;
                         case x of
                           Nothing -> pzero
                           Just c -> error ("Unexpected char '" ++ [c] ++ "'")
                       }
              }

-- On distingue l'analyse syntaxique d'une Sexp principale de celle d'une
-- sous-Sexp: si l'analyse d'une sous-Sexp échoue à EOF, c'est une erreur de
-- syntaxe alors que si l'analyse de la Sexp principale échoue cela peut être
-- tout à fait normal.
pSexp :: Parser Sexp --
pSexp = pSexpTop <|> error "Unexpected end of stream"

-- Une séquence de Sexps.
pSexps :: Parser [Sexp]  -- reconnait sequence de sexp
pSexps = do pSpaces
            many (do e <- pSexpTop
                     pSpaces
                     return e)

-- Déclare que notre analyseur syntaxique peut-être utilisé pour la fonction
-- générique "read".
instance Read Sexp where  -- read lis des chaines et les convertit en sexp
    readsPrec _p s = case parse pSexp "" s of
                      Left _ -> []
                      Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Sexp Pretty Printer                                                   --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS   -- convertit un sexp en string lisible
showSexp' Snil = showString "()"       -- si liste vide
showSexp' (Snum n) = showsPrec 0 n     -- gere les nombres
showSexp' (Ssym s) = showString s      -- gere les symboles
showSexp' (Snode h t) =                -- gere les noeuds (liste non vide de sexp)
    let showTail [] = showChar ')'
        showTail (e : es) =
            showChar ' ' . showSexp' e . showTail es
    in showChar '(' . showSexp' h . showTail t

-- On peut utiliser notre pretty-printer pour la fonction générique "show"
-- (utilisée par la boucle interactive de GHCi).  Mais avant de faire cela,
-- il faut enlever le "deriving Show" dans la déclaration de Sexp.
{-
instance Show Sexp where
    showsPrec p = showSexp'
-}

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de Hugs/GHCi:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Représentation intermédiaire Lexp                                     --
---------------------------------------------------------------------------

type Var = String

data Lexp = Lnum Int             -- Constante entière.
          | Lbool Bool           -- Constante Booléenne.
          | Lvar Var             -- Référence à une variable.
          | Ltest Lexp Lexp Lexp -- Expression conditionelle.
          | Lfob [Var] Lexp      -- Construction de fobjet.
          | Lsend Lexp [Lexp]    -- Appel de fobjet.
          | Llet Var Lexp Lexp   -- Déclaration non-récursive.
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Lfix [(Var, Lexp)] Lexp
          deriving (Show, Eq)

-- Première passe simple qui analyse une Sexp et construit une Lexp équivalente.
s2l :: Sexp -> Lexp

-- Nombres : conversion directe de Snum en Lnum
s2l (Snum n) = Lnum n

-- Valeurs booléennes : conversion de symboles "true" et "false" en Lbool
s2l (Ssym "true") = Lbool True
s2l (Ssym "false") = Lbool False

-- Variables : conversion d'un symbole en variable
s2l (Ssym s) = Lvar s

-- Expressions 'if' : conversion d'une expression conditionnelle
s2l (Snode (Ssym "if") [cond, thenExpr, elseExpr]) =
    Ltest (s2l cond) (s2l thenExpr) (s2l elseExpr)

-- Expressions 'let' : conversion d'une expression let avec
-- une variable, un corps et une valeur
s2l (Snode (Ssym "let") [Ssym var, valueExpr, bodyExpr]) =
    Llet var (s2l valueExpr) (s2l bodyExpr)

-- Expressions 'fob' : conversion d'une fonction objet (fob) 
-- avec ses paramètres et son corps
s2l (Snode (Ssym "fob") [paramsSexp, bodyExpr]) =
    Lfob (extractParams paramsSexp) (s2l bodyExpr)

-- Expressions 'fix' : conversion d'une expression récursive avec 
-- des liaisons et un corps
s2l (Snode (Ssym "fix") [bindingsSexp, bodyExpr]) =
    Lfix (extractBindings bindingsSexp) (s2l bodyExpr)
s2l (Snode (Ssym "fix") _) =
    error "Invalid 'fix' expression: expected (fix bindings body)"

-- Appel de fonction : conversion d'un appel de fonction avec ses arguments
s2l (Snode funcExpr args) =
    Lsend (s2l funcExpr) (map s2l args)

-- Cas d'erreur : levée d'une erreur pour une expression inconnue
s2l se = error ("Expression inconnue: " ++ showSexp se)

-- Convertir une liste Sexp en liste Haskell
sexpToList :: Sexp -> [Sexp]
sexpToList Snil = []
sexpToList (Snode h t) = h : t
sexpToList se = [se]

-- Extraire les paramètres d'une liste Sexp (pour une fonction fob)
extractParams :: Sexp -> [Var]
extractParams se = map fromSsym (sexpToList se)

-- Convertir un symbole Ssym en variable Var
fromSsym :: Sexp -> Var
fromSsym (Ssym s) = s
fromSsym se = error ("Symbole attendu, " ++ showSexp se ++ " reçu")

-- Extraire les liaisons d'une expression 'fix'
extractBindings :: Sexp -> [(Var, Lexp)]
extractBindings se = map processBinding (sexpToList se)

-- Traiter chaque liaison dans 'fix'
processBinding :: Sexp -> (Var, Lexp)
processBinding (Snode (Snode (Ssym var) paramsTail) [body]) =
    (var, Lfob (map fromSsym paramsTail) (s2l body))
processBinding (Snode (Ssym var) [expr]) =
    (var, s2l expr)
processBinding se = error ("Liaison invalide dans fix: " ++ showSexp se)


---------------------------------------------------------------------------
-- Représentation du contexte d'exécution                                --
---------------------------------------------------------------------------

-- Type des valeurs manipulées à l'exécution.
data Value = Vnum Int
           | Vbool Bool
           | Vbuiltin ([Value] -> Value)
           | Vfob VEnv [Var] Lexp

instance Show Value where        -- pour vhci
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec p (Vbool b) = showsPrec p b
    showsPrec _ (Vbuiltin _) = showString "<primitive>"
    showsPrec _ (Vfob _ _ _) = showString "<fobjet>"

type VEnv = [(Var, Value)]         -- environnement execution

-- L'environnement initial qui contient les fonctions prédéfinies.
env0 :: VEnv
env0 = let binop f op =    -- fonction binaire (2 args)
              Vbuiltin (\vs -> case vs of
                         [Vnum n1, Vnum n2] -> f (n1 `op` n2)
                         [_, _] -> error "Pas un nombre"
                         _ -> error "Nombre d'arguments incorrect")

          in [("+", binop Vnum (+)),
              ("*", binop Vnum (*)),
              ("/", binop Vnum div),
              ("-", binop Vnum (-)),
              ("<", binop Vbool (<)),
              (">", binop Vbool (>)),
              ("≤", binop Vbool (<=)),
              ("≥", binop Vbool (>=)),
              ("=", binop Vbool (==)),
              ("true",  Vbool True),
              ("false", Vbool False)]

---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

eval :: VEnv -> Lexp -> Value

-- Nombres : retourne directement la valeur numérique
eval _ (Lnum n) = Vnum n

-- Booléens : retourne directement la valeur booléenne
eval _ (Lbool b) = Vbool b

-- Variables : recherche la variable dans l'environnement
eval env (Lvar x) =
    case lookup x env of
      Just val -> val
      Nothing -> error ("Variable non définie: " ++ x)

-- Expressions conditionnelles : évalue la condition 
-- et retourne la branche correspondante
eval env (Ltest cond e1 e2) =
    case eval env cond of
      Vbool True -> eval env e1
      Vbool False -> eval env e2
      _ -> error "Condition non booléenne"

-- FOBs (fonctions objets) : crée un objet fonctionnel (fermeture)
eval env (Lfob params body) = Vfob env params body

-- Appel de fonctions : évalue la fonction et ses arguments,
-- puis applique la fonction
eval env (Lsend funcExpr argExprs) =
    let func = eval env funcExpr        -- évalue la fonction
        args = map (eval env) argExprs  -- évalue les arguments
    in case func of
         Vbuiltin f -> f args  -- si fonction pré-définie, applique directement
         Vfob closureEnv params body ->  -- si c'est un fob (fermeture)
           if length params == length args
           then let newEnv = zip params args ++ closureEnv  
                in eval newEnv body 
           else error ("Nombre d'arguments incorrect: attendu " ++
                       show (length params) ++ ", reçu " ++ show (length args))
         _ -> error "Appel de fonction sur une valeur non-fonction"

-- Expressions 'let' : évalue la valeur de la variable,
-- puis le corps avec la variable ajoutée à l'environnement
eval env (Llet x e1 e2) =
    let v1 = eval env e1
        newEnv = (x, v1) : env
    in eval newEnv e2

-- Expressions 'fix' : évalue des liaisons récursives mutuelles
eval env (Lfix bindings e) =
    let recEnv = buildRecEnv bindings env  -- crée un environnement récursif
    in eval recEnv e  -- évalue l'expression dans cet environnement

-- Helper function : crée un environnement récursif 
-- pour gérer les liaisons de 'fix'
buildRecEnv :: [(Var, Lexp)] -> VEnv -> VEnv
buildRecEnv bindings env = env'
  where
    -- évalue chaque binding en utilisant l'environnement récursif
    env' = [(x, eval env' e) | (x, e) <- bindings] ++ env

---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

evalSexp :: Sexp -> Value  -- converti sexp en lexp avc s21 et evalue avc eval
evalSexp = eval env0 . s2l

-- Lit un fichier contenant plusieurs Sexps, les évalues l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename =
    do inputHandle <- openFile filename ReadMode
       hSetEncoding inputHandle utf8
       s <- hGetContents inputHandle
       (hPutStr stdout . show)
           (let sexps s' = case parse pSexps filename s' of
                             Left _ -> [Ssym "#<parse-error>"]
                             Right es -> es
            in map evalSexp (sexps s))
       hClose inputHandle

sexpOf :: String -> Sexp
sexpOf = read

lexpOf :: String -> Lexp
lexpOf = s2l . sexpOf

valOf :: String -> Value
valOf = evalSexp . sexpOf
