-- TP-1  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}
--
-- Ce fichier définit les fonctionnalités suivantes :
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Pretty printer
-- - Implantation du langage

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------

import Text.ParserCombinators.Parsec -- Bibliothèque d'analyse syntaxique.
import Data.Char                -- Conversion de Chars de/vers Int et autres.
import System.IO                -- Pour stdout, hPutStr

---------------------------------------------------------------------------
-- La représentation interne des expressions de notre langage            --
---------------------------------------------------------------------------
data Sexp = Snil                        -- La liste vide
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
          | Snode Sexp [Sexp]           -- Une liste non vide
          -- Génère automatiquement un pretty-printer et une fonction de
          -- comparaison structurelle.
          deriving (Show, Eq)

-- Exemples :
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

pChar :: Char -> Parser ()
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()
pComment = do
    { pChar ';';
      _ <- many (satisfy (\c -> not (c == '\n')));
      (pChar '\n' <|> eof);
      return ()
    }

-- N'importe quelle combinaison d'espaces et de commentaires est considérée
-- comme du blanc.
pSpaces :: Parser ()
pSpaces = do
    { _ <- many (do { _ <- space; return () } <|> pComment);
      return ()
    }

-- Un nombre entier est composé de chiffres.
integer :: Parser Int
integer = do c <- digit
             integer' (digitToInt c)
          <|> do _ <- satisfy (\c -> (c == '-'))
                 n <- integer
                 return (- n)
    where
      integer' :: Int -> Parser Int
      integer' n = do c <- digit
                      integer' (10 * n + (digitToInt c))
                   <|> return n

-- Les symboles sont constitués de caractères alphanumériques et de signes
-- de ponctuation.
pSymchar :: Parser Char
pSymchar = alphaNum <|>
           satisfy (\c -> not (isAscii c) || c `elem` "!@$%^&*_+-=:|/?<>")

pSymbol :: Parser Sexp
pSymbol = do
    { s <- many1 pSymchar;
      return (case parse integer "" s of
                Right n -> Snum n
                _ -> Ssym s)
    }

---------------------------------------------------------------------------
-- Analyseur syntaxique                                                  --
---------------------------------------------------------------------------

-- La notation "'E" est équivalente à "(quote E)"
pQuote :: Parser Sexp
pQuote = do
    { pChar '\'';
      pSpaces;
      e <- pSexp;
      return (Snode (Ssym "quote") [e])
    }

-- Une liste est de la forme :  ( {e} [. e] )
pList :: Parser Sexp
pList = do
    { pChar '(';
      pSpaces;
      ses <- pTail;
      return (case ses of
                [] -> Snil
                se : ses' -> Snode se ses')
    }

pTail :: Parser [Sexp]
pTail = do { pChar ')'; return [] }
     <|> do
        { e <- pSexp;
          pSpaces;
          es <- pTail;
          return (e : es)
        }

-- Accepte n'importe quel caractère : utilisé en cas d'erreur.
pAny :: Parser (Maybe Char)
pAny = do { c <- anyChar; return (Just c) } <|> return Nothing

-- Une Sexp peut être une liste, un symbole ou un entier.
pSexpTop :: Parser Sexp
pSexpTop = do
    { pSpaces;
      pList <|> pQuote <|> pSymbol
      <|> do
          { x <- pAny;
            case x of
              Nothing -> pzero
              Just c -> error ("Caractère inattendu '" ++ [c] ++ "'")
          }
    }

-- On distingue l'analyse syntaxique d'une Sexp principale de celle d'une
-- sous-Sexp : si l'analyse d'une sous-Sexp échoue à EOF, c'est une erreur de
-- syntaxe alors que si l'analyse de la Sexp principale échoue cela peut être
-- tout à fait normal.
pSexp :: Parser Sexp
pSexp = pSexpTop <|> error "Fin de flux inattendue"

-- Une séquence de Sexps.
pSexps :: Parser [Sexp]
pSexps = do
    pSpaces
    many (do { e <- pSexpTop; pSpaces; return e })

-- Déclare que notre analyseur syntaxique peut être utilisé pour la fonction
-- générique "read".
instance Read Sexp where
    readsPrec _p s = case parse pSexp "" s of
                      Left _ -> []
                      Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Sexp Pretty Printer                                                   --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Snode h t) =
    let showTail [] = showChar ')'
        showTail (e : es) = showChar ' ' . showSexp' e . showTail es
    in showChar '(' . showSexp' h . showTail t

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de Hugs/GHCi :
readSexp :: String -> Sexp
readSexp = read

showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Représentation intermédiaire Lexp                                     --
---------------------------------------------------------------------------

type Var = String

data Type = Terror String        -- Utilisé quand le type n'est pas connu.
          | Tnum                 -- Type des nombres entiers.
          | Tbool                -- Type des booléens.
          | Tfob [Type] Type     -- Type des fobjets.
          deriving (Show, Eq)

data Lexp = Lnum Int             -- Constante entière.
          | Lbool Bool           -- Constante Booléenne.
          | Lvar Var             -- Référence à une variable.
          | Ltype Lexp Type      -- Annotation de type.
          | Ltest Lexp Lexp Lexp -- Expression conditionnelle.
          | Lfob [(Var, Type)] Lexp -- Construction de fobjet.
          | Lsend Lexp [Lexp]    -- Appel de fobjet.
          | Llet Var Lexp Lexp   -- Déclaration non récursive.
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Lfix [(Var, Lexp)] Lexp
          deriving (Show, Eq)

-- Première passe simple qui analyse une Sexp et construit une Lexp équivalente.
-- Fonction auxiliaire pour analyser les arguments de fonction en paires (Var, Type)
parseArg :: Sexp -> (Var, Type)
parseArg (Snode (Ssym x) [t]) = (x, s2t t)
parseArg se = error ("Argument invalide: " ++ showSexp se)

-- Fonctions pour aider avec les listes
s2list :: Sexp -> [Sexp]
s2list Snil = []
s2list (Snode h t) = h : t
s2list se = error ("Pas une liste: " ++ showSexp se)

-- Fonction pour convertir une Sexp en Type
s2t :: Sexp -> Type
s2t (Ssym "Num") = Tnum
s2t (Ssym "Bool") = Tbool
s2t (Snode h t) =
    let ts = h : t
        tys = map s2t ts
    in if null tys
       then error "Liste de types vide"
       else if length tys == 1
            then head tys -- Gère correctement les types à un seul élément
            else Tfob (init tys) (last tys)
s2t se = error ("Type inconnu: " ++ showSexp se)

-- Fonction pour vérifier si une Sexp représente un type
isType :: Sexp -> Bool
isType (Ssym "Num") = True
isType (Ssym "Bool") = True
isType (Snode _ _) = True
isType _ = False

-- Première passe simple qui analyse une Sexp et construit une Lexp équivalente.
s2l :: Sexp -> Lexp
s2l (Snum n) = Lnum n
s2l (Ssym "true") = Lbool True
s2l (Ssym "false") = Lbool False
s2l (Ssym s) = Lvar s
s2l (Snode (Ssym "if") [e1, e2, e3]) =
    Ltest (s2l e1) (s2l e2) (s2l e3)
s2l (Snode (Ssym "let") [Ssym x, e1, e2]) =
    Llet x (s2l e1) (s2l e2)
-- Cas mis à jour pour 'fix'
s2l (Snode (Ssym "fix") [decls, body]) =
    let sdecl2ldecl :: Sexp -> (Var, Lexp)
        sdecl2ldecl (Snode (Ssym v) es) =
            if null es
            then error ("La déclaration de fonction n'a pas de corps: " ++ v)
            else let argsExps = init es
                     bodyExp = last es
                     args = map parseArg argsExps
                     innerBody = s2l bodyExp
                 in (v, Lfob args innerBody)
        sdecl2ldecl se = error ("Déclaration inconnue: " ++ showSexp se)
    in Lfix (map sdecl2ldecl (s2list decls)) (s2l body)
s2l (Snode (Ssym ":") [e, t]) =
    Ltype (s2l e) (s2t t)
s2l (Snode (Ssym "fob") [args, e]) =
    Lfob (map parseArg (s2list args)) (s2l e)
-- Le schéma général d'application de fonction vient avant le sucre syntaxique
s2l (Snode f es) =
    Lsend (s2l f) (map s2l es)
-- Gestion du sucre syntaxique
s2l se = error ("Expression SSlip inconnue: " ++ showSexp se)

---------------------------------------------------------------------------
-- Représentation du contexte d'exécution                                --
---------------------------------------------------------------------------

-- Type des valeurs manipulées à l'exécution.
data Value = Vnum Int
           | Vbool Bool
           | Vbuiltin ([Value] -> Value)
           | Vfob VEnv Int Dexp -- L'entier indique le nombre d'arguments.

instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec p (Vbool b) = showsPrec p b
    showsPrec _ (Vbuiltin _) = showString "<primitive>"
    showsPrec _ (Vfob _ _ _) = showString "<fobjet>"

type Env = [(Var, Type, Value)]

-- L'environnement initial qui contient les fonctions prédéfinies.
env0 :: Env
env0 = let binop f op =
              Vbuiltin (\vs -> case vs of
                         [Vnum n1, Vnum n2] -> f (n1 `op` n2)
                         [_, _] -> error "Pas un nombre"
                         _ -> error "Nombre d'arguments incorrect")
           intbin = Tfob [Tnum, Tnum] Tnum
           boolbin = Tfob [Tnum, Tnum] Tbool
       in [("+", intbin,  binop Vnum (+)),
           ("*", intbin,  binop Vnum (*)),
           ("/", intbin,  binop Vnum div),
           ("-", intbin,  binop Vnum (-)),
           ("<", boolbin, binop Vbool (<)),
           (">", boolbin, binop Vbool (>)),
           ("≤", boolbin, binop Vbool (<=)),
           ("≥", boolbin, binop Vbool (>=)),
           ("=", boolbin, binop Vbool (==)),
           ("true",  Tbool, Vbool True),
           ("false", Tbool, Vbool False)]

---------------------------------------------------------------------------
-- Vérification des types                                                --
---------------------------------------------------------------------------

type TEnv = [(Var, Type)]

-- `check c Γ e` renvoie le type de `e` dans l'environnement `Γ`.
-- Si `c` est vrai, on fait une vérification complète, alors que s'il
-- est faux, alors on présume que le code est typé correctement et on
-- se contente de chercher le type.
check :: Bool -> TEnv -> Lexp -> Type
check _ _ (Lnum _) = Tnum
check _ _ (Lbool _) = Tbool
check _ gamma (Lvar x) =
    case lookup x gamma of
        Just t  -> t
        Nothing -> Terror ("Variable non trouvée: " ++ x)
check c gamma (Ltype e t) =
    let t' = check c gamma e
    in if c then
           if t == t' then t
           else Terror ("L'annotation de type ne correspond pas: attendu "
                        ++ show t ++ ", obtenu " ++ show t')
       else t
check c gamma (Ltest e1 e2 e3) =
    let t1 = check c gamma e1
        t2 = check c gamma e2
        t3 = check c gamma e3
    in if c then
        if t1 == Tbool then
            if t2 == t3 then t2
            else Terror ("Les branches du if ont des types différents: "
                         ++ show t2 ++ " et " ++ show t3)
        else Terror ("La condition du if n'est pas Bool, obtenu " ++ show t1)
    else t2
check c gamma (Lsend e0 es) =
    let t0 = check c gamma e0
        ts = map (check c gamma) es
    in case t0 of
        Tfob tArgs tRet ->
            if c then
                if tArgs == ts then tRet
                else Terror ("Les types des arguments ne correspondent pas dans "
                             ++ "l'appel de fonction: attendu "
                             ++ show tArgs ++ ", obtenu " ++ show ts)
            else tRet
        _ -> Terror ("Tentative d'appeler un objet non fonction de type "
                     ++ show t0)
check c gamma (Lfob args e) =
    let gamma' = args ++ gamma
        tr = check c gamma' e
    in Tfob (map snd args) tr
check c gamma (Llet x e1 e2) =
    let t1 = check c gamma e1
        gamma' = (x, t1) : gamma
        t2 = check c gamma' e2
    in t2
check c gamma (Lfix xs e) =
    let tempGamma = map (\(x, _) -> (x, Terror "Type inconnu")) xs ++ gamma
        tis = [check False tempGamma ei | (_, ei) <- xs]
        gamma' = zip (map fst xs) tis ++ gamma
        te = check c gamma' e
    in if c then
        let tis' = [check c gamma' ei | (_, ei) <- xs]
        in if all (\(ti, ti') -> ti == ti') (zip tis tis') then te
           else Terror "Incompatibilité de type dans les liaisons fix"
       else te

---------------------------------------------------------------------------
-- Pré-évaluation
---------------------------------------------------------------------------

-- Dexp simplifie le code en éliminant deux aspects qui ne sont plus
-- utiles lors de l'évaluation :
-- - Les annotations de types.
-- - Les noms de variables, remplacés par des entiers qui représentent
--   la position de la variable dans l'environnement. On appelle ces entiers
--   des [index de De Bruijn](https://fr.wikipedia.org/wiki/Index_de_De_Bruijn).

type VarIndex = Int

data Dexp = Dnum Int             -- Constante entière.
          | Dbool Bool           -- Constante Booléenne.
          | Dvar VarIndex        -- Référence à une variable.
          | Dtest Dexp Dexp Dexp -- Expression conditionnelle.
          | Dfob Int Dexp        -- Construction de fobjet de N arguments.
          | Dsend Dexp [Dexp]    -- Appel de fobjet.
          | Dlet Dexp Dexp       -- Déclaration non récursive.
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Dfix [Dexp] Dexp
          deriving (Show, Eq)

-- Renvoie l'index de De Bruijn de la variable, i.e. sa position
-- dans le contexte.
lookupDI :: TEnv -> Var -> Int -> Int
lookupDI ((x1, _) : xs) x2 n =
    if x1 == x2 then n else lookupDI xs x2 (n + 1)
lookupDI _ x _ = error ("Variable inconnue: " ++ show x)

-- Conversion de Lexp en Dexp.
-- Les types contenus dans le "TEnv" ne sont en fait pas utilisés.
l2d :: TEnv -> Lexp -> Dexp
l2d _ (Lnum n) = Dnum n
l2d _ (Lbool b) = Dbool b
l2d tenv (Lvar v) = Dvar (lookupDI tenv v 0)
l2d tenv (Ltype e _) = l2d tenv e
l2d tenv (Ltest e1 e2 e3) =
    Dtest (l2d tenv e1) (l2d tenv e2) (l2d tenv e3)
l2d tenv (Lsend e0 es) =
    Dsend (l2d tenv e0) (map (l2d tenv) es)
l2d tenv (Lfob args e) =
    let tenv' = args ++ tenv
        argCount = length args
        de = l2d tenv' e
    in Dfob argCount de
l2d tenv (Llet x e1 e2) =
    let de1 = l2d tenv e1
        tenv' = (x, undefined) : tenv -- le type n'est pas nécessaire ici
        de2 = l2d tenv' e2
    in Dlet de1 de2
l2d tenv (Lfix xs e) =
    let tenv' = map (\(x, _) -> (x, undefined)) xs ++ tenv
        des = map (\(_, ei) -> l2d tenv' ei) xs
        de = l2d tenv' e
    in Dfix des de

---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

type VEnv = [Value]

eval :: VEnv -> Dexp -> Value
eval _   (Dnum n) = Vnum n
eval _   (Dbool b) = Vbool b
eval venv (Dvar n) = venv !! n
eval venv (Dtest e1 e2 e3) =
    case eval venv e1 of
        Vbool True  -> eval venv e2
        Vbool False -> eval venv e3
        _ -> error "La condition dans if n'est pas un booléen"
eval venv (Dsend e0 es) =
    let v0 = eval venv e0
        vs = map (eval venv) es
    in case v0 of
        Vbuiltin f -> f vs
        Vfob closure argCount body ->
            if length vs == argCount then
                let venv' = vs ++ closure
                in eval venv' body
            else error "Nombre incorrect d'arguments dans l'appel de fonction"
        _ -> error "Tentative d'appeler une valeur non fonction"
eval venv (Dfob argCount body) =
    Vfob venv argCount body
eval venv (Dlet e1 e2) =
    let v1 = eval venv e1
        venv' = v1 : venv
    in eval venv' e2
eval venv (Dfix des body) =
    let recVenv = map (\de -> eval recVenv de) des ++ venv
    in eval recVenv body

---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

tenv0 :: TEnv
tenv0 = map (\(x,t,_v) -> (x,t)) env0

venv0 :: VEnv
venv0 = map (\(_x,_t,v) -> v) env0

-- Évaluation sans vérification de types.
evalSexp :: Sexp -> Value
evalSexp = eval venv0 . l2d tenv0 . s2l

checkSexp :: Sexp -> Type
checkSexp = check True tenv0 . s2l

tevalSexp :: Sexp -> Either (Type, Value) String
tevalSexp se = let le = s2l se
               in case check True tenv0 le of
                    Terror err -> Right err
                    t -> Left (t, eval venv0 (l2d tenv0 le))

-- Lit un fichier contenant plusieurs Sexps, les évalue l'une après
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
            in map tevalSexp (sexps s))
       hClose inputHandle

sexpOf :: String -> Sexp
sexpOf = read

lexpOf :: String -> Lexp
lexpOf = s2l . sexpOf

valOf :: String -> Value
valOf = evalSexp . sexpOf

