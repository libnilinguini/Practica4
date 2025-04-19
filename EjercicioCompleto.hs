-- Definición de términos y fórmulas
data Term = Var String | Const String | Func String [Term]
  deriving (Show, Eq)

data Formula = Pred String [Term]
             | Not Formula
             | And Formula Formula
             | Or Formula Formula
             | Implies Formula Formula
             | Forall String Formula
             | Exists String Formula
             deriving (Show, Eq)

-- Interpretación: asignación de variables y predicados
type Interpretacion = (String -> Maybe Term, String -> [Term] -> Bool)

-- Evaluación de términos bajo una interpretación
evalTerm :: Interpretacion -> Term -> Term
evalTerm (varInt, _) (Var x) = case varInt x of
                                 Just t -> t
                                 Nothing -> Var x
evalTerm _ t = t

-- Evaluación de fórmulas bajo una interpretación
satisface :: Interpretacion -> Formula -> Bool
satisface int (Pred p terms) =
  let evalTerms = map (evalTerm int) terms
  in (snd int) p evalTerms
satisface int (Not f) = not (satisface int f)
satisface int (And f1 f2) = satisface int f1 && satisface int f2
satisface int (Or f1 f2) = satisface int f1 || satisface int f2
satisface int (Implies f1 f2) = not (satisface int f1) || satisface int f2
satisface int@(varInt, predInt) (Forall x f) =
  let posiblesValores = [Const "Alicia", Const "Bob"]
  in all (\v -> satisface ((\y -> if y == x then Just v else varInt y), predInt) f) posiblesValores
satisface int@(varInt, predInt) (Exists x f) =
  let posiblesValores = [Const "Alicia", Const "Bob"]
  in any (\v -> satisface ((\y -> if y == x then Just v else varInt y), predInt) f) posiblesValores

-- Reglas del sistema
regla1 :: Formula
regla1 = Implies (And (Pred "esUsuario" [Var "x"]) (Pred "tieneLlave" [Var "x"]))
                 (Pred "accesoPermitido" [Var "x"])

regla2 :: Formula
regla2 = Implies (Pred "esAdministrador" [Var "x"]) (Pred "tieneLlave" [Var "x"])

-- Base de conocimiento (reglas + hechos)
baseConocimiento :: [Formula]
baseConocimiento = [regla1, regla2, Pred "esAdministrador" [Const "Alicia"], Pred "esUsuario" [Const "Alicia"]]

-- Función para verificar consecuencia lógica (simplificada)
consecuenciaLogica :: [Formula] -> Formula -> [Interpretacion] -> Bool
consecuenciaLogica premisas conclusion interpretaciones =
  all (\interp -> all (satisface interp) premisas --> satisface interp conclusion) interpretaciones
  where
    (-->) a b = not a || b

-- Interpretación de ejemplo
interpEjemplo :: Interpretacion
interpEjemplo = (
  \x -> Just (Const x),  -- Asignación de variables
  \p args -> case (p, args) of
    ("esAdministrador", [Const "Alicia"]) -> True
    ("esUsuario", [Const "Alicia"]) -> True
    ("tieneLlave", [Const "Alicia"]) -> True  -- por regla2
    ("accesoPermitido", [Const "Alicia"]) -> True  -- objetivo
    _ -> False
  )

-- Verificar acceso
verificarAcceso :: String -> Bool
verificarAcceso nombre =
  let hecho = Pred "accesoPermitido" [Const nombre]
      interpretaciones = [interpEjemplo]
  in consecuenciaLogica baseConocimiento hecho interpretaciones

-- Main
main :: IO ()
main = do
  putStrLn "¿Alicia tiene acceso?"
  print (verificarAcceso "Alicia")  -- Debería imprimir True
